use super::opcode;
use super::*;

use nom::{
    bits::{
        bits,
        complete::{tag as tag_bits, take as take_bits},
    },
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{cut, flat_map, map, peek, success, verify},
    error::Error,
    multi::count,
    number::complete::{be_i16, be_i8, be_u8},
    sequence::{preceded, tuple},
    IResult,
};

// detail of nom implementation
type Bitstream<'a> = (&'a [u8], usize);

//TODO: propagate parse errors
pub fn decode_instr(input: &[u8]) -> Option<(Instr, usize)> {
    let (remaining, instr) = one_instr(input).ok()?;
    let len = input.len() - remaining.len();
    Some((instr, len))
}

fn one_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    alt((
        zero_op_instr,
        one_op_instr,
        two_op_instr,
        var_op_instr,
        extended_instr,
    ))(input)
}

fn operand_type_bits(input: Bitstream) -> IResult<Bitstream, OperandType> {
    map(take_bits(2usize), |s: u8| match s {
        0b00 => OperandType::LargeConst,
        0b01 => OperandType::SmallConst,
        0b10 => OperandType::Variable,
        0b11 => OperandType::Omitted,
        _ => panic!(),
    })(input)
}

// used in the "long" format
fn operand_type_single_bit(input: Bitstream) -> IResult<Bitstream, OperandType> {
    map(take_bits(1usize), |s: u8| match s {
        0b0 => OperandType::SmallConst,
        0b1 => OperandType::Variable,
        _ => panic!(),
    })(input)
}

fn operand<'a>(
    op_type: OperandType,
) -> impl FnMut(&'a [u8]) -> IResult<&[u8], Option<Operand>, Error<&[u8]>> {
    move |input: &'a [u8]| match op_type {
        // cut because this happens after we've decoded the instruction itself
        OperandType::LargeConst => cut(map(be_i16, |n| Some(Operand::LargeConst(n))))(input),
        OperandType::SmallConst => cut(map(be_i8, |n| Some(Operand::SmallConst(n))))(input),
        OperandType::Variable => cut(map(be_u8, |v| Some(Operand::Variable(v))))(input),
        OperandType::Omitted => Ok((input, None)),
    }
}

fn long_branch_offset(input: Bitstream) -> IResult<Bitstream, i16> {
    map(
        // "If bit 6 is clear, then the offset is a signed 14-bit number
        // "given in bits 0 to 5 of the first byte followed by all 8 of the second.
        preceded(tag_bits(0b0, 1usize), take_bits(14usize)),
        // sign extend because nom doesn't do that for us...
        |offset: i16| (offset << 2) >> 2,
    )(input)
}

fn short_branch_offset(input: Bitstream) -> IResult<Bitstream, i16> {
    map(
        // "If bit 6 is set, then the branch occupies 1 byte only,
        // "and the "offset" is in the range 0 to 63, given in the bottom 6 bits.
        preceded(tag_bits(0b1, 1usize), take_bits(6usize)),
        // not sure whether the offset is signed
        // if it is this should take offset as i8 instead of u8
        // and sign extend like we do with long branches
        |offset: u8| offset as i16,
    )(input)
}

fn branch_dst(input: Bitstream) -> IResult<Bitstream, BranchDst> {
    map(
        alt((short_branch_offset, long_branch_offset)),
        |offset| match offset {
            0 => BranchDst::Return(false),
            1 => BranchDst::Return(true),
            _ => BranchDst::Offset(offset),
        },
    )(input)
}

fn branch_invert_cond(input: Bitstream) -> IResult<Bitstream, bool> {
    // condition is inverted if the first bit is 0
    map(take_bits(1usize), |b: u8| b == 0)(input)
}

fn branch_data(input: &[u8]) -> IResult<&[u8], BranchData> {
    map(
        // cut because this happens after we've decoded the instruction itself
        bits(cut(tuple((branch_invert_cond, branch_dst)))),
        |(invert_cond, dst): (bool, BranchDst)| BranchData { invert_cond, dst },
    )(input)
}

fn dst_var(input: &[u8]) -> IResult<&[u8], u8> {
    // cut because this happens after we've decoded the instruction itself
    cut(be_u8)(input)
}

fn op_type_field_byte(input: &[u8]) -> IResult<&[u8], Vec<OperandType>> {
    bits(count(operand_type_bits, 4))(input)
}

fn op_type_field_bytes<'a>(
    n: usize,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Vec<OperandType>, Error<&[u8]>> {
    map(
        verify(
            map(
                count(op_type_field_byte, n),
                |op_types: Vec<Vec<OperandType>>| {
                    op_types.into_iter().flatten().collect::<Vec<_>>()
                },
            ),
            |op_types: &Vec<OperandType>| {
                op_types
                    .iter()
                    .skip_while(|&&op_type| op_type != OperandType::Omitted)
                    .all(|&op_type| op_type == OperandType::Omitted)
            },
        ),
        // only include the non-"Omitted" operand types
        |op_types: Vec<OperandType>| {
            op_types
                .into_iter()
                .take_while(|&op_type| op_type != OperandType::Omitted)
                .collect::<Vec<_>>()
        },
    )
}

fn zero_op(input: &[u8]) -> IResult<&[u8], u8> {
    bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(preceded(
        // 10 = short instruction format
        // 11 = zero operands
        tag_bits(0b1011, 4usize),
        // 0xbe is first byte of extended instr
        verify(take_bits(4usize), |&opcode| opcode != 0xe),
    ))(input)
}

fn one_op(input: &[u8]) -> IResult<&[u8], (OperandType, u8)> {
    bits(preceded(
        tag_bits(0b10, 2usize),
        tuple((
            verify(operand_type_bits, |&op_type| {
                op_type != OperandType::Omitted
            }),
            take_bits(4usize),
        )),
    ))(input)
}

fn long_two_op(input: &[u8]) -> IResult<&[u8], (OperandType, OperandType, u8)> {
    bits(preceded(
        tag_bits(0b0, 1usize),
        tuple((
            operand_type_single_bit,
            operand_type_single_bit,
            take_bits(5usize),
        )),
    ))(input)
}

fn var_two_op_opcode(input: &[u8]) -> IResult<&[u8], u8> {
    // nom needs this horrible type annotation to compile
    bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(preceded(
        tag_bits(0b110, 3usize),
        take_bits(5usize),
    ))(input)
}

fn var_two_op(input: &[u8]) -> IResult<&[u8], (OperandType, OperandType, u8)> {
    map(
        tuple((
            var_two_op_opcode,
            // TODO: JE can take more than two operands?
            cut(verify(
                op_type_field_bytes(1),
                |op_types: &Vec<OperandType>| op_types.len() == 2,
            )),
        )),
        |(opcode, op_types): (u8, Vec<OperandType>)| (op_types[0], op_types[1], opcode),
    )(input)
}

fn n_op_type_bytes(opcode: u8) -> usize {
    match opcode {
        opcode::var_op::CALL_VS2 | opcode::var_op::CALL_VN2 => 2,
        _ => 1,
    }
}

fn var_op_opcode(input: &[u8]) -> IResult<&[u8], u8> {
    // nom needs this horrible type annotation to compile
    bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(preceded(
        tag_bits(0b111, 3usize),
        take_bits(5usize),
    ))(input)
}

fn var_op(input: &[u8]) -> IResult<&[u8], (Vec<OperandType>, u8)> {
    map(
        tuple((
            peek(var_op_opcode),
            flat_map(map(var_op_opcode, n_op_type_bytes), op_type_field_bytes),
        )),
        |(opcode, op_types): (u8, Vec<OperandType>)| (op_types, opcode),
    )(input)
}

fn extended(input: &[u8]) -> IResult<&[u8], (Vec<OperandType>, u8)> {
    map(
        preceded(
            tag([0xbe]),
            cut(tuple((take(1usize), op_type_field_bytes(1)))),
        ),
        |(opcode, op_types): (&[u8], Vec<OperandType>)| (op_types, opcode[0]),
    )(input)
}

// silly way of generating errors
fn check<'a>(b: bool) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], (), Error<&[u8]>> {
    cut(verify(success(()), move |_| b))
}

fn zero_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (mut input, op) = zero_op(input)?;
    use opcode::zero_op::*;
    let instr = match op {
        0x10..=0xff => panic!("parser should emit a 4-bit opcode"),
        R_TRUE => Instr::RTrue,
        R_FALSE => Instr::RFalse,
        PRINT => Instr::Print,
        PRINT_RET => Instr::PrintRet,
        NOP => Instr::Nop,
        RESTART => Instr::Restart,
        RET_POPPED => Instr::RetPopped,
        CATCH => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Catch { dst }
        }
        QUIT => Instr::Quit,
        NEW_LINE => Instr::NewLine,
        VERIFY => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::Verify { bdata }
        }
        EXTENDED => panic!("this should be parsed as extended opcode"),
        PIRACY => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::Piracy { bdata }
        }
        _ => Instr::IllegalZeroOp,
    };
    Ok((input, instr))
}

fn one_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, (op_type, opcode)) = one_op(input)?;
    let (input, operand) = cut(operand(op_type))(input)?;
    let operand = operand.unwrap();
    let mut input = input;
    use opcode::one_op::*;
    let instr = match opcode {
        0x10..=0xff => panic!("parser should return a 4-bit opcode"),
        JZ => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::JZ { a: operand, bdata }
        }
        GET_SIBLING => {
            let (new_input, bdata) = branch_data(input)?;
            let (new_input, dst) = dst_var(new_input)?;
            input = new_input;
            Instr::GetSibling {
                obj_id: operand,
                dst,
                bdata,
            }
        }
        GET_CHILD => {
            let (new_input, bdata) = branch_data(input)?;
            let (new_input, dst) = dst_var(new_input)?;
            input = new_input;
            Instr::GetChild {
                obj_id: operand,
                dst,
                bdata,
            }
        }
        GET_PARENT => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::GetParent {
                obj_id: operand,
                dst,
            }
        }
        GET_PROP_LEN => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::GetPropLen {
                prop_addr: operand,
                dst,
            }
        }
        INC => Instr::Inc {
            var_by_ref: operand,
        },
        DEC => Instr::Dec {
            var_by_ref: operand,
        },
        PRINT_ADDR => Instr::PrintAddr {
            zstr_byteaddr: operand,
        },
        CALL_1S => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Call1S {
                routine_paddr: operand,
                dst,
            }
        }
        REMOVE_OBJ => Instr::RemoveObj { obj_id: operand },
        PRINT_OBJ => Instr::PrintObj { obj_id: operand },
        RET => Instr::Ret { val: operand },
        // TODO: not sure if offset is actually a normal operand
        JUMP => Instr::Jump { offset: operand },
        PRINT_PADDR => Instr::PrintPAddr {
            zstr_paddr: operand,
        },
        LOAD => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Load {
                var_by_ref: operand,
                dst,
            }
        }
        CALL_1N => Instr::Call1N {
            routine_paddr: operand,
        },
    };
    Ok((input, instr))
}

fn two_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, (op_type1, op_type2, opcode)) = alt((long_two_op, var_two_op))(input)?;
    let (input, op1) = cut(operand(op_type1))(input)?;
    let (input, op2) = cut(operand(op_type2))(input)?;
    let op1 = op1.unwrap();
    let op2 = op2.unwrap();
    let mut input = input;
    use opcode::two_op::*;
    let instr = match opcode {
        0x20..=0xff => panic!("parser should return a 5-bit opcode"),
        // TODO: je has defined semantics for more than two ops in the standard
        // but the standard also says a two-op instruction can only have exactly two operands...
        // right now the semantics for multiple operands are implemented but the decoder only
        // accepts exactly two
        JE => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::JE {
                a: op1,
                others: vec![op2],
                bdata,
            }
        }
        JL => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::JL {
                a: op1,
                b: op2,
                bdata,
            }
        }
        JG => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::JG {
                a: op1,
                b: op2,
                bdata,
            }
        }
        DEC_CHK => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::DecChk {
                var_by_ref: op1,
                cmp_val: op2,
                bdata,
            }
        }
        INC_CHK => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::IncChk {
                var_by_ref: op1,
                cmp_val: op2,
                bdata,
            }
        }
        JIN => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::JIn {
                obj1: op1,
                obj2: op2,
                bdata,
            }
        }
        TEST => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::Test {
                bitmap: op1,
                flags: op2,
                bdata,
            }
        }
        OR => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Or {
                a: op1,
                b: op2,
                dst,
            }
        }
        AND => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::And {
                a: op1,
                b: op2,
                dst,
            }
        }
        TEST_ATTR => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::TestAttr {
                obj_id: op1,
                attr: op2,
                bdata,
            }
        }
        SET_ATTR => Instr::SetAttr {
            obj_id: op1,
            attr: op2,
        },
        CLEAR_ATTR => Instr::ClearAttr {
            obj_id: op1,
            attr: op2,
        },
        STORE => Instr::Store {
            var_by_ref: op1,
            val: op2,
        },
        INSERT_OBJ => Instr::InsertObj {
            obj_id: op1,
            dst_obj: op2,
        },
        LOADW => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::LoadW {
                array: op1,
                word_index: op2,
                dst,
            }
        }
        LOADB => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::LoadB {
                array: op1,
                byte_index: op2,
                dst,
            }
        }
        GET_PROP => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::GetProp {
                obj_id: op1,
                prop: op2,
                dst,
            }
        }
        GET_PROP_ADDR => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::GetPropAddr {
                obj_id: op1,
                prop: op2,
                dst,
            }
        }
        GET_NEXT_PROP => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::GetNextProp {
                obj_id: op1,
                prop: op2,
                dst,
            }
        }
        ADD => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Add {
                a: op1,
                b: op2,
                dst,
            }
        }
        SUB => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Sub {
                a: op1,
                b: op2,
                dst,
            }
        }
        MUL => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Mul {
                a: op1,
                b: op2,
                dst,
            }
        }
        DIV => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Div {
                a: op1,
                b: op2,
                dst,
            }
        }
        MOD => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Mod {
                a: op1,
                b: op2,
                dst,
            }
        }
        CALL_2S => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Call2S {
                routine_paddr: op1,
                arg1: op2,
                dst,
            }
        }
        CALL_2N => Instr::Call2N {
            routine_paddr: op1,
            arg1: op2,
        },
        SET_COLOR => Instr::SetColor { fg: op1, bg: op1 },
        opcode => Instr::IllegalTwoOp { opcode },
    };
    Ok((input, instr))
}

fn var_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (mut input, (op_types, opcode)) = var_op(input)?;
    let mut operands: Vec<Operand> = Vec::new();
    for op_type in op_types {
        let (new_input, op) = cut(operand(op_type))(input)?;
        operands.push(op.unwrap());
        input = new_input;
    }
    let n_ops = operands.len();
    use opcode::var_op::*;
    let instr = match opcode {
        0x20..=0xff => panic!("parser should return a 5-bit opcode"),
        CALL_VS | CALL_VS2 => {
            check((1..=8).contains(&n_ops))(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            let routine_paddr = operands.remove(0);
            Instr::CallVS {
                routine_paddr,
                args: operands,
                dst,
            }
        }
        STOREW => {
            check(n_ops == 3)(input)?;
            Instr::StoreW {
                array: operands[0],
                word_index: operands[1],
                val: operands[2],
            }
        }
        STOREB => {
            check(n_ops == 3)(input)?;
            Instr::StoreB {
                array: operands[0],
                byte_index: operands[1],
                val: operands[2],
            }
        }
        PUT_PROP => {
            check(n_ops == 3)(input)?;
            Instr::PutProp {
                obj_id: operands[0],
                prop: operands[1],
                val: operands[2],
            }
        }
        AREAD => {
            check(n_ops == 2 || n_ops == 4)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::ARead {
                text: operands[0],
                parse: operands[1],
                time: operands.get(2).copied(),
                routine_paddr: operands.get(3).copied(),
                dst,
            }
        }
        PRINT_CHAR => {
            check(n_ops == 1)(input)?;
            Instr::PrintChar {
                zscii_code: operands[0],
            }
        }
        PRINT_NUM => {
            check(n_ops == 1)(input)?;
            Instr::PrintNum { val: operands[0] }
        }
        RANDOM => {
            check(n_ops == 1)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Random {
                range: operands[0],
                dst,
            }
        }
        PUSH => {
            check(n_ops == 1)(input)?;
            Instr::Push { val: operands[0] }
        }
        PULL => {
            check(n_ops == 1)(input)?;
            Instr::Pull {
                var_by_ref: operands[0],
            }
        }
        SPLIT_WINDOW => {
            check(n_ops == 1)(input)?;
            Instr::SplitWindow { lines: operands[0] }
        }
        SET_WINDOW => {
            check(n_ops == 1)(input)?;
            Instr::SetWindow {
                window: operands[0],
            }
        }
        ERASE_WINDOW => {
            check(n_ops == 1)(input)?;
            Instr::EraseWindow {
                window: operands[0],
            }
        }
        ERASE_LINE => {
            check(n_ops == 1)(input)?;
            Instr::EraseLine { val: operands[0] }
        }
        SET_CURSOR => {
            check(n_ops == 2)(input)?;
            Instr::SetCursor {
                line: operands[0],
                column: operands[1],
            }
        }
        GET_CURSOR => {
            check(n_ops == 1)(input)?;
            Instr::GetCursor { array: operands[0] }
        }
        SET_TEXT_STYLE => {
            check(n_ops == 1)(input)?;
            Instr::SetTextStyle { style: operands[0] }
        }
        BUFFER_MODE => {
            check(n_ops == 1)(input)?;
            Instr::BufferMode { flag: operands[0] }
        }
        OUTPUT_STREAM => {
            check(n_ops == 2)(input)?;
            Instr::OutputStream {
                number: operands[0],
                table: operands[1],
            }
        }
        INPUT_STREAM => {
            check(n_ops == 1)(input)?;
            Instr::InputStream {
                number: operands[0],
            }
        }
        SOUND_EFFECT => Instr::SoundEffect {
            number: operands.get(0).copied(),
            effect: operands.get(1).copied(),
            volume: operands.get(2).copied(),
            routine: operands.get(3).copied(),
        },
        READ_CHAR => {
            check(n_ops == 1 || n_ops == 3)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::ReadChar {
                always_one: operands[0],
                time: operands.get(1).copied(),
                routine: operands.get(2).copied(),
                dst,
            }
        }
        SCAN_TABLE => {
            check(n_ops == 3 || n_ops == 4)(input)?;
            let (new_input, dst) = dst_var(input)?;
            let (new_input, bdata) = branch_data(new_input)?;
            input = new_input;
            Instr::ScanTable {
                x: operands[0],
                table: operands[1],
                len: operands[2],
                form: operands.get(3).copied(),
                dst,
                bdata,
            }
        }
        NOT => {
            check(n_ops == 1)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Not {
                val: operands[0],
                dst,
            }
        }
        CALL_VN | CALL_VN2 => {
            check((1..=8).contains(&n_ops))(input)?;
            let routine_paddr = operands.remove(0);
            Instr::CallVN {
                routine_paddr,
                args: operands,
            }
        }
        TOKENIZE => {
            check(n_ops == 4)(input)?;
            Instr::Tokenize {
                text: operands[0],
                parse: operands[1],
                dictionary: operands[2],
                flag: operands[3],
            }
        }
        ENCODE_TEXT => {
            check(n_ops == 4)(input)?;
            Instr::EncodeText {
                zscii_text: operands[0],
                length: operands[1],
                from: operands[2],
                coded_text: operands[3],
            }
        }
        COPY_TABLE => {
            check(n_ops == 3)(input)?;
            Instr::CopyTable {
                first: operands[0],
                second: operands[1],
                size: operands[2],
            }
        }
        PRINT_TABLE => {
            check(n_ops >= 2)(input)?;
            Instr::PrintTable {
                zscii_text: operands[0],
                width: operands[1],
                height: operands.get(2).copied(),
                skip: operands.get(3).copied(),
            }
        }
        CHECK_ARG_COUNT => {
            check(n_ops == 1)(input)?;
            Instr::CheckArgCount {
                arg_num: operands[0],
            }
        }
    };
    Ok((input, instr))
}

fn extended_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (mut input, (op_types, opcode)) = extended(input)?;
    let mut operands: Vec<Operand> = Vec::new();
    for op_type in op_types {
        let (new_input, op) = cut(operand(op_type))(input)?;
        operands.push(op.unwrap());
        input = new_input;
    }
    let n_ops = operands.len();
    use opcode::extended::*;
    let instr = match opcode {
        SAVE => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Save {
                table: operands.get(0).copied(),
                bytes: operands.get(1).copied(),
                name: operands.get(2).copied(),
                prompt: operands.get(3).copied(),
                dst,
            }
        }
        RESTORE => {
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::Restore {
                table: operands.get(0).copied(),
                bytes: operands.get(1).copied(),
                name: operands.get(2).copied(),
                prompt: operands.get(3).copied(),
                dst,
            }
        }
        LOG_SHIFT => {
            check(n_ops == 2)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::LogShift {
                number: operands[0],
                places: operands[1],
                dst,
            }
        }
        ART_SHIFT => {
            check(n_ops == 2)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::ArtShift {
                number: operands[0],
                places: operands[1],
                dst,
            }
        }
        SET_FONT => {
            check(n_ops == 1)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::SetFont {
                font: operands[0],
                dst,
            }
        }
        SAVE_UNDO => {
            check(n_ops == 0)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::SaveUndo { dst }
        }
        RESTORE_UNDO => {
            check(n_ops == 0)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::RestoreUndo { dst }
        }
        PRINT_UNICODE => {
            check(n_ops == 1)(input)?;
            Instr::PrintUnicode {
                char_num: operands[0],
            }
        }
        CHECK_UNICODE => {
            check(n_ops == 1)(input)?;
            let (new_input, dst) = dst_var(input)?;
            input = new_input;
            Instr::CheckUnicode {
                char_num: operands[0],
                dst,
            }
        }
        SET_TRUE_COLOR => {
            check(n_ops == 2)(input)?;
            Instr::SetTrueColor {
                fg: operands[0],
                bg: operands[1],
            }
        }
        opcode => Instr::IllegalExtended(opcode),
    };
    Ok((input, instr))
}

pub fn test() {
    let print: [u8; 1] = [0xbe];
    let (_, print_instr) = zero_op_instr(&print).unwrap();
    println!("{:?}", print_instr);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_op() {
        let print: [u8; 1] = [0b10110010];
        let (_, print_instr) = zero_op_instr(&print).unwrap();
        match print_instr {
            Instr::Print { .. } => (),
            _ => panic!(),
        }
    }

    #[test]
    #[should_panic]
    fn test_extended_isnt_zero_op() {
        let extended: [u8; 1] = [0xbe];
        let (_, _) = zero_op_instr(&extended).unwrap();
    }

    #[test]
    fn test_long_branch() {
        let bytes = &[0x9f, 0xff];
        let (_, bdata) = branch_data(bytes).expect("failed to parse");
        assert_eq!(bdata.invert_cond, false);
        match bdata.dst {
            BranchDst::Offset(0x1fff) => (),
            BranchDst::Offset(x) => panic!("wrong offset: {:#x}", x),
            _ => panic!(),
        }
    }

    #[test]
    fn test_long_branch_signed() {
        // test that the 14-bit offset is sign-extended into an i16
        let bytes = &[0xbf, 0xff];
        let (_, bdata) = branch_data(bytes).expect("failed to parse");
        assert_eq!(bdata.invert_cond, false);
        match bdata.dst {
            BranchDst::Offset(-1) => (),
            BranchDst::Offset(x) => panic!("wrong offset: {:#x}", x),
            _ => panic!(),
        }
    }
}
