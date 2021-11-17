use super::opcode;
use super::*;

use nom;
use nom::bits::bits;
use nom::bits::complete::tag as tag_bits;
use nom::bits::complete::take as take_bits;
use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, not, verify};
use nom::multi::count;
use nom::number::complete::{be_i16, be_i8, be_u16, be_u8};
use nom::sequence::preceded;
use nom::IResult;

// detail of nom implementation that we don't actually care about
type Bitstream<'a> = (&'a [u8], usize);

pub fn decode_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    alt((
        zero_op_instr,
        one_op_instr,
        two_op_instr,
        var_op_instr,
        extended_instr,
    ))(input)
}

fn operand_type_bits(input: Bitstream) -> IResult<Bitstream, OperandType> {
    let (input, spec) = take_bits(2usize)(input)?;
    let op_type = match spec {
        0b00 => OperandType::LargeConst,
        0b01 => OperandType::SmallConst,
        0b10 => OperandType::Variable,
        0b11 => OperandType::Omitted,
        _ => panic!(),
    };
    Ok((input, op_type))
}

// used in the "long" format
fn operand_type_single_bit(input: Bitstream) -> IResult<Bitstream, OperandType> {
    let (input, spec) = take_bits(1usize)(input)?;
    let op_type = match spec {
        0b0 => OperandType::SmallConst,
        0b1 => OperandType::Variable,
        _ => panic!(),
    };
    Ok((input, op_type))
}

fn take_operand(input: &[u8], op_type: OperandType) -> IResult<&[u8], Option<Operand>> {
    match op_type {
        OperandType::LargeConst => map(be_i16, |n| Some(Operand::LargeConst(n)))(input),
        OperandType::SmallConst => map(be_i8, |n| Some(Operand::SmallConst(n)))(input),
        OperandType::Variable => map(be_u8, |v| Some(Operand::Variable(v)))(input),
        OperandType::Omitted => Ok((input, None)),
    }
}

fn long_branch_data_bits(input: Bitstream) -> IResult<Bitstream, BranchData> {
    // need to be explicit about types for this to compile
    let t: (Bitstream, u8) = take_bits(1usize)(input)?;
    let (input, invert_cond) = t;
    let invert_cond = invert_cond == 0;

    // "If bit 6 is clear, then the offset is a signed 14-bit number
    // "given in bits 0 to 5 of the first byte followed by all 8 of the second.
    let (input, _) = tag_bits(0b0, 1usize)(input)?;
    let t: (Bitstream, i16) = take_bits(14usize)(input)?;
    let (input, offset) = t;
    // sign extend because nom doesn't do that for us...
    let offset = (offset << 2) >> 2;
    let dst = match offset {
        0 => BranchDst::Return(false),
        1 => BranchDst::Return(true),
        _ => BranchDst::Offset(offset),
    };
    Ok((input, BranchData { invert_cond, dst }))
}

fn short_branch_data_bits(input: Bitstream) -> IResult<Bitstream, BranchData> {
    // need to be explicit about types for this to compile
    let t: (Bitstream, u8) = take_bits(1usize)(input)?;
    let (input, invert_cond) = t;
    let invert_cond = if invert_cond == 0 { true } else { false };

    // "If bit 6 is set, then the branch occupies 1 byte only,
    // "and the "offset" is in the range 0 to 63, given in the bottom 6 bits.
    let (input, _) = tag_bits(0b1, 1usize)(input)?;
    // spec implies offset is unsigned
    // that might be wrong in which case we'd have to sign extend the offset
    let (input, offset) = take_bits(6usize)(input)? as (Bitstream, i16);
    let dst = match offset {
        0 => BranchDst::Return(false),
        1 => BranchDst::Return(true),
        _ => BranchDst::Offset(offset),
    };
    Ok((input, BranchData { invert_cond, dst }))
}

fn branch_data(input: &[u8]) -> IResult<&[u8], BranchData> {
    bits(alt((short_branch_data_bits, long_branch_data_bits)))(input)
}

fn op_type_field(input: Bitstream, n_bytes: usize) -> IResult<Bitstream, Vec<OperandType>> {
    let (input, mut op_types) = count(operand_type_bits, 4 * n_bytes)(input)?;
    let n_ops = op_types
        .iter()
        .take_while(|&&op_type| op_type != OperandType::Omitted)
        .count();
    if op_types[n_ops..]
        .iter()
        .any(|&op_type| op_type != OperandType::Omitted)
    {
        // TODO: do this through nom's error handling
        panic!("Invalidly formed operand specifier");
    }
    op_types.truncate(n_ops);
    Ok((input, op_types))
}

fn zero_op_bits(input: Bitstream) -> IResult<Bitstream, u8> {
    // 10 = short instruction format
    // 11 = zero operands
    let (input, _) = tag_bits(0b1011, 4usize)(input)?;
    // 0xbe is first byte of extended instr
    verify(take_bits(4usize), |&op| op != 0xe)(input)
}

fn one_op_bits(input: Bitstream) -> IResult<Bitstream, (OperandType, u8)> {
    // short instruction format
    let (input, _) = tag_bits(0b10, 2usize)(input)?;
    // if the op_type is omitted this is actually a zero_op instr
    let (input, op_type) = verify(operand_type_bits, |&op_type| {
        op_type != OperandType::Omitted
    })(input)?;
    let (input, opcode) = take_bits(4usize)(input)?;
    Ok((input, (op_type, opcode)))
}

fn long_two_op_bits(input: Bitstream) -> IResult<Bitstream, (OperandType, OperandType, u8)> {
    // long format marked by 0 as first bit
    let (input, _) = tag_bits(0b0, 1usize)(input)?;
    let (input, op1) = operand_type_single_bit(input)?;
    let (input, op2) = operand_type_single_bit(input)?;
    let (input, opcode) = take_bits(5usize)(input)?;
    Ok((input, (op1, op2, opcode)))
}

fn var_two_op_bits(input: Bitstream) -> IResult<Bitstream, (OperandType, OperandType, u8)> {
    // 11 means var format, 0 means two op instruction
    let (input, _) = tag_bits(0b110, 3usize)(input)?;
    let (input, opcode) = take_bits(5usize)(input)?;
    let (input, op_types) = op_type_field(input, 1)?;
    if op_types.len() != 2 {
        // TODO: do through nom's error handling
        // TODO: JE can take more than two operands?
        panic!("two op instruction needs exactly two operands");
    }
    Ok((input, (op_types[0], op_types[1], opcode)))
}

fn var_op_bits(input: Bitstream) -> IResult<Bitstream, (Vec<OperandType>, u8)> {
    // 11 means var format, 1 means var op instruction
    let (input, _) = tag_bits(0b111, 3usize)(input)?;
    let (input, opcode) = take_bits(5usize)(input)?;
    // these two instructions (and only them) take up to 8 operands
    // and so have two bytes instead of one specifying the types of those operands
    let n_bytes_ops = if matches!(opcode, opcode::var_op::CALL_VS2 | opcode::var_op::CALL_VN2) {
        2
    } else {
        1
    };
    let (input, op_types) = op_type_field(input, n_bytes_ops)?;
    Ok((input, (op_types, opcode)))
}

fn extended(input: &[u8]) -> IResult<&[u8], (Vec<OperandType>, u8)> {
    let (input, _) = tag([0xbe])(input)?;
    let (input, opcode) = take(1usize)(input)?;
    let opcode = opcode[0];
    let (input, op_types) = bits(|input| op_type_field(input, 1))(input)?;
    Ok((input, (op_types, opcode)))
}

fn zero_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (mut input, op) = bits(zero_op_bits)(input)?;
    use opcode::zero_op::*;
    let instr = match op {
        0x10..=0xff => panic!("parser should emit a 4-bit opcode"),
        R_TRUE => Instr::RTrue,
        R_FALSE => Instr::RFalse,
        PRINT => Instr::Print {
            // TODO
            ztext: Vec::new(),
        },
        PRINT_RET => Instr::PrintRet {
            // TODO
            ztext: Vec::new(),
        },
        NOP => Instr::Nop,
        RESTART => Instr::Restart,
        RET_POPPED => Instr::RetPopped,
        CATCH => {
            let (new_input, dst) = be_u8(input)?;
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
    let (input, (op_type, opcode)) = bits(one_op_bits)(input)?;
    let (input, operand) = take_operand(input, op_type)?;
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
            let (new_input, dst) = be_u8(new_input)?;
            input = new_input;
            Instr::GetSibling {
                obj_id: operand,
                dst,
                bdata,
            }
        }
        GET_CHILD => {
            let (new_input, bdata) = branch_data(input)?;
            let (new_input, dst) = be_u8(new_input)?;
            input = new_input;
            Instr::GetChild {
                obj_id: operand,
                dst,
                bdata,
            }
        }
        GET_PARENT => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::GetParent {
                obj_id: operand,
                dst,
            }
        }
        GET_PROP_LEN => {
            let (new_input, dst) = be_u8(input)?;
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
            let (new_input, dst) = be_u8(input)?;
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
            let (new_input, dst) = be_u8(input)?;
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
    // TODO: also account for var op format
    let (input, (op_type1, op_type2, opcode)) =
        bits(alt((long_two_op_bits, var_two_op_bits)))(input)?;
    let (input, op1) = take_operand(input, op_type1)?;
    let (input, op2) = take_operand(input, op_type2)?;
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
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Or {
                a: op1,
                b: op2,
                dst,
            }
        }
        AND => {
            let (new_input, dst) = be_u8(input)?;
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
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::LoadW {
                array: op1,
                word_index: op2,
                dst,
            }
        }
        LOADB => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::LoadB {
                array: op1,
                byte_index: op2,
                dst,
            }
        }
        GET_PROP => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::GetProp {
                obj_id: op1,
                prop: op2,
                dst,
            }
        }
        GET_PROP_ADDR => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::GetPropAddr {
                obj_id: op1,
                prop: op2,
                dst,
            }
        }
        GET_NEXT_PROP => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::GetNextProp {
                obj_id: op1,
                prop: op2,
                dst,
            }
        }
        ADD => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Add {
                a: op1,
                b: op2,
                dst,
            }
        }
        SUB => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Sub {
                a: op1,
                b: op2,
                dst,
            }
        }
        MUL => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Mul {
                a: op1,
                b: op2,
                dst,
            }
        }
        DIV => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Div {
                a: op1,
                b: op2,
                dst,
            }
        }
        MOD => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Mod {
                a: op1,
                b: op2,
                dst,
            }
        }
        CALL_2S => {
            let (new_input, dst) = be_u8(input)?;
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
    let (mut input, (op_types, opcode)) = bits(var_op_bits)(input)?;
    let mut operands: Vec<Operand> = Vec::new();
    for op_type in op_types {
        let (new_input, op) = take_operand(input, op_type)?;
        operands.push(op.unwrap());
        input = new_input;
    }
    let instr = match opcode {
        0x00..=0x1f => unimplemented!(),
        0x20..=0xff => panic!("parser should return a 5-bit opcode"),
    };
    Ok((input, instr))
}

fn extended_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (mut input, (op_types, opcode)) = extended(input)?;
    let mut operands: Vec<Operand> = Vec::new();
    for op_type in op_types {
        let (new_input, op) = take_operand(input, op_type)?;
        operands.push(op.unwrap());
        input = new_input;
    }
    let instr = match opcode {
        0x00..=0xff => unimplemented!(),
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
        let (_, instr) = zero_op_instr(&extended).unwrap();
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
