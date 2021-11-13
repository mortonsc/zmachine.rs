use super::*;

use nom;
use nom::bits::bits;
use nom::bits::complete::tag as tag_bits;
use nom::bits::complete::take as take_bits;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, not, verify};
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::preceded;
use nom::IResult;

// detail of nom implementation that we don't actually care about
type Bitstream<'a> = (&'a [u8], usize);

pub fn decode_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    // TODO: other types of instruction
    alt((zero_op_instr, one_op_instr))(input)
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

fn take_operand(input: &[u8], op_type: OperandType) -> IResult<&[u8], Option<Operand>> {
    match op_type {
        OperandType::LargeConst => map(be_u16, |n| Some(Operand::LargeConst(n)))(input),
        OperandType::SmallConst => map(be_u8, |n| Some(Operand::SmallConst(n)))(input),
        OperandType::Variable => map(be_u8, |v| Some(Operand::Variable(v)))(input),
        OperandType::Omitted => Ok((input, None)),
    }
}

fn long_branch_data_bits(input: Bitstream) -> IResult<Bitstream, BranchData> {
    // need to be explicit about types for this to compile
    let t: (Bitstream, u8) = take_bits(1usize)(input)?;
    let (input, invert_cond) = t;
    let invert_cond = if invert_cond == 0 { true } else { false };

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

fn one_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (input, (op_type, opcode)) = bits(one_op_bits)(input)?;
    let (input, operand) = take_operand(input, op_type)?;
    let operand = operand.unwrap();
    let (input, instr) = match opcode {
        // TODO
        _ => (input, Instr::IllegalOneOp),
    };
    Ok((input, instr))
}

fn zero_op_bits(input: Bitstream) -> IResult<Bitstream, u8> {
    // 10 = short instruction format
    // 11 = zero operands
    let (input, _) = tag_bits(0b1011, 4usize)(input)?;
    // 0xbe is first byte of extended instr
    verify(take_bits(4usize), |&op| op != 0xe)(input)
}

fn zero_op_instr(input: &[u8]) -> IResult<&[u8], Instr> {
    let (mut input, op) = bits(zero_op_bits)(input)?;
    let instr = match op {
        0x0 => Instr::RTrue,
        0x1 => Instr::RFalse,
        0x2 => Instr::Print {
            // TODO
            zstr: ZStr::from(b"" as &[u8]),
        },
        0x3 => Instr::PrintRet {
            // TODO
            zstr: ZStr::from(b"" as &[u8]),
        },
        0x4 => Instr::Nop,
        0x5 => Instr::IllegalZeroOp, // save pre-V5
        0x6 => Instr::IllegalZeroOp, // restore pre-V5
        0x7 => Instr::Restart,
        0x8 => Instr::RetPopped,
        0x9 => {
            let (new_input, dst) = be_u8(input)?;
            input = new_input;
            Instr::Catch { dst }
        }
        0xa => Instr::Quit,
        0xb => Instr::NewLine,
        0xc => Instr::IllegalZeroOp, // show_status pre-V4
        0xd => {
            let (new_input, bdata) = branch_data(input)?;
            input = new_input;
            Instr::Verify { bdata }
        }
        _ => panic!(), // parser should not emit any other value
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
