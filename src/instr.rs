use nom;
use nom::bits::bits;
use nom::bits::complete::tag as tag_bits;
use nom::bits::complete::take as take_bits;
use nom::bytes::complete::tag;
use nom::combinator::{map, not, verify};
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::preceded;
use nom::IResult;

use super::text::ZStr;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    LargeConst(u16),
    SmallConst(u8),
    Variable(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperandType {
    LargeConst,
    SmallConst,
    Variable,
    Omitted,
}

#[derive(Debug)]
pub struct BranchData {
    invert_cond: bool,
    dst: BranchDst,
}

#[derive(Debug)]
pub enum BranchDst {
    Offset(i16),
    Return(bool),
}

// detail of nom implementation that we don't actually care about
type Bitstream<'a> = (&'a [u8], usize);

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
    let (input, op) = bits(zero_op_bits)(input)?;
    let instr = match op {
        0x0 => Instr::RTrue,
        0x1 => Instr::RFalse,
        0x2 => Instr::Print,
        0x3 => Instr::PrintRet,
        0x4 => Instr::Nop,
        0x5 => Instr::IllegalZeroOp, // save pre-V5
        0x6 => Instr::IllegalZeroOp, // restore pre-V5
        0x7 => Instr::Restart,
        0x8 => Instr::RetPopped,
        0x9 => Instr::Catch,
        0xa => Instr::Quit,
        0xb => Instr::NewLine,
        0xc => Instr::IllegalZeroOp, // show_status pre-V4
        0xd => Instr::Verify,
        _ => panic!(), // parser should not emit any other value
    };
    Ok((input, instr))
}

#[derive(Debug)]
pub enum Instr<'a> {
    // zero op
    RTrue,
    RFalse,
    Print {
        zstr: ZStr<'a>,
    },
    PrintRet {
        zstr: ZStr<'a>,
    },
    Nop,
    Save,
    Restore,
    Restart,
    RetPopped,
    Catch {
        dst: u8,
    },
    Quit,
    NewLine,
    ShowStatus,
    Verify {
        bdata: BranchData,
    },
    Piracy {
        bdata: BranchData,
    },
    IllegalZeroOp,
    IllegalOneOp,

    // one op
    JZ {
        a: Operand,
        bdata: BranchData,
    },
    GetSibling {
        obj_id: Operand,
        dst: u8,
        bdata: BranchData,
    },
    GetChild {
        obj_id: Operand,
        dst: u8,
        bdata: BranchData,
    },
    GetParent {
        obj_id: Operand,
        dst: u8,
        bdata: BranchData,
    },
    GetPropLen {
        prop_addr: Operand,
        dst: u8,
    },
    Inc {
        var_by_ref: Operand,
    },
    Dec {
        var_by_ref: Operand,
    },
    PrintAddr {
        zstr_byteaddr: Operand,
    },
    Call1S {
        routine_paddr: Operand,
        dst: u8,
    },
    RemoveObj {
        obj_id: Operand,
    },
    PrintObj {
        obj_id: Operand,
    },
    Ret {
        val: Operand,
    },
    Jump {
        offset: Operand,
    },
    PrintPAddr {
        zstr_paddr: Operand,
    },
    Load {
        var_by_ref: Operand,
        dst: u8,
    },
    Call1N {
        routine_paddr: Operand,
    },
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
            Instr::Print => (),
            _ => panic!(),
        }
    }

    #[test]
    #[should_panic]
    fn test_extended_isnt_zero_op() {
        let extended: [u8; 1] = [0xbe];
        let (_, instr) = zero_op_instr(&extended).unwrap();
    }
}
