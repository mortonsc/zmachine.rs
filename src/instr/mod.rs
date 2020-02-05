extern crate num;

use num::FromPrimitive;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    LargeConst(u16),
    SmallConst(u8),
    Variable(u8),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum OperandType {
    LargeConst,
    SmallConst,
    Variable,
    Omitted,
}

impl OperandType {
    fn is_not_omitted(self) -> bool {
        match self {
            OperandType::Omitted => false,
            _ => true,
        }
    }

    fn from_last_two_bits(bits: u8) -> Self {
        match bits & 0b11 {
            0b00 => OperandType::LargeConst,
            0b01 => OperandType::SmallConst,
            0b10 => OperandType::Variable,
            _ => OperandType::Omitted,
        }
    }

    fn from_last_bit(bit: u8) -> Self {
        match bit & 0b1 {
            0b0 => OperandType::SmallConst,
            _ => OperandType::Variable,
        }
    }

}

enum InstrType {
    LongTwoOp,
    ShortZeroOp,
    ShortOneOp,
    VariableTwoOp,
    VariableVarOp,
    VariableDoubleVarOp,
    Ext,
}

pub enum Instr {
    ZeroOp {opcode: ZeroOpCode},
    OneOp {opcode: OneOpCode, op: Operand},
    TwoOp {opcode: TwoOpCode, op1: Operand, op2: Operand},
    VarOp {opcode: VarOpCode, ops: Vec<Operand>},
    Ext {opcode: ExtOpCode, ops: Vec<Operand>},
}


impl Operand {
    fn decode_one<'a, I>(program: &mut I, op_type: OperandType) -> Option<Self>
        where I: Iterator<Item=&'a u8>
    {
        match op_type {
            OperandType::Omitted => None,
            OperandType::SmallConst => Some(Operand::SmallConst(*program.next()?)),
            OperandType::Variable => Some(Operand::Variable(*program.next()?)),
            OperandType::LargeConst => Some(Operand::LargeConst(u16::from_be_bytes([
                *program.next()?,
                *program.next()?,
            ]))),
        }
    }

    fn decode_to_vec<'a, I>(program: &mut I, n_spec_bytes: usize) -> Option<Vec<Self>>
        where I: Iterator<Item=&'a u8>
    {
        let mut op_types: Vec<OperandType> = Vec::new();
        for byte in program.take(n_spec_bytes) {
            op_types.push(OperandType::from_last_two_bits(*byte >> 6));
            op_types.push(OperandType::from_last_two_bits(*byte >> 4));
            op_types.push(OperandType::from_last_two_bits(*byte >> 2));
            op_types.push(OperandType::from_last_two_bits(*byte));
        };

        let mut result: Vec<Operand> = Vec::new();
        for op_type in op_types.iter().take_while(|t| t.is_not_omitted()) {
            result.push(Operand::decode_one(program, *op_type)?);
        };
        Some(result)
    }

}



impl Instr {
    pub fn decode_one<'a, I: Iterator<Item=&'a u8>>(program: &mut I) -> Option<Self> {
        let opcode1 = *program.next()?;
        let instr_type = match opcode1 {
            0xbe => InstrType::Ext,
            0x00..=0x7f => InstrType::LongTwoOp,
            0x80..=0xaf => InstrType::ShortOneOp,
            0xb0..=0xbf => InstrType::ShortZeroOp,
            0xc0..=0xdf => InstrType::VariableTwoOp,
            0xe4 | 0xfa => InstrType::VariableDoubleVarOp, //call_vs2 and call_vn2
            0xe0..=0xff => InstrType::VariableVarOp,
        };

        let opcode_byte = match instr_type {
            InstrType::ShortZeroOp
            | InstrType::ShortOneOp => opcode1 & 0x0f,
            InstrType::LongTwoOp
            | InstrType::VariableTwoOp
            | InstrType::VariableDoubleVarOp
            | InstrType::VariableVarOp => opcode1 & 0x1f,
            InstrType::Ext => *program.next()?,
        };

        let instr = match instr_type {
            InstrType::ShortZeroOp => Instr::ZeroOp {
                opcode: ZeroOpCode::from_u8(opcode_byte)?
            },
            InstrType::ShortOneOp => Instr::OneOp {
                opcode: OneOpCode::from_u8(opcode_byte)?,
                op: {
                    let op_type = OperandType::from_last_two_bits(opcode1 >> 4);
                    Operand::decode_one(program, op_type)?
                }
            },
            InstrType::LongTwoOp => Instr::TwoOp {
                opcode: TwoOpCode::from_u8(opcode_byte)?,
                op1: {
                    let op_type = OperandType::from_last_bit(opcode1 >> 6);
                    Operand::decode_one(program, op_type)?
                },
                op2: {
                    let op_type = OperandType::from_last_bit(opcode1 >> 5);
                    Operand::decode_one(program, op_type)?
                },
            },
            InstrType::VariableTwoOp => {
                let operands = Operand::decode_to_vec(program, 1)?;
                // TODO: log a warning if ops 3, 4 aren't omitted
                Instr::TwoOp {
                    opcode: TwoOpCode::from_u8(opcode_byte)?,
                    op1: *operands.get(0)?,
                    op2: *operands.get(1)?,
                }
            }
            InstrType::VariableVarOp => Instr::VarOp {
                opcode: VarOpCode::from_u8(opcode_byte)?,
                ops: Operand::decode_to_vec(program, 1)?,
            },
            InstrType::VariableDoubleVarOp => Instr::VarOp {
                opcode: VarOpCode::from_u8(opcode_byte)?,
                ops: Operand::decode_to_vec(program, 2)?,
            },
            InstrType::Ext => Instr::Ext {
                opcode: ExtOpCode::from_u8(opcode_byte)?,
                ops: Operand::decode_to_vec(program, 1)?,
            },
        };
        Some(instr)
    } 
}


enum_from_primitive! {
#[derive(Debug, PartialEq)]
pub enum TwoOpCode {
    JE = 0x01,
    JL,
    JG,
    DecChk,
    IncChk,
    JIn,
    Test,
    Or,
    And,
    TestAttr,
    SetAttr,
    ClearAttr,
    Store,
    InsertObj,
    LoadW,
    LoadB,
    GetProp,
    GetPropAddr,
    GetNextProp,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Call2S,
    Call2N,
    SetColor,
    Throw,
}
}

enum_from_primitive! {
#[derive(Debug, PartialEq)]
pub enum OneOpCode {
    JZ = 0x00,
    GetSibling,
    GetChild,
    GetParent,
    GetPropLen,
    Inc,
    Dec,
    PrintAddr,
    Call1S,
    RemoveObj,
    PrintObj,
    Ret,
    Jump,
    PrintPAddr,
    Load,
    Call1N, // was "Not" before V5
}
}

enum_from_primitive! {
#[derive(Debug, PartialEq)]
pub enum ZeroOpCode {
    RTrue = 0x00,
    RFalse,
    Print,
    PrintRet,
    Nop,
    Save,
    Restore,
    Restart,
    RetPopped,
    Pop,
    Catch,
    Quit,
    NewLine,
    ShowStatus,
    Verify,
    Extended,
    Piracy,
}
}

enum_from_primitive! {
#[derive(Debug, PartialEq)]
pub enum VarOpCode {
    CallVS = 0x00,
    StoreW,
    StoreB,
    PutProp,
    ARead,
    PrintChar,
    PrintNum,
    Random,
    Push,
    Pull,
    SplitWindow,
    SetWindow,
    CallVS2,
    EraseWindow,
    EraseLine,
    SetCursor,
    GetCursor,
    SetTextStyle,
    BufferMode,
    OutputStream,
    InputStream,
    SoundEffect,
    ReadChar,
    ScanTable,
    Not,
    CallVN,
    CallVN2,
    Tokenise,
    EncodeText,
    CopyTable,
    PrintTable,
    CheckArgCount,
}
}

enum_from_primitive! {
#[derive(Debug, PartialEq)]
pub enum ExtOpCode {
    Save = 0x00,
    Restore,
    LogShift,
    ArtShift,
    SetFont,
    DrawPicture,
    PictureData,
    ErasePicture,
    SetMargins,
    SaveUndo,
    RestoreUndo,
    PrintUnicode,
    CheckUnicode,
    SetTrueColor,

    // V6 commands, don't support
    MoveWindow = 0x10,
    WindowSize,
    WindowStyle,
    GetWindProp,
    ScrollWindow,
    PopStack,
    ReadMouse,
    MouseWindow,
    PushStack,
    PutWindProp,
    PrintForm,
    MakeMenu,
    PictureTable,
    BufferScreen,
}
}
