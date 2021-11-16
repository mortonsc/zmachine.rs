pub mod parse;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    LargeConst(u16),
    SmallConst(u8),
    Variable(u8),
}

impl Operand {
    // TODO: not sure what type this should return exactly
    // fn value(self, zm: &ZMachineState) -> i16;
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

#[derive(Debug)]
pub enum Instr {
    // zero op
    RTrue,
    RFalse,
    Print {
        ztext: Vec<u8>,
    },
    PrintRet {
        ztext: Vec<u8>,
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
