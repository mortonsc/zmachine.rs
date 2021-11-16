pub mod parse;

pub const SP: u8 = 0x00;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    LargeConst(i16),
    SmallConst(i8),
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
    pub invert_cond: bool,
    pub dst: BranchDst,
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
        // TODO: is the offset a normal operand, or always a constant?
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

    // two-op
    // JE is "two-op" but the standard defines its behavior for 2, 3, or 4 operands
    JE {
        a: Operand,
        others: Vec<Operand>,
        bdata: BranchData,
    },
    JL {
        a: Operand,
        b: Operand,
        bdata: BranchData,
    },
    JG {
        a: Operand,
        b: Operand,
        bdata: BranchData,
    },
    DecChk {
        var_by_ref: Operand,
        cmp_val: Operand,
        bdata: BranchData,
    },
    IncChk {
        var_by_ref: Operand,
        cmp_val: Operand,
        bdata: BranchData,
    },
    JIn {
        obj1: Operand,
        obj2: Operand,
        bdata: BranchData,
    },
    Test {
        bitmap: Operand,
        flags: Operand,
        bdata: BranchData,
    },
    Or {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    And {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    TestAttr {
        obj_id: Operand,
        attr: Operand,
        bdata: BranchData,
    },
    SetAttr {
        obj_id: Operand,
        attr: Operand,
    },
    ClearAttr {
        obj_id: Operand,
        attr: Operand,
    },
    Store {
        var_by_ref: Operand,
        val: Operand,
    },
    InsertObj {
        obj_id: Operand,
        dst_obj: Operand,
    },
    LoadW {
        array: Operand,
        word_index: Operand,
        dst: u8,
    },
    LoadB {
        array: Operand,
        byte_index: Operand,
        dst: u8,
    },
    Add {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    Sub {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    Mul {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    Div {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    Mod {
        a: Operand,
        b: Operand,
        dst: u8,
    },
    Call2S {
        routine_paddr: Operand,
        arg1: Operand,
        dst: u8,
    },
    Call2N {
        routine_paddr: Operand,
        arg1: Operand,
    },

    // var ops
    CallVS {
        routine_paddr: Operand,
        args: Vec<Operand>,
        dst: u8,
    },
}
