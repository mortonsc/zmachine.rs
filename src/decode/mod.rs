pub mod opcode;
pub mod parse;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    LargeConst(i16),
    SmallConst(i8),
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
    Print,
    PrintRet,
    Nop,
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
    GetProp {
        obj_id: Operand,
        prop: Operand,
        dst: u8,
    },
    GetPropAddr {
        obj_id: Operand,
        prop: Operand,
        dst: u8,
    },
    GetNextProp {
        obj_id: Operand,
        prop: Operand,
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
    SetColor {
        fg: Operand,
        bg: Operand,
    },
    Throw {
        ret_val: Operand,
        catch_frame: Operand,
    },
    IllegalTwoOp {
        opcode: u8,
    },

    // var ops
    // CallVS includes call_vs and call_vs2
    CallVS {
        routine_paddr: Operand,
        args: Vec<Operand>,
        dst: u8,
    },
    StoreW {
        array: Operand,
        word_index: Operand,
        val: Operand,
    },
    StoreB {
        array: Operand,
        byte_index: Operand,
        val: Operand,
    },
    PutProp {
        obj_id: Operand,
        prop: Operand,
        val: Operand,
    },
    ARead {
        text: Operand,
        parse: Operand,
        // either both or neither of these operands must be provided
        time: Option<Operand>,
        routine_paddr: Option<Operand>,
        dst: u8,
    },
    PrintChar {
        zscii_code: Operand,
    },
    PrintNum {
        val: Operand,
    },
    Random {
        range: Operand,
        dst: u8,
    },
    Push {
        val: Operand,
    },
    Pull {
        var_by_ref: Operand,
    },
    SplitWindow {
        lines: Operand,
    },
    SetWindow {
        window: Operand,
    },
    EraseWindow {
        window: Operand,
    },
    EraseLine {
        val: Operand,
    },
    SetCursor {
        line: Operand,
        column: Operand,
    },
    GetCursor {
        array: Operand,
    },
    SetTextStyle {
        style: Operand,
    },
    BufferMode {
        flag: Operand,
    },
    OutputStream {
        number: Operand,
        table: Operand,
    },
    InputStream {
        number: Operand,
    },
    SoundEffect {
        number: Option<Operand>,
        effect: Option<Operand>,
        volume: Option<Operand>,
        routine: Option<Operand>,
    },
    ReadChar {
        always_one: Operand,
        // both or neither of these must be provided
        time: Option<Operand>,
        routine: Option<Operand>,
        dst: u8,
    },
    ScanTable {
        x: Operand,
        table: Operand,
        len: Operand,
        form: Option<Operand>,
        dst: u8,
        bdata: BranchData,
    },
    Not {
        val: Operand,
        dst: u8,
    },
    // used for both call_vn and call_vn2
    CallVN {
        routine_paddr: Operand,
        args: Vec<Operand>,
    },
    Tokenize {
        text: Operand,
        parse: Operand,
        dictionary: Operand,
        flag: Operand,
    },
    EncodeText {
        zscii_text: Operand,
        length: Operand,
        from: Operand,
        coded_text: Operand,
    },
    CopyTable {
        first: Operand,
        second: Operand,
        size: Operand,
    },
    PrintTable {
        zscii_text: Operand,
        width: Operand,
        height: Option<Operand>,
        skip: Option<Operand>,
    },
    CheckArgCount {
        arg_num: Operand,
        bdata: BranchData,
    },

    // extended opcodes
    Save {
        table: Option<Operand>,
        bytes: Option<Operand>,
        name: Option<Operand>,
        prompt: Option<Operand>,
        dst: u8,
    },
    Restore {
        table: Option<Operand>,
        bytes: Option<Operand>,
        name: Option<Operand>,
        prompt: Option<Operand>,
        dst: u8,
    },
    LogShift {
        number: Operand,
        places: Operand,
        dst: u8,
    },
    ArtShift {
        number: Operand,
        places: Operand,
        dst: u8,
    },
    SetFont {
        font: Operand,
        dst: u8,
    },
    SaveUndo {
        dst: u8,
    },
    RestoreUndo {
        dst: u8,
    },
    PrintUnicode {
        char_num: Operand,
    },
    CheckUnicode {
        char_num: Operand,
        dst: u8,
    },
    SetTrueColor {
        fg: Operand,
        bg: Operand,
    },
    IllegalExtended(u8),
}
