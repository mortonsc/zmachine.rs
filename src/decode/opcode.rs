pub mod zero_op {
    pub const R_TRUE: u8 = 0x0;
    pub const R_FALSE: u8 = 0x1;
    pub const PRINT: u8 = 0x2;
    pub const PRINT_RET: u8 = 0x3;
    pub const NOP: u8 = 0x4;
    pub const RESTART: u8 = 0x7;
    pub const RET_POPPED: u8 = 0x8;
    pub const CATCH: u8 = 0x9;
    pub const QUIT: u8 = 0xa;
    pub const NEW_LINE: u8 = 0xb;
    pub const VERIFY: u8 = 0xd;
    pub const EXTENDED: u8 = 0xe;
    pub const PIRACY: u8 = 0xf;
}

pub mod one_op {
    pub const JZ: u8 = 0x0;
    pub const GET_SIBLING: u8 = 0x1;
    pub const GET_CHILD: u8 = 0x2;
    pub const GET_PARENT: u8 = 0x3;
    pub const GET_PROP_LEN: u8 = 0x4;
    pub const INC: u8 = 0x5;
    pub const DEC: u8 = 0x6;
    pub const PRINT_ADDR: u8 = 0x7;
    pub const CALL_1S: u8 = 0x8;
    pub const REMOVE_OBJ: u8 = 0x9;
    pub const PRINT_OBJ: u8 = 0xa;
    pub const RET: u8 = 0xb;
    pub const JUMP: u8 = 0xc;
    pub const PRINT_PADDR: u8 = 0xd;
    pub const LOAD: u8 = 0xe;
    pub const CALL_1N: u8 = 0xf;
}

pub mod two_op {
    pub const JE: u8 = 0x01;
    pub const JL: u8 = 0x02;
    pub const JG: u8 = 0x03;
    pub const DEC_CHK: u8 = 0x04;
    pub const INC_CHK: u8 = 0x05;
    pub const JIN: u8 = 0x06;
    pub const TEST: u8 = 0x07;
    pub const OR: u8 = 0x08;
    pub const AND: u8 = 0x09;
    pub const TEST_ATTR: u8 = 0x0a;
    pub const SET_ATTR: u8 = 0x0b;
    pub const CLEAR_ATTR: u8 = 0x0c;
    pub const STORE: u8 = 0x0d;
    pub const INSERT_OBJ: u8 = 0x0e;
    pub const LOADW: u8 = 0x0f;
    pub const LOADB: u8 = 0x10;
    pub const GET_PROP: u8 = 0x11;
    pub const GET_PROP_ADDR: u8 = 0x12;
    pub const GET_NEXT_PROP: u8 = 0x13;
    pub const ADD: u8 = 0x14;
    pub const SUB: u8 = 0x15;
    pub const MUL: u8 = 0x16;
    pub const DIV: u8 = 0x17;
    pub const MOD: u8 = 0x18;
    pub const CALL_2S: u8 = 0x19;
    pub const CALL_2N: u8 = 0x1a;
    pub const SET_COLOR: u8 = 0x1b;
    pub const THROW: u8 = 0x1c;
}

pub mod var_op {
    pub const CALL_VS: u8 = 0x00;
    pub const STOREW: u8 = 0x01;
    pub const STOREB: u8 = 0x02;
    pub const PUT_PROP: u8 = 0x03;
    pub const AREAD: u8 = 0x04;
    pub const PRINT_CHAR: u8 = 0x05;
    pub const PRINT_NUM: u8 = 0x06;
    pub const RANDOM: u8 = 0x07;
    pub const PUSH: u8 = 0x08;
    pub const PULL: u8 = 0x09;
    pub const SPLIT_WINDOW: u8 = 0x0a;
    pub const SET_WINDOW: u8 = 0x0b;
    pub const CALL_VS2: u8 = 0x0c;
    pub const ERASE_WINDOW: u8 = 0x0d;
    pub const ERASE_LINE: u8 = 0x0e;
    pub const SET_CURSOR: u8 = 0x0f;
    pub const GET_CURSOR: u8 = 0x10;
    pub const SET_TEXT_STYLE: u8 = 0x11;
    pub const BUFFER_MODE: u8 = 0x12;
    pub const OUTPUT_STREAM: u8 = 0x13;
    pub const INPUT_STREAM: u8 = 0x14;
    pub const SOUND_EFFECT: u8 = 0x15;
    pub const READ_CHAR: u8 = 0x16;
    pub const SCAN_TABLE: u8 = 0x17;
    pub const NOT: u8 = 0x18;
    pub const CALL_VN: u8 = 0x19;
    pub const CALL_VN2: u8 = 0x1a;
    pub const TOKENIZE: u8 = 0x1b;
    pub const ENCODE_TEXT: u8 = 0x1c;
    pub const COPY_TABLE: u8 = 0x1d;
    pub const PRINT_TABLE: u8 = 0x1e;
    pub const CHECK_ARG_COUNT: u8 = 0x1f;
}

pub mod extended {
    pub const SAVE: u8 = 0x00;
    pub const RESTORE: u8 = 0x01;
    pub const LOG_SHIFT: u8 = 0x02;
    pub const ART_SHIFT: u8 = 0x03;
    pub const SET_FONT: u8 = 0x04;
    pub const SAVE_UNDO: u8 = 0x09;
    pub const RESTORE_UNDO: u8 = 0x0a;
    pub const PRINT_UNICODE: u8 = 0x0b;
    pub const CHECK_UNICODE: u8 = 0x0c;
    pub const SET_TRUE_COLOR: u8 = 0x0d;
}
