use crate::text::Zscii;
use std::convert::TryInto;

pub mod parse;

const DICT_WORD_LEN_BYTES: usize = 6;

#[derive(Debug)]
pub struct DictTable<'a> {
    pub word_seps: Vec<Zscii>,
    pub entries: Vec<DictEntry<'a>>,
}

#[derive(Debug)]
pub struct DictEntry<'a> {
    pub word: &'a [u8; DICT_WORD_LEN_BYTES],
    pub data: &'a [u8],
}


