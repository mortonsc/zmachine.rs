#[macro_use]
extern crate lazy_static;

pub mod instr;
pub mod memory;
pub mod text;

mod dict;
mod execution;
mod object;
mod util;

pub use dict::Dictionary;
pub use memory::{MemoryMap, MemorySlice, MemoryWrite, WriteData};
pub use object::{DefaultPropertyTable, Object, ObjectTable, Property, PropertyTable};

use std::ops::Deref;

use text::{AbbrTable, AlphTable, TextEngine, UnicodeTransTable, ZStr};

pub enum Version {
    V5,
    V7,
    V8,
}

const SP: u8 = 0x00;

pub struct ZMachine<'a> {
    // TODO: add version and other data from header
    version: Version,
    memory: &'a mut [u8],
}

impl<'a> ZMachine<'a> {
    #[inline]
    pub fn from_src(src: &'a mut [u8]) -> Self {
        ZMachine {
            version: Version::V5, // TODO
            memory: src,
        }
    }

    fn routine_paddr_to_byteaddr(&self, paddr: u16) -> usize {
        match self.version {
            Version::V5 => 4 * (paddr as usize),
            Version::V7 => unimplemented!(), // TODO: 4P + 4R_O
            Version::V8 => 8 * (paddr as usize),
        }
    }

    fn string_paddr_to_byteaddr(&self, paddr: u16) -> usize {
        match self.version {
            Version::V5 => 4 * (paddr as usize),
            Version::V7 => unimplemented!(), // TODO: 4P + 4S_O
            Version::V8 => 8 * (paddr as usize),
        }
    }

    #[inline]
    fn write_byte(&mut self, addr: usize, val: u8) {
        self.memory[addr] = val;
    }

    #[inline]
    fn write_word(&mut self, addr: usize, val: u16) {
        let [byte1, byte2] = val.to_be_bytes();
        self.memory[addr] = byte1;
        self.memory[addr + 1] = byte2;
    }

    pub fn apply(&mut self, write: &MemoryWrite) {
        //TODO: enforce boundary between dynamic/static, protect read-only header data, etc
        let addr = write.byteaddr as usize;
        match write.data {
            WriteData::Byte(byte) => self.write_byte(addr, byte),
            WriteData::Word(word) => self.write_word(addr, word),
            WriteData::ByteRange(ref bytes) => {
                for (offset, byte) in bytes.iter().enumerate() {
                    self.write_byte(addr + offset, *byte);
                }
            }
            WriteData::WordRange(ref words) => {
                for (offset, word) in words.iter().enumerate() {
                    self.write_word(addr + (2 * offset), *word);
                }
            }
        }
    }

    #[inline]
    pub fn apply_all(&mut self, writes: &[MemoryWrite]) {
        for write in writes {
            self.apply(write);
        }
    }
}
