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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Version {
    V5,
    V7,
    V8,
}

pub struct ZMachine<'a> {
    // TODO: add other data from header
    memory: &'a mut [u8],
    version: Version,
    global_vars_byteaddr: usize,
    routines_offset: usize,
    strings_offset: usize,
}

impl<'a> ZMachine<'a> {
    #[inline]
    pub fn from_src(src: &'a mut [u8]) -> Self {
        let mut zm = ZMachine {
            memory: src,
            version: Version::V5,
            global_vars_byteaddr: 0,
            routines_offset: 0,
            strings_offset: 0,
        };
        zm.initialize();
        zm
    }

    fn initialize(&mut self) {
        let version = self.read_byte(0x00);
        self.version = match version {
            1 | 2 | 3 | 4 | 6 => panic!("Version not supported: {}", version),
            5 => Version::V5,
            7 => Version::V7,
            8 => Version::V8,
            _ => panic!("Version not recognized: {}", version),
        };
        self.global_vars_byteaddr = self.read_word(0x0c) as usize;
        self.routines_offset = self.read_word(0x28) as usize;
        self.strings_offset = self.read_word(0x2a) as usize;
    }

    pub fn routine_paddr_to_byteaddr(&self, paddr: u16) -> usize {
        let paddr = paddr as usize;
        match self.version {
            Version::V5 => 4 * paddr,
            Version::V7 => (4 * paddr) + (8 * self.routines_offset),
            Version::V8 => 8 * paddr,
        }
    }

    pub fn string_paddr_to_byteaddr(&self, paddr: u16) -> usize {
        let paddr = paddr as usize;
        match self.version {
            Version::V5 => 4 * paddr,
            Version::V7 => (4 * paddr) + (8 * self.strings_offset),
            Version::V8 => 8 * paddr,
        }
    }

    pub fn byteaddr_of_global(&self, var: u8) -> usize {
        assert!(var >= 0x10);
        let var = var as usize;
        self.global_vars_byteaddr + 2 * (var - 0x10)
    }

    #[inline]
    pub fn read_byte(&self, addr: usize) -> u8 {
        self.memory[addr]
    }

    #[inline]
    pub fn read_word(&self, addr: usize) -> u16 {
        u16::from_be_bytes([self.memory[addr], self.memory[addr + 1]])
    }

    #[inline]
    pub fn write_byte(&mut self, addr: usize, val: u8) {
        self.memory[addr] = val;
    }

    #[inline]
    pub fn write_word(&mut self, addr: usize, val: u16) {
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
