#[macro_use]
extern crate lazy_static;

pub mod instr;
pub mod text;

mod dict;
mod execution;
mod object;
mod util;

pub use dict::Dictionary;
pub use object::{DefaultPropertyTable, Object, ObjectTable, Property, PropertyTable};

use std::ops::Deref;

use text::{AbbrTable, AlphTable, TextEngine, UnicodeTransTable, ZStr};

pub enum Version {
    V5,
    V7,
    V8,
}

const SP: u8 = 0x00;

#[derive(Debug, Clone, Copy)]
pub struct ZMachineState<'a> {
    memory: &'a [u8],
    pc: usize,
    call_stack: &'a [StackFrame],
}

impl<'a> ZMachineState<'a> {
    // returns a MemorySlice containing the entirety of memory
    #[inline]
    pub fn memory(self) -> MemorySlice<'a> {
        MemorySlice {
            base_addr: 0,
            contents: self.memory,
        }
    }
    pub fn header_extension_word(self, index: usize) -> Option<u16> {
        let header_ext_addr = self.memory().get_word(0x36) as usize;
        let header_ext_len = self.memory().get_word(header_ext_addr) as usize;
        if index > header_ext_len {
            None
        } else {
            Some(self.memory().get_word(header_ext_addr + (2 * index)))
        }
    }
    #[inline]
    pub fn objects(self) -> ObjectTable<'a> {
        // object table is located immediately after the default prop table in memory
        let byteaddr = self.memory().get_word(0xa) + (DefaultPropertyTable::SIZE as u16);
        ObjectTable::new(self, byteaddr)
    }
    #[inline]
    pub fn dictionary(self) -> Dictionary<'a> {
        let byteaddr = self.memory().get_word(0x08);
        Dictionary::new(self, byteaddr)
    }
    #[inline]
    pub fn default_properties(self) -> DefaultPropertyTable<'a> {
        let byteaddr = self.memory().get_word(0xa);
        DefaultPropertyTable::new(self, byteaddr)
    }
    pub fn alph_table(self) -> AlphTable<'a> {
        let byteaddr = self.memory().get_word(0x34) as usize;
        if byteaddr == 0 {
            AlphTable::Default
        } else {
            // TODO: log error if the alphabet table can't be read
            AlphTable::from_memory(&self.memory[byteaddr..]).unwrap_or(AlphTable::Default)
        }
    }
    pub fn abbr_table(self) -> AbbrTable<'a> {
        let byteaddr = self.memory().get_word(0x18) as usize;
        //TODO: handle errors
        AbbrTable::from_memory(self.memory, byteaddr).unwrap()
    }
    pub fn unicode_trans_table(self) -> UnicodeTransTable<'a> {
        let byteaddr = self.header_extension_word(3);
        match byteaddr {
            None | Some(0) => UnicodeTransTable::Default,
            // TODO: log error if reading the utt from memory fails
            Some(a) => UnicodeTransTable::from_memory(&self.memory[(a as usize)..])
                .unwrap_or(UnicodeTransTable::Default),
        }
    }
    pub fn text_engine(self) -> TextEngine<'a> {
        // TODO: cache some of this stuff if it's in static memory
        TextEngine::new(
            self.alph_table(),
            self.abbr_table(),
            self.unicode_trans_table(),
        )
    }
}

// wrapper around slices of program memory
// tracks addresses and handles byte -> word conversions
#[derive(Debug, Clone, Copy)]
pub struct MemorySlice<'a> {
    base_addr: usize,
    contents: &'a [u8],
}

impl<'a> MemorySlice<'a> {
    #[inline]
    pub fn len(self) -> usize {
        self.contents.len()
    }

    #[inline]
    pub fn is_empty(self) -> bool {
        self.contents.is_empty()
    }

    #[inline]
    pub fn as_slice(self) -> &'a [u8] {
        self.contents
    }

    // returns the byteaddr of the beginning of this slice
    // (a byteaddr is a u16 representing an address in memory)
    #[inline]
    pub fn byteaddr(self) -> u16 {
        self.base_addr as u16
    }

    // returns the byteaddr of the byte at the given offset
    #[inline]
    pub fn byteaddr_of_offset(self, offset: usize) -> u16 {
        (self.base_addr + offset) as u16
    }

    // returns the byte at the given index within the slice
    #[inline]
    pub fn get_byte(self, index: usize) -> u8 {
        self.contents[index]
    }

    // returns the word whose first byte is at the given index in the slice
    #[inline]
    pub fn get_word(self, index: usize) -> u16 {
        u16::from_be_bytes([self.contents[index], self.contents[index + 1]])
    }

    #[inline]
    #[must_use = "doesn't mutate"]
    pub fn write_byte(self, index: usize, val: u8) -> MemoryWrite {
        MemoryWrite {
            byteaddr: self.byteaddr_of_offset(index),
            data: WriteData::Byte(val),
        }
    }

    #[inline]
    #[must_use = "doesn't mutate"]
    pub fn write_word(self, index: usize, val: u16) -> MemoryWrite {
        MemoryWrite {
            byteaddr: self.byteaddr_of_offset(index),
            data: WriteData::Word(val),
        }
    }

    #[inline]
    pub fn byte_iter(self) -> impl Iterator<Item = &'a u8> {
        self.contents.iter()
    }

    #[inline]
    pub fn bytes(self) -> impl Iterator<Item = u8> + 'a {
        self.byte_iter().copied()
    }

    #[inline]
    pub fn words(self) -> impl Iterator<Item = u16> + 'a {
        self.contents
            .chunks_exact(2)
            .map(|pair| u16::from_be_bytes([pair[0], pair[1]]))
    }

    pub fn take_byte(&mut self) -> u8 {
        let res = self.get_byte(0);
        self.base_addr += 1;
        self.contents = &self.contents[1..];
        res
    }

    pub fn take_word(&mut self) -> u16 {
        let res = self.get_word(0);
        self.base_addr += 2;
        self.contents = &self.contents[2..];
        res
    }

    // splits off a slice containing the first n bytes of this one
    pub fn take_n_bytes(&mut self, n: usize) -> Self {
        let (taken, remaining) = self.contents.split_at(n);
        let res = MemorySlice {
            base_addr: self.base_addr,
            contents: taken,
        };
        self.base_addr += n;
        self.contents = remaining;
        res
    }

    pub fn take_zstr(&mut self) -> ZStr<'a> {
        let zstr = ZStr::from(self.contents);
        let n = zstr.len_bytes();
        let (_, remaining) = self.contents.split_at(n);
        self.base_addr += n;
        self.contents = remaining;
        zstr
    }

    pub fn get_subslice(self, start_index: usize, end_index: usize) -> Self {
        MemorySlice {
            base_addr: self.base_addr + start_index,
            contents: &self.contents[start_index..end_index],
        }
    }

    pub fn get_subslice_unbounded(self, start_index: usize) -> Self {
        MemorySlice {
            base_addr: self.base_addr + start_index,
            contents: &self.contents[start_index..],
        }
    }
}

impl Deref for MemorySlice<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.contents
    }
}

impl<'a> From<MemorySlice<'a>> for ZStr<'a> {
    fn from(ms: MemorySlice<'a>) -> Self {
        ZStr::from(ms.contents)
    }
}

#[derive(Debug)]
pub struct MemoryWrite {
    pub byteaddr: u16,
    pub data: WriteData,
}

#[derive(Debug)]
pub enum WriteData {
    Byte(u8),
    Word(u16),
    ByteRange(Vec<u8>),
    WordRange(Vec<u16>),
    // maybe add ZStr option
}

pub struct ZMachine<'a> {
    // TODO: add version and other data from header
    version: Version,
    memory: &'a mut [u8],
    pc: usize,
    call_stack: Vec<StackFrame>,
}

impl<'a> ZMachine<'a> {
    #[inline]
    pub fn from_src(src: &'a mut [u8]) -> Self {
        ZMachine {
            version: Version::V5, // TODO
            memory: src,
            pc: 0, // TODO
            call_stack: Vec::new(),
        }
    }

    #[inline]
    pub fn state(&'a self) -> ZMachineState<'a> {
        ZMachineState {
            memory: self.memory,
            pc: self.pc,
            call_stack: &self.call_stack,
        }
    }

    #[inline]
    fn stack_frame(&mut self) -> &mut StackFrame {
        let idx = self.call_stack.len() - 1;
        &mut self.call_stack[idx]
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

    // mutable because variable 0x00 pops the stack
    fn get_var(&mut self, var: u8) -> i16 {
        match var {
            0x00 => self.stack_frame().data_stack.pop().unwrap(),
            0x01..=0x0f => self.stack_frame().locals[(var - 1) as usize],
            // read the global var from memory
            0x10..=0xff => unimplemented!(),
        }
    }

    fn set_var(&mut self, var: u8, val: i16) {
        match var {
            0x00 => self.stack_frame().data_stack.push(val),
            0x01..=0x0f => self.stack_frame().locals[(var - 1) as usize] = val,
            0x10..=0xff => unimplemented!(),
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

    pub fn print_zstr(&self, zstr: ZStr) {
        // TODO: all the stuff about different output streams
        let te = self.state().text_engine();
        let unicode_str: String = te.zstr_to_unicode(zstr).collect();
        print!("{}", unicode_str);
    }

    pub fn print_newline(&self) {
        // TODO: different output streams
        println!();
    }
}

#[derive(Debug)]
struct StackFrame {
    // TODO: not sure what needs to be in this
    locals: Vec<i16>,
    data_stack: Vec<i16>,
}
