use super::*;
use crate::text::ZStr;
use std::ops::Deref;

mod map;

pub use map::MemoryMap;

pub trait MemoryAccess {
    fn base_byteaddr(&self) -> usize;
    fn contents<'b>(&'b self) -> &'b [u8];

    #[inline]
    fn len(&self) -> usize {
        self.contents().len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.contents().is_empty()
    }

    fn read_byte(&self, index: usize) -> Option<u8> {
        self.contents().get(index).copied()
    }

    // TODO: should be i16?
    fn read_word(&self, index: usize) -> Option<u16> {
        Some(u16::from_be_bytes([
            self.read_byte(index)?,
            self.read_byte(index + 1)?,
        ]))
    }

    #[must_use = "doesn't mutate"]
    fn write_byte(&self, index: usize, val: u8) -> MemoryWrite {
        MemoryWrite {
            byteaddr: self.base_byteaddr() + index,
            data: WriteData::Byte(val),
        }
    }

    #[must_use = "doesn't mutate"]
    fn write_word(&self, index: usize, val: u16) -> MemoryWrite {
        MemoryWrite {
            byteaddr: self.base_byteaddr() + index,
            data: WriteData::Word(val),
        }
    }
}

// wrapper around slices of program memory
// tracks addresses and handles byte -> word conversions
#[derive(Debug, Clone, Copy)]
pub struct MemorySlice<'a> {
    base_addr: usize,
    contents: &'a [u8],
}

impl<'a> MemoryAccess for MemorySlice<'a> {
    #[inline]
    fn base_byteaddr(&self) -> usize {
        self.base_addr
    }
    #[inline]
    fn contents<'b>(&'b self) -> &'b [u8] {
        self.contents
    }
}

impl<'a> MemorySlice<'a> {
    // returns the byteaddr of the byte at the given offset
    #[inline]
    pub fn byteaddr_of_offset(self, offset: usize) -> usize {
        self.base_addr + offset
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

    // TODO: a lot of those functions should return Options

    pub fn take_byte(&mut self) -> Option<u8> {
        let res = self.read_byte(0)?;
        self.base_addr += 1;
        self.contents = &self.contents[1..];
        Some(res)
    }

    pub fn take_word(&mut self) -> Option<u16> {
        let res = self.read_word(0)?;
        self.base_addr += 2;
        self.contents = &self.contents[2..];
        Some(res)
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
    pub byteaddr: usize,
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
