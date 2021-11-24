use super::*;
use crate::text::ZStr;
use std::cmp;
use std::ops::Deref;

pub mod header;
mod map;

pub use map::MemoryMap;

pub type MemResult<T> = Result<T, MemError>;

pub trait MemoryAccess {
    fn base_byteaddr(&self) -> usize;
    fn contents(&self) -> &[u8];

    #[inline]
    fn len(&self) -> usize {
        self.contents().len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.contents().is_empty()
    }

    fn read_byte(&self, index: usize) -> MemResult<u8> {
        self.contents().get(index).copied().ok_or(MemError {
            byteaddr: self.byteaddr_of_index(index),
            kind: ErrorKind::ByteAddrOutOfRange,
        })
    }

    // TODO: should be i16?
    fn read_word(&self, index: usize) -> MemResult<u16> {
        Ok(u16::from_be_bytes([
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

    fn slice_from(&self, index: usize) -> MemResult<MemorySlice> {
        let base_addr = self.base_byteaddr() + index;
        if index > self.len() {
            Err(MemError {
                byteaddr: base_addr,
                kind: ErrorKind::ByteAddrOutOfRange,
            })
        } else {
            Ok(MemorySlice {
                base_addr: self.base_byteaddr() + index,
                contents: &self.contents()[index..],
            })
        }
    }

    fn slice(&self, start: usize, end: usize) -> MemResult<MemorySlice> {
        assert!(start <= end);
        let base_addr = self.base_byteaddr() + start;
        let end_addr = base_addr + end;
        if end_addr > self.base_byteaddr() + self.len() {
            Err(MemError {
                byteaddr: end_addr,
                kind: ErrorKind::ByteAddrOutOfRange,
            })
        } else {
            Ok(MemorySlice {
                base_addr: self.base_byteaddr() + start,
                contents: &self.contents()[start..end],
            })
        }
    }

    // returns the byteaddr of the byte at the given offset
    #[inline]
    fn byteaddr_of_index(&self, index: usize) -> usize {
        self.base_byteaddr() + index
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
    fn contents(&self) -> &[u8] {
        self.contents
    }
}

impl<'a> MemorySlice<'a> {
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
        if self.is_empty() {
            return None;
        }
        let res = self.read_byte(0).unwrap();
        self.base_addr += 1;
        self.contents = &self.contents[1..];
        Some(res)
    }

    pub fn take_word(&mut self) -> Option<u16> {
        if self.len() < 2 {
            return None;
        }
        let res = self.read_word(0).unwrap();
        self.base_addr += 2;
        self.contents = &self.contents[2..];
        Some(res)
    }

    // splits off a slice containing the first n bytes of this one
    pub fn take_n_bytes(&mut self, n: usize) -> Option<Self> {
        if self.len() < n {
            return None;
        }
        let (taken, remaining) = self.contents.split_at(n);
        let res = MemorySlice {
            base_addr: self.base_addr,
            contents: taken,
        };
        self.base_addr += n;
        self.contents = remaining;
        Some(res)
    }

    pub fn take_zstr(&mut self) -> ZStr<'a> {
        let zstr = ZStr::from(self.contents);
        let n = cmp::min(zstr.len_bytes(), self.len());
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

#[derive(Debug, Clone, Copy)]
pub struct MemError {
    pub byteaddr: usize,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    ByteAddrOutOfRange,
    WriteStaticMem,
    WriteHighMem,
    WriteHeader,
}
