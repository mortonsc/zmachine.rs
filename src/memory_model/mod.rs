mod object;

pub use object::{dump_objs, DefaultPropertyTable, Object, ObjectTable, Property, PropertyTable};

use crate::text::ZStr;

#[derive(Debug, Clone, Copy)]
pub struct MemoryModel<'a> {
    memory: &'a [u8],
}

impl<'a> MemoryModel<'a> {
    pub fn from_src(src: &'a [u8]) -> Self {
        MemoryModel { memory: src }
    }

    // returns a MemorySlice containing the entirety of memory
    #[inline]
    pub fn memory(self) -> MemorySlice<'a> {
        MemorySlice {
            base_addr: 0,
            contents: self.memory,
        }
    }

    #[inline]
    pub fn object_table(self) -> ObjectTable<'a> {
        // object table is located immediately after the default prop table in memory
        let byteaddr = self.memory().get_word(0xa) + (DefaultPropertyTable::SIZE as u16);
        ObjectTable::new(self, byteaddr)
    }

    #[inline]
    pub fn default_prop_table(self) -> DefaultPropertyTable<'a> {
        let byteaddr = self.memory().get_word(0xa);
        DefaultPropertyTable::new(self, byteaddr)
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

    #[must_use = "returns new slice, doesn't mutate"]
    pub fn take_byte(self) -> (Self, u8) {
        (
            MemorySlice {
                base_addr: self.base_addr + 1,
                contents: &self.contents[1..],
            },
            self.get_byte(0),
        )
    }

    #[must_use = "returns new slice, doesn't mutate"]
    pub fn take_word(self) -> (Self, u16) {
        (
            MemorySlice {
                base_addr: self.base_addr + 2,
                contents: &self.contents[2..],
            },
            self.get_word(0),
        )
    }

    // splits off a slice containing the first n bytes of this one
    // returns (remaining, taken)
    // on the model of nom
    pub fn take_n_bytes(self, n: usize) -> (Self, Self) {
        let (taken, remaining) = self.contents.split_at(n);
        let taken = MemorySlice {
            base_addr: self.base_addr,
            contents: taken,
        };
        let remaining = MemorySlice {
            base_addr: self.base_addr + n,
            contents: remaining,
        };
        (remaining, taken)
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

impl<'a> From<MemorySlice<'a>> for ZStr<'a> {
    fn from(ms: MemorySlice<'a>) -> Self {
        ZStr::from(ms.contents)
    }
}


