use super::dict::Dictionary;
use super::object::{DefaultPropertyTable, ObjectTable};
use super::*;

#[derive(Debug, Clone, Copy)]
pub struct MemoryMap<'a> {
    memory: &'a [u8],
}

impl<'a> ZMachine<'a> {
    #[inline]
    pub fn memory_map(&'a self) -> MemoryMap<'a> {
        MemoryMap {
            memory: self.memory,
        }
    }
}

impl<'a> MemoryMap<'a> {
    // returns a MemorySlice containing the entirety of memory
    #[inline]
    pub fn file(self) -> MemorySlice<'a> {
        MemorySlice {
            base_addr: 0,
            contents: self.memory,
        }
    }
    pub fn header_extension_word(self, index: usize) -> Option<u16> {
        let header_ext_addr = self.file().get_word(0x36) as usize;
        let header_ext_len = self.file().get_word(header_ext_addr) as usize;
        if index > header_ext_len {
            None
        } else {
            Some(self.file().get_word(header_ext_addr + (2 * index)))
        }
    }
    #[inline]
    pub fn objects(self) -> ObjectTable<'a> {
        // object table is located immediately after the default prop table in memory
        let byteaddr = self.file().get_word(0xa) + (DefaultPropertyTable::SIZE as u16);
        ObjectTable::new(self, byteaddr)
    }
    #[inline]
    pub fn dictionary(self) -> Dictionary<'a> {
        let byteaddr = self.file().get_word(0x08);
        Dictionary::new(self, byteaddr)
    }
    #[inline]
    pub fn default_properties(self) -> DefaultPropertyTable<'a> {
        let byteaddr = self.file().get_word(0xa);
        DefaultPropertyTable::new(self, byteaddr)
    }
    pub fn alph_table(self) -> AlphTable<'a> {
        let byteaddr = self.file().get_word(0x34) as usize;
        if byteaddr == 0 {
            AlphTable::Default
        } else {
            // TODO: log error if the alphabet table can't be read
            AlphTable::from_memory(&self.memory[byteaddr..]).unwrap_or(AlphTable::Default)
        }
    }
    pub fn abbr_table(self) -> AbbrTable<'a> {
        let byteaddr = self.file().get_word(0x18) as usize;
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
    pub base_addr: usize,
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
