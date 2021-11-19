use super::dict::Dictionary;
use super::object::{DefaultPropertyTable, ObjectTable};
use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Version {
    V5,
    V7,
    V8,
}

#[derive(Debug)]
pub struct MemoryMap<'a> {
    memory: &'a mut [u8],
    version: Version,
    static_mem_base_byteaddr: usize,
    global_vars_byteaddr: usize,
    routines_offset: usize,
    strings_offset: usize,
}

impl<'a> MemoryAccess for MemoryMap<'a> {
    #[inline]
    fn base_byteaddr(&self) -> usize {
        0
    }
    #[inline]
    fn contents(&self) -> &[u8] {
        self.memory
    }
}

impl<'a> MemoryMap<'a> {
    pub fn from_src(src: &'a mut [u8]) -> Option<Self> {
        let mm = MemoryMap {
            memory: src,
            version: Version::V5,
            static_mem_base_byteaddr: 0,
            global_vars_byteaddr: 0,
            routines_offset: 0,
            strings_offset: 0,
        };
        mm.initialize()
    }

    fn initialize(mut self) -> Option<Self> {
        let version = self.read_byte(header::VERSION_NUM_B)?;
        self.version = match version {
            1 | 2 | 3 | 4 | 6 => panic!("Version not supported: {}", version),
            5 => Version::V5,
            7 => Version::V7,
            8 => Version::V8,
            _ => panic!("Version not recognized: {}", version),
        };
        self.static_mem_base_byteaddr = self.read_word(header::STATIC_MEM_BASE_BYTEADDR)? as usize;
        self.global_vars_byteaddr = self.read_word(header::GLOBAL_VAR_TABLE_BYTEADDR)? as usize;
        self.routines_offset = self.read_word(header::ROUTINES_OFFSET_W)? as usize;
        self.strings_offset = self.read_word(header::STRING_OFFSET_W)? as usize;
        Some(self)
    }

    pub fn byteaddr_of_global(&self, var: u8) -> usize {
        assert!(var >= 0x10);
        let var = var as usize;
        self.global_vars_byteaddr + 2 * (var - 0x10)
    }

    pub fn routine_paddr_to_byteaddr(&self, paddr: usize) -> usize {
        match self.version {
            Version::V5 => 4 * paddr,
            Version::V7 => (4 * paddr) + (8 * self.routines_offset),
            Version::V8 => 8 * paddr,
        }
    }

    pub fn string_paddr_to_byteaddr(&self, paddr: usize) -> usize {
        match self.version {
            Version::V5 => 4 * paddr,
            Version::V7 => (4 * paddr) + (8 * self.strings_offset),
            Version::V8 => 8 * paddr,
        }
    }

    #[inline]
    pub fn apply_write_byte(&mut self, byteaddr: usize, val: u8) {
        self.memory[byteaddr] = val;
    }

    #[inline]
    pub fn apply_write_word(&mut self, byteaddr: usize, val: u16) {
        let [byte1, byte2] = val.to_be_bytes();
        self.memory[byteaddr] = byte1;
        self.memory[byteaddr + 1] = byte2;
    }

    pub fn apply(&mut self, write: &MemoryWrite) {
        //TODO: enforce boundary between dynamic/static, protect read-only header data, etc
        let addr = write.byteaddr as usize;
        match write.data {
            WriteData::Byte(byte) => self.apply_write_byte(addr, byte),
            WriteData::Word(word) => self.apply_write_word(addr, word),
            WriteData::ByteRange(ref bytes) => {
                for (offset, byte) in bytes.iter().enumerate() {
                    self.apply_write_byte(addr + offset, *byte);
                }
            }
            WriteData::WordRange(ref words) => {
                for (offset, word) in words.iter().enumerate() {
                    self.apply_write_word(addr + (2 * offset), *word);
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

    // returns a MemorySlice containing the entirety of memory
    #[inline]
    pub fn file(&self) -> MemorySlice {
        MemorySlice {
            base_addr: 0,
            contents: self.memory,
        }
    }

    pub fn header_extension_word(&self, index: usize) -> Option<u16> {
        let header_ext_addr = self.read_word(0x36)? as usize;
        let header_ext_len = self.read_word(header_ext_addr)? as usize;
        if index > header_ext_len {
            None
        } else {
            Some(self.read_word(header_ext_addr + (2 * index))?)
        }
    }
    #[inline]
    pub fn objects(&self) -> ObjectTable {
        // object table is located immediately after the default prop table in memory
        let byteaddr = self.read_word(0xa).unwrap() + (DefaultPropertyTable::SIZE as u16);
        ObjectTable::new(self, byteaddr)
    }
    #[inline]
    pub fn dictionary(&self) -> Dictionary {
        let byteaddr = self.read_word(0x08).unwrap();
        Dictionary::new(self, byteaddr)
    }
    #[inline]
    pub fn default_properties<'b>(&self) -> DefaultPropertyTable {
        let byteaddr = self.read_word(0xa).unwrap();
        DefaultPropertyTable::new(self, byteaddr)
    }
    pub fn alph_table(&self) -> AlphTable {
        let byteaddr = self.read_word(0x34).unwrap() as usize;
        if byteaddr == 0 {
            AlphTable::Default
        } else {
            // TODO: log error if the alphabet table can't be read
            AlphTable::from_memory(&self.memory[byteaddr..]).unwrap_or(AlphTable::Default)
        }
    }
    pub fn abbr_table(&self) -> AbbrTable {
        let byteaddr = self.read_word(0x18).unwrap() as usize;
        //TODO: handle errors
        AbbrTable::from_memory(self.memory, byteaddr).unwrap()
    }
    pub fn unicode_trans_table(&self) -> UnicodeTransTable {
        let byteaddr = self.header_extension_word(3);
        match byteaddr {
            None | Some(0) => UnicodeTransTable::Default,
            // TODO: log error if reading the utt from memory fails
            Some(a) => UnicodeTransTable::from_memory(&self.memory[(a as usize)..])
                .unwrap_or(UnicodeTransTable::Default),
        }
    }
    pub fn text_engine(&self) -> TextEngine {
        // TODO: cache some of this stuff if it's in static memory
        TextEngine::new(
            self.alph_table(),
            self.abbr_table(),
            self.unicode_trans_table(),
        )
    }
}
