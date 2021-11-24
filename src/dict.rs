use super::memory::{MemoryAccess, MemoryMap, MemorySlice};
use crate::text::{zscii_to_zchars, AlphTable, ZChar, ZStr, Zscii};
use std::cmp::Ordering;
use std::iter::once;

#[derive(Debug)]
pub struct Dictionary<'a> {
    pub word_seps: MemorySlice<'a>,
    table: MemorySlice<'a>,
    entry_len: usize,
}

impl<'a> Dictionary<'a> {
    pub const KEY_SIZE: usize = 6;

    pub(super) fn new(mm: &'a MemoryMap<'a>, byteaddr: u16) -> Self {
        // TODO: error checking
        let mut table = mm.slice_from(byteaddr as usize).unwrap();
        let n_word_seps = table.take_byte().unwrap();
        // TODO: enforce that <space> can't be a word separator
        let word_seps = table.take_n_bytes(n_word_seps as usize).unwrap();
        let entry_len = table.take_byte().unwrap();
        let entry_len = entry_len as usize;
        // TODO: shouldn't use an assert for this
        assert!(entry_len >= Self::KEY_SIZE);
        let num_entries = table.take_word().unwrap();
        let table_len = entry_len * (num_entries as usize);
        let table = table.take_n_bytes(table_len).unwrap();
        Dictionary {
            word_seps,
            table,
            entry_len,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.table.len() / self.entry_len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }

    pub fn by_index(&self, idx: u16) -> DictEntry<'a> {
        let idx = idx as usize;
        let entry_start = idx * self.entry_len;
        let entry_end = entry_start + self.entry_len;
        let mut entry = self.table.get_subslice(entry_start, entry_end);
        // TODO: error handling
        let key = entry.take_n_bytes(Self::KEY_SIZE).unwrap();
        let key = ZStr::from(key);
        DictEntry { key, data: entry }
    }

    pub fn by_key(&self, key: impl IntoIterator<Item = ZChar>) -> Option<DictEntry<'a>> {
        let key = crate::text::zchars_to_dict_key(key.into_iter());
        // binary search (min_idx is inclusive, max_idx exclusive)
        let mut min_idx = 0u16;
        let mut max_idx = self.len() as u16;
        while min_idx < max_idx {
            let guess = (min_idx + max_idx) / 2;
            let entry = self.by_index(guess);
            let (new_min, new_max) = match &key[..].cmp(&entry.key) {
                Ordering::Less => (min_idx, guess),
                Ordering::Equal => return Some(entry),
                Ordering::Greater => (guess + 1, max_idx),
            };
            min_idx = new_min;
            max_idx = new_max;
        }
        None
    }
}

#[derive(Debug)]
pub struct DictEntry<'a> {
    key: ZStr<'a>,
    data: MemorySlice<'a>,
}

impl<'a> DictEntry<'a> {
    #[inline]
    pub fn key(&self) -> ZStr<'a> {
        self.key
    }

    #[inline]
    pub fn data(&self) -> MemorySlice<'a> {
        self.data
    }
    #[inline]
    fn byte_addr(&self) -> usize {
        self.data.base_byteaddr() - Dictionary::KEY_SIZE
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParseData {
    pub dict_entry_addr: Option<usize>,
    pub word_len: usize,
    pub pos_in_text_buffer: usize,
}

impl<'a> Dictionary<'a> {
    fn is_sep(&self, zscii: Zscii) -> bool {
        self.word_seps.byte_iter().any(|&z| z == zscii.0)
    }
    fn find_addr_of_entry(&self, word: &[Zscii]) -> Option<usize> {
        // TODO: handle other alphabet tables
        self.by_key(zscii_to_zchars(word.iter().copied(), AlphTable::Default))
            .map(|de| de.byte_addr())
    }
    pub fn parse_zscii_text(&self, text: &[Zscii]) -> Vec<ParseData> {
        let mut results: Vec<ParseData> = Vec::new();
        let mut current_word_offset = 0;
        // TODO: need to convert text to lowercase before searching against dictionary
        // add a space on the end to make sure we parse the last word
        for (i, z) in text.iter().copied().chain(once(Zscii::SPACE)).enumerate() {
            if z == Zscii::SPACE || self.is_sep(z) {
                // if there was some text before this separator, look it up in the dictionary
                if i > current_word_offset {
                    results.push(ParseData {
                        dict_entry_addr: self.find_addr_of_entry(&text[current_word_offset..i]),
                        word_len: i - current_word_offset,
                        pos_in_text_buffer: current_word_offset,
                    });
                }
                // separators other than a space also count as words in their own right
                if z != Zscii::SPACE {
                    results.push(ParseData {
                        dict_entry_addr: self.find_addr_of_entry(&text[i..i + 1]),
                        word_len: 1,
                        pos_in_text_buffer: i,
                    });
                }
                current_word_offset = i + 1;
            }
        }
        results
    }
}
