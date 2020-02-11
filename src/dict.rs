use super::{MemorySlice, ZMachineState};
use crate::text::{ZChar, ZStr};
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy)]
pub struct Dictionary<'a> {
    pub word_seps: MemorySlice<'a>,
    table: MemorySlice<'a>,
    entry_len: usize,
}

impl<'a> Dictionary<'a> {
    pub const KEY_SIZE: usize = 6;

    pub(super) fn new(zm: ZMachineState<'a>, byteaddr: u16) -> Self {
        let mut table = zm.memory().get_subslice_unbounded(byteaddr as usize);
        let n_word_seps = table.take_byte();
        let word_seps = table.take_n_bytes(n_word_seps as usize);
        let entry_len = table.take_byte();
        let entry_len = entry_len as usize;
        assert!(entry_len >= Self::KEY_SIZE);
        let num_entries = table.take_word();
        let table_len = entry_len * (num_entries as usize);
        let table = table.take_n_bytes(table_len);
        Dictionary {
            word_seps,
            table,
            entry_len,
        }
    }

    #[inline]
    pub fn len(self) -> usize {
        self.table.len() / self.entry_len
    }

    pub fn by_index(self, idx: u16) -> DictEntry<'a> {
        let idx = idx as usize;
        let entry_start = idx * self.entry_len;
        let entry_end = entry_start + self.entry_len;
        let mut entry = self.table.get_subslice(entry_start, entry_end);
        let key = entry.take_n_bytes(Self::KEY_SIZE);
        let key = ZStr::from(key);
        DictEntry { key, data: entry }
    }

    pub fn by_key(self, key: impl IntoIterator<Item = ZChar>) -> Option<DictEntry<'a>> {
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

#[derive(Debug, Clone, Copy)]
pub struct DictEntry<'a> {
    key: ZStr<'a>,
    data: MemorySlice<'a>,
}

impl<'a> DictEntry<'a> {
    #[inline]
    pub fn key(self) -> ZStr<'a> {
        self.key
    }

    #[inline]
    pub fn data(self) -> MemorySlice<'a> {
        self.data
    }
}
