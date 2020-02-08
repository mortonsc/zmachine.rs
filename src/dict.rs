use super::{MemoryModel, MemorySlice};
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

    pub(super) fn new(mm: MemoryModel<'a>, byteaddr: u16) -> Self {
        let table = mm.memory().get_subslice_unbounded(byteaddr as usize);
        let (table, n_word_seps) = table.take_byte();
        let (table, word_seps) = table.take_n_bytes(n_word_seps as usize);
        let (table, entry_len) = table.take_byte();
        let entry_len = entry_len as usize;
        assert!(entry_len >= Self::KEY_SIZE);
        let (table, num_entries) = table.take_word();
        let table_len = entry_len * (num_entries as usize);
        let (_, table) = table.take_n_bytes(table_len);
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
        let entry = self.table.get_subslice(entry_start, entry_end);
        let (data, key) = entry.take_n_bytes(Self::KEY_SIZE);
        let key = ZStr::from(key);
        DictEntry { key, data }
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
