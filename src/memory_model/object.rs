use crate::text::ZStr;
use std::cmp;

use super::{MemoryModel, MemorySlice};

// layout of object table in memory:
// [0..14]: object 0
// [14..28] object 1
// ...
// [14n..14(n+1)]: object n
// the end of the table is not marked in any explicit way.
// (Actually, the Z-Machine spec includes the default property table as part of the object table.
// But they have nothing in common aside from being contiguous in memory.)
#[derive(Debug, Clone, Copy)]
pub struct ObjectTable<'a> {
    table: MemorySlice<'a>,
    mm: MemoryModel<'a>,
}

impl<'a> ObjectTable<'a> {
    #[inline]
    pub(super) fn new(mm: MemoryModel<'a>, byteaddr: u16) -> Self {
        let table = mm.memory().get_subslice_unbounded(byteaddr as usize);
        ObjectTable { mm, table }
    }

    #[inline]
    fn object_id_to_byteaddr(self, id: u16) -> u16 {
        self.table.byteaddr() + (Object::SIZE * id)
    }

    // needed as a helper for guess_max_id()
    #[inline]
    fn byteaddr_to_object_id(self, byteaddr: u16) -> u16 {
        (byteaddr - self.table.byteaddr()) / Object::SIZE
    }

    pub fn get_object_by_id(&'a self, id: u16) -> Object<'a> {
        let start = (id * Object::SIZE) as usize;
        let end = start + (Object::SIZE as usize);
        let data = self.table.get_subslice(start, end);
        Object {
            id,
            data,
            mm: self.mm,
        }
    }

    // try and figure out how many objects there
    // by assuming that a property table immediately follows the object table
    // method suggested by the zmachine spec
    fn guess_max_id(self) -> u16 {
        let mut min_prop_addr = std::u16::MAX;
        for id in 0..=std::u16::MAX {
            let obj_addr = self.object_id_to_byteaddr(id);
            if min_prop_addr < obj_addr {
                return self.byteaddr_to_object_id(min_prop_addr);
            }
            let prop_addr = self.get_object_by_id(id).prop_table_byteaddr();
            if prop_addr < self.table.byteaddr() {
                continue;
            }
            min_prop_addr = cmp::min(prop_addr, min_prop_addr);
        }
        // failure
        panic!();
    }
}

// data layout of object:
// [0..6]: attribute flags
// [6..8]: id of parent
// [8..10]: id of sibling
// [10..12]: id of child
// [12..14]: byte addr of properties table
#[derive(Debug, Clone, Copy)]
pub struct Object<'a> {
    pub id: u16,
    data: MemorySlice<'a>,
    mm: MemoryModel<'a>,
}

impl<'a> Object<'a> {
    const SIZE: u16 = 14;

    #[inline]
    pub fn parent_id(self) -> u16 {
        self.data.get_word(6)
    }

    #[inline]
    pub fn sibling_id(self) -> u16 {
        self.data.get_word(8)
    }

    #[inline]
    pub fn child_id(self) -> u16 {
        self.data.get_word(10)
    }

    #[inline]
    fn prop_table_byteaddr(self) -> u16 {
        self.data.get_word(12)
    }

    pub fn get_short_name(self) -> Option<ZStr<'a>> {
        Some(self.get_prop_table().short_name)
    }

    pub fn get_prop_by_id(self, prop_id: u8) -> Property<'a> {
        let specific_prop = self
            .get_prop_table()
            .into_iter()
            // take advantage of the fact that the table is sorted
            // (in descending order of id)
            .skip_while(|p| p.id > prop_id)
            .next();
        match specific_prop {
            Some(p) if p.id == prop_id => p,
            _ => self.mm.default_prop_table().get_prop_by_id(prop_id),
        }
    }

    // returns the first property in this object's property table
    // (or None if the table is empty)
    // never returns a default prop
    // corresponds to `get_next_prop obj 0`
    pub fn get_first_prop(self) -> Option<Property<'a>> {
        self.get_prop_table().into_iter().next()
    }

    // returns the property of this object following the property with prop_id in the table
    // unlike get_prop_by_id, this function never returns a default prop
    // corresponds to `get_next_prop obj prop_id`
    pub fn get_next_prop(self, prop_id: u8) -> Option<Property<'a>> {
        let mut props = self
            .get_prop_table()
            .into_iter()
            .skip_while(|p| p.id > prop_id);
        let current_prop = props.next();
        // "It is illegal to try to find the next property of a property
        // "which does not exist, and an interpreter should halt with an error message
        // "(if it can efficiently check this condition).
        assert!(current_prop.unwrap().id == prop_id);
        // None is a valid result; it means current_prop is the last one in the table
        // `get_next_prop` will return 0 in this case
        props.next()
    }

    #[inline]
    fn get_prop_table(self) -> PropertyTable<'a> {
        PropertyTable::new(self.mm, self.prop_table_byteaddr())
    }
}

// the data in a property consists of 1 to 64 unstructured bytes
#[derive(Debug, Clone, Copy)]
pub struct Property<'a> {
    pub id: u8,
    pub data: MemorySlice<'a>,
}

impl Property<'_> {
    pub const MIN_ID: u8 = 1;
    pub const MAX_ID: u8 = 63;
    const DEFAULT_SIZE: usize = 2;
}

#[derive(Debug, Clone, Copy)]
pub struct DefaultPropertyTable<'a> {
    table: MemorySlice<'a>,
}

impl<'a> DefaultPropertyTable<'a> {
    pub(super) const SIZE: usize =
        Property::DEFAULT_SIZE * ((Property::MAX_ID - Property::MIN_ID + 1) as usize);

    pub(super) fn new(mm: MemoryModel<'a>, byteaddr: u16) -> Self {
        let start = byteaddr as usize;
        let end = start + Self::SIZE;
        DefaultPropertyTable {
            table: mm.memory().get_subslice(start, end),
        }
    }

    fn get_prop_by_id(self, id: u8) -> Property<'a> {
        let offset = Property::DEFAULT_SIZE * ((id - Property::MIN_ID) as usize);
        Property {
            id,
            data: self.table.get_subslice(offset, offset + 1),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PropertyTable<'a> {
    short_name: ZStr<'a>,
    table: MemorySlice<'a>,
}

impl<'a> PropertyTable<'a> {
    fn new(mm: MemoryModel<'a>, byteaddr: u16) -> Self {
        let table = mm.memory().get_subslice_unbounded(byteaddr as usize);
        let (table, name_len_words) = table.take_byte();
        let name_len_bytes = 2 * (name_len_words as usize);
        let (table, short_name) = table.take_n_bytes(name_len_bytes);
        let short_name = ZStr::from(short_name);
        PropertyTable { short_name, table }
    }
}

impl<'a> IntoIterator for PropertyTable<'a> {
    type Item = Property<'a>;
    type IntoIter = PropertyTableIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        PropertyTableIterator {
            prev_id: Property::MAX_ID + 1,
            table: self.table,
        }
    }
}

fn parse_1byte_prop_header(byte: u8) -> (u8, usize) {
    // id stored in bits 0..6
    let id = byte & 0x3f;
    // bit 6 marks the length (so only two possibilities)
    let data_len = if (byte & 0x40) != 0 { 2 } else { 1 };
    (id, data_len)
}

fn parse_2byte_prop_header(byte1: u8, byte2: u8) -> (u8, usize) {
    // id stored in bits 0..6 of first byte (same as one-byte format)
    let id = byte1 & 0x3f;
    // length stored in bits 0..6 of second byte
    // (bit 6 is unused, bit 7 is always set)
    let data_len = (byte2 & 0x3f) as usize;
    // "A value of 0 as property data length (in the second byte)
    // "should be interpreted as a length of 64.
    // "(Inform can compile such properties.)
    let data_len = if data_len == 0 { 64 } else { data_len };
    (id, data_len)
}

fn take_prop_header<'a>(table: MemorySlice<'a>) -> (MemorySlice<'a>, u8, usize) {
    let (table, byte1) = table.take_byte();
    if (byte1 & 0x80) != 0 {
        // high bit set in either of first two bytes means two-byte header format
        let (table, byte2) = table.take_byte();
        let (id, data_len) = parse_2byte_prop_header(byte1, byte2);
        (table, id, data_len)
    } else {
        // one-byte header format
        let (id, data_len) = parse_1byte_prop_header(byte1);
        (table, id, data_len)
    }
}

fn take_property<'a>(table: MemorySlice<'a>) -> (MemorySlice<'a>, Property<'a>) {
    let (table, id, data_len) = take_prop_header(table);
    let (table, data) = table.take_n_bytes(data_len);
    (table, Property { id, data })
}

pub struct PropertyTableIterator<'a> {
    prev_id: u8,
    table: MemorySlice<'a>,
}

impl<'a> Iterator for PropertyTableIterator<'a> {
    type Item = Property<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (table, prop) = take_property(self.table);
        if (prop.id == 0) || (prop.id >= self.prev_id) {
            // id must be non-zero and strictly decreasing down the table
            // if we hit this case we've probably gone past the end of the table
            return None;
        }

        // update internal state before we return
        self.prev_id = prop.id;
        self.table = table;

        Some(prop)
    }
}

use crate::text::{AlphTable, UnicodeTransTable};

pub fn dump_objs(mm: &MemoryModel) {
    let object_table = mm.object_table();

    let num = object_table.guess_max_id();

    for id in 0..num {
        let obj = object_table.get_object_by_id(id);
        let name = obj
            .get_short_name()
            .unwrap()
            .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
            .collect::<String>();
        println!("{}", name);
    }
}
