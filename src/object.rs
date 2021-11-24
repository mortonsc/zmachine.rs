use crate::text::ZStr;
use std::cmp;

use super::memory::*;

// layout of object table in memory:
// [0..14]: object 1
// [14..28] object 2
// ...
// [14(n-1)..14n)]: object n
// the end of the table is not marked in any explicit way.
// (Actually, the Z-Machine spec includes the default property table as part of the object table.
// But they have nothing in common aside from being contiguous in memory.)
#[derive(Debug, Clone, Copy)]
pub struct ObjectTable<'a> {
    table: MemorySlice<'a>,
    mm: &'a MemoryMap<'a>,
}

impl<'a> ObjectTable<'a> {
    #[inline]
    pub(super) fn new(mm: &'a MemoryMap<'a>, byteaddr: u16) -> Self {
        let table = mm.file().get_subslice_unbounded(byteaddr as usize);
        ObjectTable { mm, table }
    }

    #[inline]
    fn object_id_to_byteaddr(self, id: u16) -> usize {
        self.table.base_byteaddr() + ((Object::SIZE * id) as usize)
    }

    // needed as a helper for guess_max_id()
    #[inline]
    fn byteaddr_to_object_id(self, byteaddr: usize) -> u16 {
        ((byteaddr - self.table.base_byteaddr()) / (Object::SIZE as usize)) as u16
    }

    pub fn by_id(self, id: u16) -> Object<'a> {
        let index = (id - 1) as usize;
        let start = index * (Object::SIZE as usize);
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
    pub fn guess_max_id(self) -> u16 {
        let mut min_prop_addr = std::u16::MAX as usize;
        for id in 1..=std::u16::MAX {
            let obj_addr = self.object_id_to_byteaddr(id);
            if min_prop_addr < obj_addr {
                return self.byteaddr_to_object_id(min_prop_addr);
            }
            let prop_addr = self.by_id(id).prop_table_byteaddr() as usize;
            if prop_addr < self.table.base_byteaddr() {
                continue;
            }
            min_prop_addr = cmp::min(prop_addr, min_prop_addr);
        }
        // failure
        panic!();
    }
}

pub struct ObjectIterator<'a> {
    objects: ObjectTable<'a>,
    next_id: u16,
    table_lower_bound: usize,
}

impl<'a> IntoIterator for ObjectTable<'a> {
    type Item = Object<'a>;
    type IntoIter = ObjectIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ObjectIterator {
            objects: self,
            next_id: 1,
            table_lower_bound: u16::max_value() as usize,
        }
    }
}

impl<'a> Iterator for ObjectIterator<'a> {
    type Item = Object<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let obj = self.objects.by_id(self.next_id);
        if obj.data.base_byteaddr() >= self.table_lower_bound {
            return None;
        }

        let prop_addr = obj.prop_table_byteaddr();
        if prop_addr > self.objects.table.base_byteaddr() {
            self.table_lower_bound = cmp::min(prop_addr, self.table_lower_bound);
        }
        self.next_id += 1;

        Some(obj)
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
    mm: &'a MemoryMap<'a>,
}

impl<'a> Object<'a> {
    const SIZE: u16 = 14;
    const PARENT_ID_OFFSET: usize = 6;
    const SIBLING_ID_OFFSET: usize = 8;
    const CHILD_ID_OFFSET: usize = 10;
    const PROP_TABLE_OFFSET: usize = 12;

    #[inline]
    pub fn parent_id(self) -> u16 {
        self.data.read_word(Self::PARENT_ID_OFFSET).unwrap()
    }

    #[inline]
    pub fn sibling_id(self) -> u16 {
        self.data.read_word(Self::SIBLING_ID_OFFSET).unwrap()
    }

    #[inline]
    pub fn child_id(self) -> u16 {
        self.data.read_word(Self::CHILD_ID_OFFSET).unwrap()
    }

    #[inline]
    fn prop_table_byteaddr(self) -> usize {
        self.data.read_word(Self::PROP_TABLE_OFFSET).unwrap() as usize
    }

    pub fn parent(self) -> Option<Object<'a>> {
        let parent_id = self.parent_id();
        if parent_id == 0 {
            None
        } else {
            Some(self.mm.objects().by_id(parent_id))
        }
    }

    pub fn sibling(self) -> Option<Object<'a>> {
        let sibling_id = self.sibling_id();
        if sibling_id == 0 {
            None
        } else {
            Some(self.mm.objects().by_id(sibling_id))
        }
    }

    pub fn child(self) -> Option<Object<'a>> {
        let child_id = self.child_id();
        if child_id == 0 {
            None
        } else {
            Some(self.mm.objects().by_id(child_id))
        }
    }

    #[inline]
    pub fn children(self) -> impl Iterator<Item = Object<'a>> {
        ChildObjectIterator {
            next_child: self.child(),
        }
    }

    #[inline]
    pub fn properties(self) -> PropertyTable<'a> {
        PropertyTable::new(self.mm, self.prop_table_byteaddr()).unwrap()
    }

    #[inline]
    pub fn short_name(self) -> ZStr<'a> {
        self.properties().short_name
    }

    fn attr_location(attr: usize) -> (usize, u8) {
        //" [attributes] are stored topmost bit first: e.g., attribute 0 is stored in bit 7 of the first byte,
        //" attribute 31 is stored in bit 0 of the fourth.
        let byte_num = attr / 8;
        let bit_num = 7 - (attr % 8);
        (byte_num, bit_num as u8)
    }

    pub fn test_attr(self, attr: usize) -> bool {
        let (byte_num, bit_num) = Self::attr_location(attr);
        (self.data.read_byte(byte_num).unwrap() & (1 << bit_num)) != 0
    }

    pub fn set_attr(self, attr: usize) -> MemoryWrite {
        let (byte_num, bit_num) = Self::attr_location(attr);
        let current = self.data.read_byte(byte_num).unwrap();
        self.data.write_byte(byte_num, current | (1 << bit_num))
    }

    pub fn clear_attr(self, attr: usize) -> MemoryWrite {
        let (byte_num, bit_num) = Self::attr_location(attr);
        let current = self.data.read_byte(byte_num).unwrap();
        let bit_mask = 0xff ^ (1 << bit_num);
        self.data.write_byte(byte_num, current & bit_mask)
    }

    #[inline]
    fn set_parent_id(self, parent_id: u16) -> MemoryWrite {
        self.data.write_word(Self::PARENT_ID_OFFSET, parent_id)
    }

    #[inline]
    fn set_sibling_id(self, sibling_id: u16) -> MemoryWrite {
        self.data.write_word(Self::SIBLING_ID_OFFSET, sibling_id)
    }

    #[inline]
    fn set_child_id(self, child_id: u16) -> MemoryWrite {
        self.data.write_word(Self::CHILD_ID_OFFSET, child_id)
    }

    pub fn insert_into(self, dst: Object) -> Vec<MemoryWrite> {
        // "Initially [self] can be at any point in the object tree.
        let mut result = self.remove_from_parent();
        result.push(self.set_parent_id(dst.id));
        result.push(self.set_sibling_id(dst.child_id()));
        result.push(dst.set_child_id(self.id));
        result
    }

    pub fn remove_from_parent(self) -> Vec<MemoryWrite> {
        // TODO: unclear if calling this function when parent() is None
        // is invalid or just a no-op
        // but insert_into calls remove_from_parent() unconditionally
        // so for now it's a no-op for objects with no parents
        let parent = match self.parent() {
            Some(obj) => obj,
            None => return Vec::new(),
        };
        if parent.child_id() == self.id {
            vec![
                parent.set_child_id(self.sibling_id()),
                self.set_parent_id(0),
                self.set_sibling_id(0),
            ]
        } else {
            let prev_sibling = parent
                .children()
                .find(|obj| obj.sibling_id() == self.id)
                .unwrap();
            vec![
                prev_sibling.set_sibling_id(self.sibling_id()),
                self.set_parent_id(0),
                self.set_sibling_id(0),
            ]
        }
    }
}

struct ChildObjectIterator<'a> {
    next_child: Option<Object<'a>>,
}

impl<'a> Iterator for ChildObjectIterator<'a> {
    type Item = Object<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.next_child;
        if let Some(obj) = self.next_child {
            self.next_child = obj.sibling();
        }
        ret
    }
}

// the data in a property consists of 1 to 64 unstructured bytes
#[derive(Debug, Clone, Copy)]
pub struct Property<'a> {
    pub id: u8,
    data: MemorySlice<'a>,
}

impl<'a> Property<'a> {
    pub const MIN_ID: u8 = 1;
    pub const MAX_ID: u8 = 63;
    const DEFAULT_SIZE: usize = 2;

    #[inline]
    pub fn data(self) -> MemorySlice<'a> {
        self.data
    }

    #[inline]
    pub fn len(self) -> usize {
        self.data.len()
    }

    #[inline]
    pub fn byteaddr(self) -> usize {
        self.data.base_byteaddr()
    }

    pub fn put(self, val: i16) -> Option<MemoryWrite> {
        match self.len() {
            1 => Some(MemoryWrite {
                byteaddr: self.byteaddr(),
                data: WriteData::Byte(val as u8),
            }),
            2 => Some(MemoryWrite {
                byteaddr: self.byteaddr(),
                data: WriteData::Word(val as u16),
            }),
            _ => None,
        }
    }

    // returns the property whose memory begins at the given address
    // spec for `get prop_len property_address` says
    // "It is illegal to try to find the property length of a property
    // "which does not exist for the given object, and an interpreter
    // "should halt with an error message
    // "(if it can efficiently check this condition).
    //  which is incomprehensible since the instruction by itself
    //  contains no reference to an object
    //  although in practice it's used along with `get prop_addr object property`
    pub fn by_byteaddr(mm: &'a MemoryMap<'a>, byteaddr: usize) -> Option<Self> {
        let byte_neg1 = mm.read_byte(byteaddr - 1).ok()?;
        let (id, data_len) = if (byte_neg1 & 0x80) != 0 {
            //two-byte header, and this is the second byte
            let byte_neg2 = mm.read_byte(byteaddr - 2).ok()?;
            parse_2byte_prop_header(byte_neg2, byte_neg1)
        } else {
            //one-byte header
            parse_1byte_prop_header(byte_neg1)
        };
        let data = mm.file().get_subslice(byteaddr, byteaddr + data_len);
        Some(Self { id, data })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DefaultPropertyTable<'a> {
    table: MemorySlice<'a>,
}

impl<'a> DefaultPropertyTable<'a> {
    pub(super) const SIZE: usize =
        Property::DEFAULT_SIZE * ((Property::MAX_ID - Property::MIN_ID + 1) as usize);

    pub(super) fn new(mm: &'a MemoryMap<'a>, byteaddr: u16) -> Self {
        let start = byteaddr as usize;
        let end = start + Self::SIZE;
        DefaultPropertyTable {
            table: mm.file().get_subslice(start, end),
        }
    }

    pub fn by_id(self, id: u8) -> Property<'a> {
        let offset = Property::DEFAULT_SIZE * ((id - Property::MIN_ID) as usize);
        Property {
            id,
            data: self.table.get_subslice(offset, offset + 1),
        }
    }
}

#[derive(Debug)]
pub struct PropertyTable<'a> {
    short_name: ZStr<'a>,
    table: MemorySlice<'a>,
    mm: &'a MemoryMap<'a>,
}

impl<'a> PropertyTable<'a> {
    fn new(mm: &'a MemoryMap<'a>, byteaddr: usize) -> Option<Self> {
        let mut table = mm.slice_from(byteaddr).ok()?;
        let name_len_words = table.take_byte()?;
        let name_len_bytes = 2 * (name_len_words as usize);
        let short_name = table.take_n_bytes(name_len_bytes)?;
        let short_name = ZStr::from(short_name);
        Some(PropertyTable {
            short_name,
            table,
            mm,
        })
    }

    pub fn iter(&self) -> PropertyTableIterator<'a> {
        PropertyTableIterator {
            prev_id: Property::MAX_ID + 1,
            table: self.table,
        }
    }

    pub fn first(&self) -> Option<Property<'a>> {
        self.iter().next()
    }

    pub fn by_id(&self, id: u8) -> Option<Property<'a>> {
        self.iter()
            // take advantage of the fact that the table is sorted
            // (in descending order of id)
            .take_while(|p| p.id >= id)
            .find(|p| p.id == id)
    }

    pub fn by_id_with_default(&self, id: u8) -> Property<'a> {
        self.by_id(id)
            .unwrap_or_else(|| self.mm.default_properties().by_id(id))
    }

    pub fn next_after_id(&self, id: u8) -> Option<Property<'a>> {
        let mut iter = self.iter().skip_while(|p| p.id > id);
        let current_prop = iter.next();
        // "It is illegal to try to find the next property of a property
        // "which does not exist, and an interpreter should halt with an error message
        // "(if it can efficiently check this condition).
        // TODO shouldn't really be using an assert for this check though
        assert!(current_prop.unwrap().id == id);
        // None is a valid result; it means current_prop is the last one in the table
        iter.next()
    }
}

impl<'a> IntoIterator for PropertyTable<'a> {
    type Item = Property<'a>;
    type IntoIter = PropertyTableIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
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

fn take_prop_header(table: &mut MemorySlice) -> Option<(u8, usize)> {
    let byte1 = table.take_byte()?;
    if (byte1 & 0x80) != 0 {
        // high bit set in either of first two bytes means two-byte header format
        let byte2 = table.take_byte()?;
        Some(parse_2byte_prop_header(byte1, byte2))
    } else {
        // one-byte header format
        Some(parse_1byte_prop_header(byte1))
    }
}

fn take_property<'a>(table: &mut MemorySlice<'a>) -> Option<Property<'a>> {
    let (id, data_len) = take_prop_header(table)?;
    let data = table.take_n_bytes(data_len)?;
    Some(Property { id, data })
}

pub struct PropertyTableIterator<'a> {
    prev_id: u8,
    table: MemorySlice<'a>,
}

impl<'a> Iterator for PropertyTableIterator<'a> {
    type Item = Property<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let prop = take_property(&mut self.table)?;
        if (prop.id == 0) || (prop.id >= self.prev_id) {
            // id must be non-zero and strictly decreasing down the table
            // if we hit this case we've probably gone past the end of the table
            return None;
        }
        self.prev_id = prop.id;
        Some(prop)
    }
}
