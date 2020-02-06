use crate::text::ZStr;
use std::cmp;
use std::convert::TryInto;

// layout of object table in memory:
// [0..2]: default data for property 1
// ...
// [124..126]: default data for property 63
// [126..140]: data for object with id = 0
// ...
// [126+14n..126+14(n+1)]: data for object with id = n
// the end of the table is not marked in any explicit way
struct ObjectTable<'a> {
    memory: &'a [u8],
    default_prop_table_addr: usize,
}

impl<'a> ObjectTable<'a> {
    pub fn new(memory: &'a [u8], addr: usize) -> Self {
        ObjectTable {
            memory,
            default_prop_table_addr: addr,
        }
    }

    #[inline]
    fn object_table_addr(&self) -> usize {
        self.default_prop_table_addr + (Property::DEFAULT_SIZE * Property::MAX_NUM)
    }

    #[inline]
    fn object_id_to_addr(&self, id: u16) -> usize {
        self.object_table_addr() + (Object::SIZE * (id as usize))
    }

    // needed as a helper for guess_max_id()
    #[inline]
    fn addr_to_object_id(&self, addr: usize) -> u16 {
        ((addr - self.object_table_addr()) / Object::SIZE) as u16
    }

    pub fn get_object_by_id(&'a self, id: u16) -> Object<'a> {
        let addr = self.object_id_to_addr(id);
        let data = &self.memory[addr..(addr + Object::SIZE)];
        let data: &[u8; Object::SIZE] = data.try_into().unwrap();
        Object {
            id,
            data,
            object_table: &self,
        }
    }

    #[inline]
    fn prop_id_to_default_addr(&self, prop_id: u8) -> usize {
        let idx = (prop_id as usize) - 1;
        self.default_prop_table_addr + (Property::DEFAULT_SIZE * idx)
    }

    pub fn get_default_prop_by_id(&'a self, prop_id: u8) -> Property<'a> {
        let addr = self.prop_id_to_default_addr(prop_id);
        let data = &self.memory[addr..=(addr + 1)];
        Property {
            data_addr: addr,
            id: prop_id,
            data,
        }
    }

    // try and figure out how many objects there
    // by assuming that a property table immediately follows the object table
    // method suggested by the zmachine spec
    fn guess_max_id(&self) -> u16 {
        let mut min_prop_addr = std::usize::MAX;
        for id in 0..=std::u16::MAX {
            let obj_addr = self.object_id_to_addr(id);
            if min_prop_addr < obj_addr {
                return self.addr_to_object_id(min_prop_addr);
            }
            let prop_addr = self.get_object_by_id(id).prop_table_addr();
            if prop_addr < self.object_table_addr() {
                continue;
            }
            min_prop_addr = std::cmp::min(prop_addr, min_prop_addr);
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
pub struct Object<'a> {
    pub id: u16,
    // rustc crashes if I say [u8; Self::SIZE]
    data: &'a [u8; 14],
    object_table: &'a ObjectTable<'a>,
}

impl Object<'_> {
    const SIZE: usize = 14;

    pub fn parent_id(&self) -> u16 {
        u16::from_be_bytes([self.data[6], self.data[7]])
    }

    pub fn sibling_id(&self) -> u16 {
        u16::from_be_bytes([self.data[8], self.data[9]])
    }

    pub fn child_id(&self) -> u16 {
        u16::from_be_bytes([self.data[10], self.data[11]])
    }

    pub fn get_short_name(&self) -> Option<ZStr> {
        Some(self.get_prop_table()?.short_name)
    }

    pub fn get_prop_by_id(&self, prop_id: u8) -> Property {
        let specific_prop = self
            .get_prop_table()
            .unwrap()
            .into_iter()
            // take advantage of the fact that the table is sorted
            // (in descending order of id)
            .skip_while(|p| p.id > prop_id)
            .next();
        match specific_prop {
            Some(p) if p.id == prop_id => p,
            _ => self.object_table.get_default_prop_by_id(prop_id),
        }
    }

    // returns the first property in this object's property table
    // (or None if the table is empty)
    // never returns a default prop
    // corresponds to `get_next_prop obj 0`
    pub fn get_first_prop(&self) -> Option<Property> {
        self.get_prop_table().unwrap().into_iter().next()
    }

    // returns the property of this object following the property with prop_id in the table
    // unlike get_prop_by_id, this function never returns a default prop
    // corresponds to `get_next_prop obj prop_id`
    pub fn get_next_prop(&self, prop_id: u8) -> Option<Property> {
        let mut props = self
            .get_prop_table()
            .unwrap()
            .into_iter()
            .skip_while(|p| p.id > prop_id);
        let current_prop = props.next();
        // "It is illegal to try to find the next property of a property
        // "which does not exist, and an interpreter should halt with an error message
        // "(if it can efficiently check this condition).
        // TODO: use proper error handling
        assert!(current_prop.unwrap().id == prop_id);
        // None is a valid result; it means current_prop is the last one in the table
        // `get_next_prop` returns 0 in this case
        props.next()
    }

    fn prop_table_addr(&self) -> usize {
        u16::from_be_bytes([self.data[12], self.data[13]]) as usize
    }

    #[inline]
    fn get_prop_table(&self) -> Option<PropertyTable> {
        PropertyTable::new(self.object_table.memory, self.prop_table_addr())
    }
}

// the data in a property consists of 1 to 64 unstructured bytes
pub struct Property<'a> {
    // data_addr isn't needed internally, it's provided to implement get_prop_addr
    // TODO: I think this is the address of the data, not the whole block
    // but I'm not sure
    data_addr: usize,
    id: u8,
    data: &'a [u8],
}

impl Property<'_> {
    const DEFAULT_SIZE: usize = 2;
    const MAX_NUM: usize = 63;
}

struct PropertyTable<'a> {
    short_name: ZStr<'a>,
    table_start_addr: usize,
    memory: &'a [u8],
}

impl<'a> PropertyTable<'a> {
    fn new(memory: &'a [u8], addr: usize) -> Option<Self> {
        let name_len_words = *memory.get(addr)?;
        let name_len_bytes = 2 * (name_len_words as usize);
        let table_start_addr = addr + 1 + name_len_bytes;
        let name_text = memory.get((addr + 1)..table_start_addr)?;
        let short_name = ZStr::from(name_text);
        Some(PropertyTable {
            short_name,
            table_start_addr,
            memory,
        })
    }
}

impl<'a> IntoIterator for PropertyTable<'a> {
    type Item = Property<'a>;
    type IntoIter = PropertyTableIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        PropertyTableIterator {
            addr: self.table_start_addr,
            last_id: 0xff,
            memory: self.memory,
        }
    }
}

struct PropertyTableIterator<'a> {
    addr: usize,
    last_id: u8,
    memory: &'a [u8],
}

impl<'a> Iterator for PropertyTableIterator<'a> {
    type Item = Property<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let byte1 = *self.memory.get(self.addr)?;
        // bottom 6 bits always contain the id
        let id = byte1 & 0x3f;
        if (id == 0) || (id >= self.last_id) {
            // id must be non-zero and strictly decreasing down the table
            // if we hit this case we've probably gone past the end of the table
            return None;
        }
        let data_len: usize;
        let header_len: usize;
        if (byte1 & 0x80) != 0 {
            // 2-byte header format
            header_len = 2;
            let byte2 = *self.memory.get(self.addr + 1)?;
            // bottom 6 bits contain number of data bytes
            // (top 2 bits contain no information)
            let len_mod_64 = (byte2 & 0x3f) as usize;
            // "A value of 0 as property data length (in the second byte)
            // "should be interpreted as a length of 64.
            // "(Inform can compile such properties.)
            data_len = if len_mod_64 == 0 { 64 } else { len_mod_64 };
        } else {
            // 1-byte header format
            header_len = 1;
            // bit 6 specifies the length (so only two possibilities)
            data_len = if (byte1 & 0x40) != 0 { 2 } else { 1 }
        }

        let data_start_addr = self.addr + header_len;
        let data_end_addr = data_start_addr + data_len;
        let data = &self.memory.get(data_start_addr..data_end_addr)?;

        let property = Property {
            data_addr: data_start_addr,
            id,
            data,
        };

        // update internal state before we return
        self.addr = data_end_addr;
        self.last_id = id;

        Some(property)
    }
}

use crate::text::{AlphTable, UnicodeTransTable};

pub fn dump_objs(memory: &[u8]) {
    let object_table_addr = u16::from_be_bytes([memory[0xa], memory[0xb]]) as usize;
    let object_table = ObjectTable::new(memory, object_table_addr);

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
