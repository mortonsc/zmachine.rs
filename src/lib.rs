#[macro_use]
extern crate lazy_static;

pub mod decode;
pub mod exec;
pub mod interface;
pub mod memory;
pub mod text;

mod dict;
mod object;
mod util;

pub use dict::Dictionary;
pub use memory::{MemoryMap, MemorySlice, MemoryWrite, WriteData};
pub use object::{DefaultPropertyTable, Object, ObjectTable, Property, PropertyTable};
pub use text::{AbbrTable, AlphTable, TextEngine, UnicodeTransTable, ZChar, ZStr, ZTextEncodable};

use std::fs::File;
use std::io::prelude::*;

pub fn dump_dictionary(dict: Dictionary) {
    println!("word separators: {:?}", dict.word_seps);
    let len = dict.len() as u16;
    for idx in 0..len {
        let entry = dict.by_index(idx);
        let headword = entry
            .key()
            .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
            .collect::<String>();
        println!("{}", headword);
    }
}

pub fn dump_abbrs() {
    let mut f = File::open("assets/castles.z5").unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();

    let table_addr = u16::from_be_bytes([src[0x18], src[0x19]]) as usize;
    let abbr_table = AbbrTable::from_memory(&src, table_addr).unwrap();

    for zc1 in 1u8..=3 {
        for zc2 in 0u8..=31 {
            let expansion = abbr_table.lookup_expansion(ZChar(zc1), ZChar(zc2)).unwrap();
            let expansion = expansion
                .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
                .collect::<String>();
            println!("{}", expansion);
        }
    }
}

pub fn dump_objs_rec(objs: ObjectTable) {
    for obj in objs {
        if obj.parent().is_none() {
            dump_children_rec(obj, 1)
        }
    }
}

pub fn dump_children_rec(root: Object, indent_level: usize) {
    let name = root
        .short_name()
        .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
        .collect::<String>();
    println!("{}{}. {}", "  ".repeat(indent_level), root.id, name);

    for child in root.children() {
        dump_children_rec(child, indent_level + 1)
    }
}

pub fn dump_objs_seq(objs: ObjectTable) {
    for obj in objs {
        let name = obj
            .short_name()
            .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
            .collect::<String>();
        println!(
            "{}. {}: [parent: {}, sibling: {}, child: {}]",
            obj.id,
            name,
            obj.parent_id(),
            obj.sibling_id(),
            obj.child_id()
        );
    }

    // let obj = objs.by_id(50);
    // for prop in obj.properties() {
    //     println!("{:?}", prop);
    // }
}

pub fn lookup_word(mm: MemoryMap, word: &str) {
    let key = word
        .chars()
        .encode_ztext(AlphTable::Default, UnicodeTransTable::Default)
        .collect::<Vec<_>>();
    let key = ZStr::from(&key[..]).zchars();
    let entry = mm.dictionary().by_key(key).unwrap();
    let headword = entry
        .key()
        .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
        .collect::<String>();
    println!("{}", headword);
}
