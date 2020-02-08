use std::fs::File;
use std::io::prelude::*;

use zmachine::text::{AbbrTable, AlphTable, UnicodeTransTable, ZChar, ZStr};
use zmachine::{MemoryModel, Object, ObjectTable};

fn dump_dictionary(src: &[u8]) {
    let table_addr = u16::from_be_bytes([src[8], src[9]]) as usize;
    let (_, dict_table) = zmachine::dict::parse::dict_table(&src[table_addr..]).unwrap();

    println!("word separators: {:?}", dict_table.word_seps);
    for entry in dict_table.entries {
        let headword = ZStr::from(&entry.word[..])
            .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
            .collect::<String>();
        println!("{}: {:?}", headword, entry.data);
    }
}

fn dump_abbrs() {
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

fn dump_objs_rec(objs: ObjectTable) {
    for obj in objs {
        if obj.parent().is_none() {
            dump_children_rec(obj, 1)
        }
    }
}

fn dump_children_rec(root: Object, indent_level: usize) {
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

fn main() {
    let mut f = File::open("assets/AllRoads.z5").unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();
    let mm = MemoryModel::from_src(&src);

    dump_objs_rec(mm.objects());
}
