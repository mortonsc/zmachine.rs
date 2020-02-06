#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate enum_primitive;

mod instr;
mod dict;
mod text;
mod object;
mod util;

use std::fs::File;
use std::io::prelude::*;

use text::{AlphTable, UnicodeTransTable, AbbrTable, ZChar, ZStr};

fn dump_dictionary(src: &[u8]) {
    let table_addr = u16::from_be_bytes([src[8], src[9]]) as usize;
    let (_, dict_table) = dict::parse::dict_table(&src[table_addr..]).unwrap();

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

fn main() {
    let mut f = File::open("assets/AllRoads.z5").unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();

    object::dump_objs(&src);
}
