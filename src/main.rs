#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate enum_primitive;

mod instr;
mod dict;
mod text;
mod util;

use std::fs::File;
use std::io::prelude::*;

use text::{AlphTable, UnicodeTransTable, ZTextDecodable};

fn main() {
    let mut f = File::open("assets/castles.z5").unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();

    let table_addr = u16::from_be_bytes([src[8], src[9]]) as usize;
    let (_, dict_table) = dict::parse::dict_table(&src[table_addr..]).unwrap();

    println!("word separators: {:?}", dict_table.word_seps);
    for entry in dict_table.entries {
        let headword = entry
            .word
            .iter()
            .copied()
            .decode_ztext(AlphTable::Default, UnicodeTransTable::Default)
            .collect::<String>();
        println!("{}: {:?}", headword, entry.data);
    }
}
