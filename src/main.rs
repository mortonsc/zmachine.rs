#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate enum_primitive;

mod instr;
mod text;
mod util;

fn main() {
    text::test();
}
