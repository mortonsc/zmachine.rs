use std::fs::File;
use std::io::prelude::*;

use zmachine::exec::ZMachine;
use zmachine::memory::MemoryMap;

fn main() {
    env_logger::init();

    let mut f = File::open("assets/AllRoads.z5").unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();
    let mut mm = MemoryMap::from_src(&mut src[..]).unwrap();
    let zm = ZMachine::new(&mut mm);
    let mut debug = zm.debug_context();
    debug.repl();
}
