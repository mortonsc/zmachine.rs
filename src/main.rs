use std::fs::File;
use std::io::prelude::*;

use zmachine::exec::ZMachine;
use zmachine::interface::DebugInterface;
use zmachine::memory::MemoryMap;

fn main() {
    env_logger::init();

    let mut f = File::open("assets/AllRoads.z5").unwrap();
    let mut src = Vec::new();
    f.read_to_end(&mut src).unwrap();
    let mut mm = MemoryMap::from_src(&mut src[..]).unwrap();
    let mut interface = DebugInterface::new();
    let zm = ZMachine::new(&mut mm, &mut interface);
    let mut debug = zm.debug_context();
    debug.repl();
}
