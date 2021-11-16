use super::instr::parse::decode_instr;
use super::instr::Instr;
use super::text::ZStr;
use super::{ZMachine, SP};

#[derive(Debug)]
struct StackFrame {
    // TODO: not sure what needs to be in this
    ret_addr: usize,
    ret_dst: u8,
    locals: Vec<i16>,
    data_stack: Vec<i16>,
}

#[derive(Debug, Clone)]
enum CtrlFlow {
    Proceed,
    Jump {
        offset: i16,
    },
    Call {
        paddr: u16,
        ret_dst: u8,
        args: Vec<i16>,
    },
    Return {
        ret_val: i16,
    },
    Throw {
        ret_val: i16,
        catch_frame: usize,
    },
    Quit,
}

struct ProgramState<'a> {
    zm: &'a mut ZMachine<'a>,
    call_stack: Vec<StackFrame>,
}

impl<'a> ZMachine<'a> {
    pub fn execute_program(&'a mut self) {
        let mut pc = self.memory_map().file().get_word(0x06) as usize;
        let mut program_state = ProgramState {
            zm: self,
            call_stack: Vec::new(),
        };
        while let Some(next_pc) = program_state.step(pc) {
            pc = next_pc;
        }
    }
}

impl<'a> ProgramState<'a> {
    pub const MAX_N_LOCALS: u8 = 15;

    #[inline]
    fn stack_frame(&mut self) -> Option<&mut StackFrame> {
        self.call_stack.last_mut()
    }

    // mutable because variable 0x00 pops the stack
    fn get_var(&mut self, var: u8) -> Option<i16> {
        match var {
            0x00 => self.stack_frame()?.data_stack.pop(),
            0x01..=0x0f => self.stack_frame()?.locals.get((var - 1) as usize).copied(),
            // read the global var from memory
            0x10..=0xff => unimplemented!(),
        }
    }

    fn set_var(&mut self, var: u8, val: i16) {
        // TODO: lots of error checking
        match var {
            0x00 => self.stack_frame().unwrap().data_stack.push(val),
            0x01..=0x0f => {
                *self
                    .stack_frame()
                    .unwrap()
                    .locals
                    .get_mut((var - 1) as usize)
                    .unwrap() = val
            }
            0x10..=0xff => unimplemented!(),
        }
    }

    fn step(&mut self, pc: usize) -> Option<usize> {
        let (next_pc, instr) = {
            let code = &self.zm.memory[pc..];
            let (_, instr) = decode_instr(code).unwrap();
            // TODO: determine the actual next pc
            (0usize, instr)
        };
        let ctrl_flow = self.execute_instr(instr);
        match ctrl_flow {
            CtrlFlow::Proceed => Some(next_pc),
            CtrlFlow::Jump { offset } => {
                let target = (next_pc as i64) + (offset as i64) - 2;
                if target < 0 {
                    panic!("out of bounds branch");
                }
                Some(target as usize)
            }
            CtrlFlow::Call {
                paddr,
                ret_dst,
                args,
            } => Some(self.call_routine(paddr, next_pc, ret_dst, args)),
            CtrlFlow::Return { ret_val } => Some(self.return_from_routine(ret_val)),
            CtrlFlow::Throw {
                ret_val,
                catch_frame,
            } => {
                if catch_frame >= self.call_stack.len() {
                    panic!("attempt to throw with invalid stack frame");
                }
                // "[throw] returns as if from the routine which executed the catch which found this stack frame value.
                // truncate the call stack so that the requested stack frame is on top,
                // then return like normal
                self.call_stack.truncate(catch_frame + 1);
                Some(self.return_from_routine(ret_val))
            }
            CtrlFlow::Quit => None,
        }
    }

    fn call_routine(&mut self, paddr: u16, ret_addr: usize, ret_dst: u8, args: Vec<i16>) -> usize {
        let mut new_frame = StackFrame {
            ret_addr,
            ret_dst,
            locals: args,
            data_stack: Vec::new(),
        };
        let byteaddr = self.zm.routine_paddr_to_byteaddr(paddr);
        let n_locals = self.zm.memory_map().file().get_byte(byteaddr);
        // TODO error handling
        if n_locals > Self::MAX_N_LOCALS {
            panic!("invalid number of local variables: {}", n_locals);
        }
        new_frame.locals.resize(n_locals as usize, 0);
        self.call_stack.push(new_frame);
        byteaddr + 1
    }

    fn return_from_routine(&mut self, ret_val: i16) -> usize {
        if self.call_stack.is_empty() {
            panic!("attempt to return when not in a routine");
        }
        let ret_addr = self.stack_frame().ret_addr;
        let ret_dst = self.stack_frame().ret_dst;
        self.call_stack.pop();
        // important to do this after pop'ing
        // so we're setting variables of the caller not the callee
        self.set_var(ret_dst, ret_val);
        ret_addr
    }

    fn print_zstr(&self, zstr: ZStr) {
        // TODO: all the stuff about different output streams
        let te = self.zm.memory_map().text_engine();
        let unicode_str: String = te.zstr_to_unicode(zstr).collect();
        print!("{}", unicode_str);
    }

    fn print_newline(&self) {
        // TODO: different output streams
        println!();
    }

    fn execute_instr(&mut self, instr: Instr) -> CtrlFlow {
        match instr {
            Instr::RTrue => CtrlFlow::Return { ret_val: 1 },
            Instr::RFalse => CtrlFlow::Return { ret_val: 0 },
            Instr::Print { ztext } => {
                self.print_zstr(ZStr::from(&ztext[..]));
                CtrlFlow::Proceed
            }
            Instr::PrintRet { ztext } => {
                self.print_zstr(ZStr::from(&ztext[..]));
                self.print_newline();
                CtrlFlow::Return { ret_val: 1 }
            }
            Instr::Nop => CtrlFlow::Proceed,
            Instr::RetPopped => {
                let ret_val = self.get_var(SP);
                CtrlFlow::Return { ret_val }
            }
            _ => unimplemented!(),
        }
    }
}
