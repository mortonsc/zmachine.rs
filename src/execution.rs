use super::instr::parse::decode_instr;
use super::instr::Instr;
use super::text::ZStr;
use super::{StackFrame, ZMachine, SP};
use std::cmp::min;

#[derive(Debug)]
enum CtrlFlow {
    Proceed,
    Jump { offset: i16 },
    Return { ret_val: i16 },
}

impl<'a> ZMachine<'a> {
    pub const MAX_N_LOCALS: u8 = 15;

    fn execute_routine(&mut self, paddr: u16, args: &[i16]) -> Option<i16> {
        if paddr == 0 {
            return Some(0);
        }
        let byteaddr = self.routine_paddr_to_byteaddr(paddr);
        let n_locals = self.state().memory().get_byte(byteaddr);
        // TODO error handling
        if n_locals > Self::MAX_N_LOCALS {
            panic!("invalid number of local variables: {}", n_locals);
        }
        let mut sf = StackFrame {
            locals: vec![0; n_locals as usize],
            data_stack: Vec::new(),
        };
        for i in 0..min(sf.locals.len(), args.len()) {
            // it's not an error if there are more arguments than local variables
            // the extra arguments just get ignored
            sf.locals[i] = args[i];
        }
        self.call_stack.push(sf);
        let result = self.execute_until_return(byteaddr + 1);
        self.call_stack.pop();
        result
    }
    fn execute_until_return(&mut self, byteaddr: usize) -> Option<i16> {
        let mut pc = byteaddr;
        loop {
            let (next_pc, instr) = {
                let code = &self.memory[pc..];
                let (_, instr) = decode_instr(code).unwrap();
                // TODO: determine the actual next pc
                (0, instr)
            };
            let ctrl_flow = self.execute_instr(instr);
            pc = match ctrl_flow {
                CtrlFlow::Proceed => next_pc,
                CtrlFlow::Jump { offset: _ } => unimplemented!(), // TODO
                CtrlFlow::Return { ret_val } => return Some(ret_val),
            }
        }
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
