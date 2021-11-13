use super::instr::Instr;
use super::{CtrlFlow, ZMachine, SP};

impl<'a> ZMachine<'a> {
    fn execute(&mut self, instr: Instr) -> CtrlFlow {
        match instr {
            Instr::RTrue => CtrlFlow::Return { ret_val: 1 },
            Instr::RFalse => CtrlFlow::Return { ret_val: 0 },
            Instr::Print { zstr } => {
                self.print_zstr(zstr);
                CtrlFlow::Proceed
            }
            Instr::PrintRet { zstr } => {
                self.print_zstr(zstr);
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
