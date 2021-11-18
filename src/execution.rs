use super::debug;
use super::instr::parse::decode_instr;
use super::instr::{BranchData, BranchDst, Instr, Operand, SP};
use super::text::ZStr;
use super::ZMachine;

#[derive(Debug)]
pub struct StackFrame {
    ret_addr: usize,
    ret_dst: Option<u8>,
    n_args: usize,
    locals: Vec<i16>,
    data_stack: Vec<i16>,
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    NotImplemented,
    DivByZero,
    ForbiddenInInterrupt,
    EmptyStack,
    IllegalReturn,
    InvalidVarRef(u16),
    InvalidLocal(u8),
    InvalidNumLocals(u8),
    InvalidStackFrame(usize),
    DecodingFailed, // TODO: pass some data along
    ExecutionFinished,
}

pub type ExeResult<T> = Result<T, ErrorKind>;

#[derive(Debug, Clone)]
pub enum CtrlFlow {
    Proceed,
    Jump {
        offset: i16,
    },
    Call {
        byteaddr: usize,
        ret_dst: Option<u8>,
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

impl CtrlFlow {
    fn branch(cond: bool, bdata: BranchData) -> Self {
        if cond ^ bdata.invert_cond {
            match bdata.dst {
                BranchDst::Offset(offset) => Self::Jump { offset },
                BranchDst::Return(false) => Self::Return { ret_val: 0 },
                BranchDst::Return(true) => Self::Return { ret_val: 1 },
            }
        } else {
            Self::Proceed
        }
    }
}

pub struct ProgramState<'a> {
    pub zm: &'a mut ZMachine<'a>,
    pub pc: Option<usize>,
    pub call_stack: Vec<StackFrame>,
}

impl<'a> ZMachine<'a> {
    pub fn initial_program_state(&'a mut self) -> ProgramState {
        let initial_frame = StackFrame {
            ret_addr: 0,
            ret_dst: None,
            n_args: 0,
            locals: Vec::new(),
            data_stack: Vec::new(),
        };
        let pc = self.read_word(0x06) as usize;
        ProgramState {
            zm: self,
            pc: Some(pc),
            call_stack: vec![initial_frame],
        }
    }
}

impl<'a> ProgramState<'a> {
    pub const MAX_N_LOCALS: u8 = 15;

    pub fn debug_context(self) -> debug::Context<'a> {
        debug::Context::new(self)
    }

    fn stack_frame(&self) -> &StackFrame {
        self.call_stack.last().expect("Call stack is empty")
    }

    fn stack_frame_mut(&mut self) -> &mut StackFrame {
        // panic as we shouldn't allow the program to pop the last frame
        self.call_stack.last_mut().expect("Call stack is empty")
    }

    // mutable because variable 0x00 pops the stack
    fn operand_as_value(&mut self, op: Operand) -> ExeResult<i16> {
        match op {
            Operand::LargeConst(val) => Ok(val),
            Operand::SmallConst(val) => Ok(val as i16),
            Operand::Variable(var) => self.get_var_by_value(var),
        }
    }

    fn operand_as_var_by_ref(&mut self, op: Operand) -> ExeResult<u8> {
        let var_by_ref = self.operand_as_value(op)? as u16;
        if var_by_ref > (u8::MAX as u16) {
            Err(ErrorKind::InvalidVarRef(var_by_ref))
        } else {
            Ok(var_by_ref as u8)
        }
    }

    fn operand_as_byteaddr(&mut self, op: Operand) -> ExeResult<usize> {
        self.operand_as_value(op).map(|op| op as u16 as usize)
    }

    fn operand_as_routine_paddr(&mut self, op: Operand) -> ExeResult<usize> {
        self.operand_as_value(op)
            .map(|op| self.zm.routine_paddr_to_byteaddr(op as u16 as usize))
    }

    fn operand_as_string_paddr(&mut self, op: Operand) -> ExeResult<usize> {
        self.operand_as_value(op)
            .map(|op| self.zm.string_paddr_to_byteaddr(op as u16 as usize))
    }

    // mutable because variable 0x00 pops the stack
    fn get_var_by_value(&mut self, var: u8) -> ExeResult<i16> {
        if var == SP {
            self.stack_frame_mut()
                .data_stack
                .pop()
                .ok_or(ErrorKind::EmptyStack)
        } else {
            self.get_var_by_ref(var)
        }
    }

    fn set_var_by_value(&mut self, var: u8, val: i16) -> ExeResult<()> {
        if var == SP {
            self.stack_frame_mut().data_stack.push(val);
            Ok(())
        } else {
            self.set_var_by_ref(var, val)
        }
    }

    pub fn get_var_by_ref(&self, var: u8) -> ExeResult<i16> {
        match var {
            0x00 => self
                .stack_frame()
                .data_stack
                .last()
                .copied()
                .ok_or(ErrorKind::EmptyStack),
            0x01..=0x0f => self
                .stack_frame()
                .locals
                .get((var - 1) as usize)
                .copied()
                .ok_or(ErrorKind::InvalidLocal(var)),
            0x10..=0xff => Ok(self.zm.read_word(self.zm.byteaddr_of_global(var)) as i16),
        }
    }

    fn set_var_by_ref(&mut self, var: u8, val: i16) -> ExeResult<()> {
        match var {
            0x00 => {
                *self
                    .stack_frame_mut()
                    .data_stack
                    .last_mut()
                    .ok_or(ErrorKind::EmptyStack)? = val;
            }
            0x01..=0x0f => {
                *self
                    .stack_frame_mut()
                    .locals
                    .get_mut((var - 1) as usize)
                    .ok_or(ErrorKind::InvalidLocal(var))? = val;
            }
            0x10..=0xff => {
                self.zm
                    .write_word(self.zm.byteaddr_of_global(var), val as u16);
            }
        };
        Ok(())
    }

    pub fn step(&mut self) -> ExeResult<()> {
        let pc = self.pc.ok_or(ErrorKind::ExecutionFinished)?;
        self.pc = self.step_at(pc)?;
        Ok(())
    }

    // returns the pc of the next instruction to execute
    // or None if this instruction was a quit
    pub fn step_at(&mut self, pc: usize) -> ExeResult<Option<usize>> {
        let code = &self.zm.memory[pc..];
        // TODO: preserve data about the encoding error
        let (new_code, instr) = decode_instr(code).map_err(|_| ErrorKind::DecodingFailed)?;
        let instr_len = code.len() - new_code.len();
        let next_pc = pc + instr_len;
        let ctrl_flow = self.execute_instr(instr)?;
        match ctrl_flow {
            CtrlFlow::Proceed => Ok(Some(next_pc)),
            CtrlFlow::Jump { offset } => {
                let target = (next_pc as i64) + (offset as i64) - 2;
                if target < 0 {
                    panic!("out of bounds branch");
                }
                Ok(Some(target as usize))
            }
            CtrlFlow::Call {
                byteaddr,
                ret_dst,
                args,
            } => self
                .call_routine(byteaddr, next_pc, ret_dst, args)
                .map(|pc| Some(pc)),
            CtrlFlow::Return { ret_val } => self.return_from_routine(ret_val).map(|pc| Some(pc)),
            CtrlFlow::Throw {
                ret_val,
                catch_frame,
            } => {
                if catch_frame >= self.call_stack.len() {
                    Err(ErrorKind::InvalidStackFrame(catch_frame))
                } else {
                    // "[throw] returns as if from the routine which executed the catch which found this stack frame value.
                    // truncate the call stack so that the requested stack frame is on top,
                    // then return like normal
                    self.call_stack.truncate(catch_frame + 1);
                    self.return_from_routine(ret_val).map(|pc| Some(pc))
                }
            }
            CtrlFlow::Quit => Ok(None),
        }
    }

    fn call_routine(
        &mut self,
        byteaddr: usize,
        ret_addr: usize,
        ret_dst: Option<u8>,
        args: Vec<i16>,
    ) -> ExeResult<usize> {
        let mut new_frame = StackFrame {
            ret_addr,
            ret_dst,
            n_args: args.len(),
            locals: args,
            data_stack: Vec::new(),
        };
        let n_locals = self.zm.memory_map().file().get_byte(byteaddr);
        // TODO error handling
        if n_locals > Self::MAX_N_LOCALS {
            return Err(ErrorKind::InvalidNumLocals(n_locals));
        }
        new_frame.locals.resize(n_locals as usize, 0);
        self.call_stack.push(new_frame);
        Ok(byteaddr + 1)
    }

    fn return_from_routine(&mut self, ret_val: i16) -> ExeResult<usize> {
        if self.call_stack.len() == 1 {
            return Err(ErrorKind::IllegalReturn);
        }
        let frame = self.stack_frame();
        let ret_addr = frame.ret_addr;
        let ret_dst = frame.ret_dst;
        self.call_stack.pop();
        // important to do this after popping the call stack
        // so we're setting variables of the caller not the callee
        if let Some(dst) = ret_dst {
            self.set_var_by_value(dst, ret_val)?;
        }
        Ok(ret_addr)
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

    pub fn execute_instr(&mut self, instr: Instr) -> ExeResult<CtrlFlow> {
        match instr {
            Instr::RTrue => Ok(CtrlFlow::Return { ret_val: 1 }),
            Instr::RFalse => Ok(CtrlFlow::Return { ret_val: 0 }),
            Instr::Print { ztext } => {
                self.print_zstr(ZStr::from(&ztext[..]));
                Ok(CtrlFlow::Proceed)
            }
            Instr::PrintRet { ztext } => {
                self.print_zstr(ZStr::from(&ztext[..]));
                self.print_newline();
                Ok(CtrlFlow::Return { ret_val: 1 })
            }
            Instr::Nop => Ok(CtrlFlow::Proceed),
            Instr::RetPopped => {
                let ret_val = self.get_var_by_value(SP)?;
                Ok(CtrlFlow::Return { ret_val })
            }
            Instr::Catch { dst } => {
                let frame_num = (self.call_stack.len() - 1) as u16;
                self.set_var_by_value(dst, frame_num as i16)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::Quit => Ok(CtrlFlow::Quit),
            Instr::NewLine => {
                self.print_newline();
                Ok(CtrlFlow::Proceed)
            }
            Instr::Piracy { bdata } => Ok(CtrlFlow::branch(true, bdata)),
            Instr::Verify { bdata } => {
                let success = true; // TODO: verification routine
                Ok(CtrlFlow::branch(success, bdata))
            }
            Instr::JZ { a, bdata } => {
                let a = self.operand_as_value(a)?;
                Ok(CtrlFlow::branch(a == 0, bdata))
            }
            Instr::GetSibling { obj_id, dst, bdata } => {
                let obj_id = self.operand_as_value(obj_id)? as u16;
                let sib_id = self.zm.memory_map().objects().by_id(obj_id).sibling_id();
                self.set_var_by_value(dst, sib_id as i16)?;
                Ok(CtrlFlow::branch(sib_id != 0, bdata))
            }
            Instr::GetChild { obj_id, dst, bdata } => {
                let obj_id = self.operand_as_value(obj_id)? as u16;
                let child_id = self.zm.memory_map().objects().by_id(obj_id).child_id();
                self.set_var_by_value(dst, child_id as i16)?;
                Ok(CtrlFlow::branch(child_id != 0, bdata))
            }
            Instr::GetParent { obj_id, dst } => {
                let obj_id = self.operand_as_value(obj_id)? as u16;
                let parent_id = self.zm.memory_map().objects().by_id(obj_id).parent_id();
                self.set_var_by_value(dst, parent_id as i16)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::Inc { var_by_ref } => {
                let var_by_ref = self.operand_as_var_by_ref(var_by_ref)?;
                let new_val = self.get_var_by_ref(var_by_ref)? + 1;
                self.set_var_by_ref(var_by_ref, new_val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::Dec { var_by_ref } => {
                let var_by_ref = self.operand_as_var_by_ref(var_by_ref)?;
                let new_val = self.get_var_by_ref(var_by_ref)? - 1;
                self.set_var_by_ref(var_by_ref, new_val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::PrintAddr { zstr_byteaddr } => {
                let zstr_byteaddr = self.operand_as_byteaddr(zstr_byteaddr)?;
                self.print_zstr(ZStr::from(&self.zm.memory[zstr_byteaddr..]));
                Ok(CtrlFlow::Proceed)
            }
            Instr::RemoveObj { obj_id } => {
                let obj_id = self.operand_as_value(obj_id)? as u16;
                let writes = self
                    .zm
                    .memory_map()
                    .objects()
                    .by_id(obj_id)
                    .remove_from_parent();
                self.zm.apply_all(&writes[..]);
                Ok(CtrlFlow::Proceed)
            }
            Instr::PrintObj { obj_id } => {
                let obj_id = self.operand_as_value(obj_id)? as u16;
                let name = self.zm.memory_map().objects().by_id(obj_id).short_name();
                self.print_zstr(name);
                Ok(CtrlFlow::Proceed)
            }
            Instr::Ret { val } => Ok(CtrlFlow::Return {
                ret_val: self.operand_as_value(val)?,
            }),
            Instr::Jump { offset } => Ok(CtrlFlow::Jump {
                offset: self.operand_as_value(offset)?,
            }),
            Instr::PrintPAddr { zstr_paddr } => {
                let byteaddr = self.operand_as_string_paddr(zstr_paddr)?;
                self.print_zstr(ZStr::from(&self.zm.memory[byteaddr..]));
                Ok(CtrlFlow::Proceed)
            }
            Instr::Load { var_by_ref, dst } => {
                let var_by_ref = self.operand_as_var_by_ref(var_by_ref)?;
                let val = self.get_var_by_ref(var_by_ref)?;
                self.set_var_by_value(dst, val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::Call1S { routine_paddr, dst } => {
                self.routine_call_op(routine_paddr, Some(dst), Vec::new())
            }
            Instr::Call1N { routine_paddr } => {
                self.routine_call_op(routine_paddr, None, Vec::new())
            }

            Instr::JE { a, others, bdata } => {
                let a = self.operand_as_value(a)?;
                let mut equal = false;
                for op in others {
                    let op = self.operand_as_value(op)?;
                    if op == a {
                        // don't break because we want to check the validity
                        // of the other operands
                        equal = true;
                    }
                }
                Ok(CtrlFlow::branch(equal, bdata))
            }
            Instr::JL { a, b, bdata } => {
                let a = self.operand_as_value(a)?;
                let b = self.operand_as_value(b)?;
                Ok(CtrlFlow::branch(a < b, bdata))
            }
            Instr::JG { a, b, bdata } => {
                let a = self.operand_as_value(a)?;
                let b = self.operand_as_value(b)?;
                Ok(CtrlFlow::branch(a > b, bdata))
            }
            Instr::DecChk {
                var_by_ref,
                cmp_val,
                bdata,
            } => {
                let var = self.operand_as_var_by_ref(var_by_ref)?;
                let cmp_val = self.operand_as_value(cmp_val)?;
                let decremented = self.get_var_by_ref(var)? - 1;
                self.set_var_by_ref(var, decremented)?;
                Ok(CtrlFlow::branch(decremented < cmp_val, bdata))
            }
            Instr::IncChk {
                var_by_ref,
                cmp_val,
                bdata,
            } => {
                let var = self.operand_as_var_by_ref(var_by_ref)?;
                let cmp_val = self.operand_as_value(cmp_val)?;
                let incremented = self.get_var_by_ref(var)? + 1;
                self.set_var_by_ref(var, incremented)?;
                Ok(CtrlFlow::branch(incremented > cmp_val, bdata))
            }
            Instr::JIn { obj1, obj2, bdata } => {
                let obj1 = self.operand_as_value(obj1)? as u16;
                let obj2 = self.operand_as_value(obj2)? as u16;
                let is_child = self.zm.memory_map().objects().by_id(obj1).parent_id() == obj2;
                Ok(CtrlFlow::branch(is_child, bdata))
            }
            Instr::Test {
                bitmap,
                flags,
                bdata,
            } => {
                let bitmap = self.operand_as_value(bitmap)? as u16;
                let flags = self.operand_as_value(flags)? as u16;
                Ok(CtrlFlow::branch(bitmap & flags == flags, bdata))
            }
            Instr::Or { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a | b),
            Instr::And { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a & b),
            // TODO: probably supposed to use explicit wrapping arithmetic
            Instr::Add { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a + b),
            Instr::Sub { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a - b),
            Instr::Mul { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a * b),
            // TODO: deal with divide by zero
            Instr::Div { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a / b),
            Instr::Mod { a, b, dst } => self.arithmetic_op(a, b, dst, |a, b| a % b),
            Instr::TestAttr {
                obj_id,
                attr,
                bdata,
            } => {
                let obj_id = self.operand_as_value(obj_id)? as u16;
                let attr = self.operand_as_value(attr)? as u16 as usize;
                let attr_set = self.zm.memory_map().objects().by_id(obj_id).test_attr(attr);
                Ok(CtrlFlow::branch(attr_set, bdata))
            }
            Instr::Store { var_by_ref, val } => {
                let var_by_ref = self.operand_as_var_by_ref(var_by_ref)?;
                let val = self.operand_as_value(val)?;
                self.set_var_by_ref(var_by_ref, val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::LoadW {
                array,
                word_index,
                dst,
            } => {
                let array = self.operand_as_byteaddr(array)?;
                let byte_index = 2 * self.operand_as_byteaddr(word_index)?;
                let val = self.zm.read_word(array + byte_index) as i16;
                self.set_var_by_value(dst, val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::LoadB {
                array,
                byte_index,
                dst,
            } => {
                let array = self.operand_as_byteaddr(array)?;
                let byte_index = self.operand_as_byteaddr(byte_index)?;
                // TODO: not clear whether this should be sign-extended?
                let val = self.zm.read_byte(array + byte_index) as i8 as i16;
                self.set_var_by_value(dst, val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::Call2S {
                routine_paddr,
                arg1,
                dst,
            } => self.routine_call_op(routine_paddr, Some(dst), vec![arg1]),
            Instr::Call2N {
                routine_paddr,
                arg1,
            } => self.routine_call_op(routine_paddr, None, vec![arg1]),
            Instr::CallVS {
                routine_paddr,
                args,
                dst,
            } => self.routine_call_op(routine_paddr, Some(dst), args),
            Instr::CallVN {
                routine_paddr,
                args,
            } => self.routine_call_op(routine_paddr, None, args),
            Instr::Pull { var_by_ref } => {
                let var_by_ref = self.operand_as_var_by_ref(var_by_ref)?;
                let val = self.get_var_by_value(SP)?;
                self.set_var_by_ref(var_by_ref, val)?;
                Ok(CtrlFlow::Proceed)
            }
            Instr::Throw {
                ret_val,
                catch_frame,
            } => {
                let ret_val = self.operand_as_value(ret_val)?;
                let catch_frame = self.operand_as_value(catch_frame)? as u16 as usize;
                Ok(CtrlFlow::Throw {
                    ret_val,
                    catch_frame,
                })
            }
            _ => Err(ErrorKind::NotImplemented),
        }
    }

    fn routine_call_op(
        &mut self,
        paddr: Operand,
        ret_dst: Option<u8>,
        args: Vec<Operand>,
    ) -> ExeResult<CtrlFlow> {
        let byteaddr = self.operand_as_routine_paddr(paddr)?;
        let mut arg_vals: Vec<i16> = Vec::new();
        for arg in args {
            arg_vals.push(self.operand_as_value(arg)?)
        }
        Ok(CtrlFlow::Call {
            byteaddr,
            ret_dst,
            args: arg_vals,
        })
    }

    fn arithmetic_op(
        &mut self,
        a: Operand,
        b: Operand,
        dst: u8,
        f: impl Fn(i16, i16) -> i16,
    ) -> ExeResult<CtrlFlow> {
        let a = self.operand_as_value(a)?;
        let b = self.operand_as_value(b)?;
        self.set_var_by_value(dst, f(a, b))?;
        Ok(CtrlFlow::Proceed)
    }
}
