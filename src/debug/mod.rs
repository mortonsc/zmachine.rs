use super::execution::ProgramState;
use super::instr::parse::decode_instr;
use super::memory::*;
use super::text::ZStr;
use std::convert::TryFrom;
use std::io::{stdin, stdout, Write};

mod parse;

pub use parse::cmd as parse_cmd;

#[derive(Debug)]
pub enum Command {
    Echo(NumericExpr),
    Decode(BytesExpr),
    ExecInstr(BytesExpr),
    PrintZStr(BytesExpr),
    Quit,
}

#[derive(Debug)]
pub enum NumericExpr {
    Literal(i64),
    IndirectAddrB(Box<Self>),
    IndirectAddrW(Box<Self>),
    VarAccess(Box<Self>),
    History(usize),
    MostRecent,
}

#[derive(Debug, Clone, Copy)]
enum ShallowNumericExpr {
    Literal(i64),
    IndirectAddrB(usize),
    IndirectAddrW(usize),
    VarAccess(u8),
    History(usize),
    MostRecent,
}

#[derive(Debug)]
pub enum BytesExpr {
    Literal(Vec<u8>),
    IndirectAddr(NumericExpr),
    IndirectPAddrR(NumericExpr),
    IndirectPAddrS(NumericExpr),
}

pub struct Context<'a> {
    history: Vec<Option<i64>>,
    ps: ProgramState<'a>,
}

impl<'a> Context<'a> {
    pub fn new(ps: ProgramState<'a>) -> Self {
        Self {
            history: Vec::new(),
            ps,
        }
    }

    pub fn repl(&mut self) {
        let mut done = false;
        while !done {
            let is_done = self.repl_once();
            done = matches!(is_done, Some(true));
        }
    }

    pub fn prompt_for_cmd(&mut self) -> Option<Command> {
        print!("In [{}]: ", self.history.len());
        let mut input = String::new();
        stdout().flush().ok()?;
        stdin().read_line(&mut input).ok()?;
        let (_, cmd) = parse_cmd(&input[..]).ok()?;
        Some(cmd)
    }

    pub fn repl_once(&mut self) -> Option<bool> {
        let cmd = match self.prompt_for_cmd() {
            Some(cmd) => cmd,
            None => {
                println!();
                return None;
            }
        };
        if matches!(cmd, Command::Quit) {
            return Some(true);
        }
        let result = self.do_cmd(cmd);
        match result {
            Some(val) => {
                println!("Out [{}]: {}", self.history.len(), val);
                self.history.push(result);
            }
            None => {
                println!("Err")
            }
        }
        Some(false)
    }

    pub fn do_cmd(&mut self, cmd: Command) -> Option<i64> {
        match cmd {
            Command::Echo(exp) => self.value_as_i64(&exp),
            Command::Decode(exp) => {
                let bytes = self.get_slice(&exp)?;
                let (_, instr) = decode_instr(bytes).ok()?;
                println!("{:?}", instr);
                Some(0)
            }
            Command::ExecInstr(exp) => {
                let bytes = self.get_slice(&exp)?;
                let (_, instr) = decode_instr(bytes).ok()?;
                println!("{:?}", instr);
                let ctrl_flow = self.ps.execute_instr(instr).ok()?;
                println!("{:?}", ctrl_flow);
                Some(0)
            }
            Command::PrintZStr(exp) => {
                let bytes = self.get_slice(&exp)?;
                let zstr = ZStr::from(bytes);
                let unicode: String = self.ps.mm.text_engine().zstr_to_unicode(zstr).collect();
                println!("{}", unicode);
                Some(0)
            }
            Command::Quit => Some(0),
        }
    }

    fn value_as_i64(&self, exp: &NumericExpr) -> Option<i64> {
        let exp = self.make_shallow(exp)?;
        self.value_as_i64_h(exp)
    }

    fn value_as_i64_h(&self, exp: ShallowNumericExpr) -> Option<i64> {
        match exp {
            ShallowNumericExpr::Literal(val) => Some(val),
            ShallowNumericExpr::IndirectAddrB(addr) => self.ps.mm.read_byte(addr).map(|n| n as i64),
            ShallowNumericExpr::IndirectAddrW(addr) => self.ps.mm.read_word(addr).map(|n| n as i64),
            ShallowNumericExpr::VarAccess(var) => {
                self.ps.get_var_by_ref(var).ok().map(|v| v as i64)
            }
            ShallowNumericExpr::History(idx) => *self.history.get(idx - 1)?,
            ShallowNumericExpr::MostRecent => *self.history.last()?,
        }
    }

    fn value_as_u8(&self, exp: &NumericExpr) -> Option<u8> {
        // can't think of a case where this isn't right
        u8::try_from(self.value_as_i64(exp)?).ok()
    }

    fn value_as_byteaddr(&self, exp: &NumericExpr) -> Option<usize> {
        let exp = self.make_shallow(exp)?;
        self.value_as_byteaddr_h(exp)
    }

    fn value_as_byteaddr_h(&self, exp: ShallowNumericExpr) -> Option<usize> {
        match exp {
            // special cases to treat these values as unsigned
            ShallowNumericExpr::IndirectAddrW(addr) => {
                self.ps.mm.read_word(addr).map(|n| n as u16 as usize)
            }
            ShallowNumericExpr::VarAccess(var) => {
                let val = self.ps.get_var_by_ref(var).ok()?;
                Some(val as u16 as usize)
            }
            _ => usize::try_from(self.value_as_i64_h(exp)?).ok(),
        }
    }

    fn make_shallow(&self, exp: &NumericExpr) -> Option<ShallowNumericExpr> {
        match exp {
            NumericExpr::Literal(n) => Some(ShallowNumericExpr::Literal(*n)),
            NumericExpr::IndirectAddrB(inner) => {
                let addr = self.value_as_byteaddr(inner)?;
                Some(ShallowNumericExpr::IndirectAddrB(addr))
            }
            NumericExpr::IndirectAddrW(inner) => {
                let addr = self.value_as_byteaddr(inner)?;
                Some(ShallowNumericExpr::IndirectAddrW(addr))
            }
            NumericExpr::VarAccess(inner) => {
                let var = self.value_as_u8(inner)?;
                Some(ShallowNumericExpr::VarAccess(var))
            }
            NumericExpr::History(idx) => Some(ShallowNumericExpr::History(*idx)),
            NumericExpr::MostRecent => Some(ShallowNumericExpr::MostRecent),
        }
    }

    fn get_slice<'b>(&'b self, expr: &'b BytesExpr) -> Option<&'b [u8]> {
        match expr {
            BytesExpr::Literal(bytes) => Some(&bytes[..]),
            BytesExpr::IndirectAddr(addr_expr) => {
                let byteaddr = self.value_as_byteaddr(addr_expr)?;
                Some(&self.ps.mm.contents()[byteaddr..])
            }
            BytesExpr::IndirectPAddrR(addr_expr) => {
                let paddr = self.value_as_byteaddr(addr_expr)?;
                let byteaddr = self.ps.mm.routine_paddr_to_byteaddr(paddr);
                Some(&self.ps.mm.contents()[byteaddr..])
            }
            BytesExpr::IndirectPAddrS(addr_expr) => {
                let paddr = self.value_as_byteaddr(addr_expr)?;
                let byteaddr = self.ps.mm.string_paddr_to_byteaddr(paddr);
                Some(&self.ps.mm.contents()[byteaddr..])
            }
        }
    }
}
