use crate::value::Value;
use crate::{
    prelude::*,
};
use core::fmt::Display;
use std::collections::VecDeque;
use std::fmt::Debug;

use std::sync::atomic::AtomicUsize;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum IASMCommandType {
    PrintFloat,
    PrintInt,
    PrintUint,
    PrintBool,
    PrintNil,
    PrintPointer,
    PrintChar,
    PrintClosure,
    JumpIfFalse,
    JumpIfTrue,
    Jump,
    Return,
    LoadImediate,
    NegateFloat,
    NegateInt,
    NegateUint,
    Not,
    Load,
    PointerAdd,
    AddInt,
    AddUint,
    AddFloat,
    SubInt,
    SubUint,
    SubFloat,
    PointerSub,
    MulInt,
    MulUint,
    MulFloat,
    DivInt,
    DivUint,
    DivFloat,
    Equal,
    NotEqual,
    GreaterInt,
    GreaterUint,
    GreaterFloat,
    GreaterEqualInt,
    GreaterEqualUint,
    GreaterEqualFloat,
    LessInt,
    LessUint,
    LessFloat,
    LessEqualInt,
    LessEqualUint,
    LessEqualFloat,
    Move,
    MemCpy,
    Store,
    Call,
    PreCall,
    Push,
    Pop,
    CastIntToFloat,
    CastUintToFloat,
    CastFloatToInt,
    CastUintToInt,
    CastIntToUint,
    CastBoolToInt,
    CastBoolToUint,
    CastBoolToFloat,
    CastIntToBool,
    CastUintToBool,
    Halt,
    #[default]
    Nop,
}

impl Display for IASMCommandType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(format!("{:?}", self).as_str())
    }
}
thread_local! {
static UNIQUE_INT: AtomicUsize = AtomicUsize::new(1);
}
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct IASMRegister(pub usize, UValueType);
impl Debug for IASMRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}[{:?}]", self.0, self.1)
    }
}

impl IASMRegister {
    pub fn new(t: UValueType) -> IASMRegister {
        IASMRegister(
            UNIQUE_INT.with(|ui| ui.fetch_add(1, std::sync::atomic::Ordering::SeqCst)),
            t,
        )
    }

    pub fn nil_reg() -> IASMRegister {
        IASMRegister(0, ValueType::Nil.intern())
    }

    pub fn get_type(&self) -> UValueType {
        self.1
    }
}

impl From<IASMRegister> for usize {
    fn from(val: IASMRegister) -> usize {
        val.0
    }
}

#[derive(Clone)]
pub enum IASMParam {
    Register(IASMRegister),
    Label(IASMLabel),
    Immediate(Value),
}

impl IASMParam {
    pub fn get_register(&self) -> Result<usize, ()> {
        match self {
            IASMParam::Register(r) => Ok(r.0),
            _ => Err(()),
        }
    }
    pub fn get_label(&self) -> Result<IASMLabel, ()> {
        match self {
            IASMParam::Label(l) => Ok(l.clone()),
            _ => Err(()),
        }
    }
    pub fn get_immediate(&self) -> Result<Value, ()> {
        match self {
            IASMParam::Immediate(v) => Ok(*v),
            _ => Err(()),
        }
    }
}

impl Debug for IASMParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IASMParam::Register(r) => write!(f, "{:?}", r),
            IASMParam::Label(l) => write!(f, "{:?} ", l),
            IASMParam::Immediate(v) => write!(f, "({:?}) ", v),
        }
    }
}

#[derive(Default, Clone)]
pub struct IASMStatement {
    pub command: IASMCommandType,
    pub params: Vec<IASMParam>,
    pub line: usize,
}
impl Debug for IASMStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:>4}]  {:<15}", self.line, self.command)?;
        for param in &self.params {
            write!(f, "{:?} ", param)?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum IASMLabel {
    Resolved(usize),
    Named(SharedString),
}
impl IASMLabel {
    pub fn new(name: &str) -> IASMLabel {
        IASMLabel::Named(name.into())
    }

    pub fn new_mangled(name: &str) -> IASMLabel {
        IASMLabel::Named(
            format!(
                "{}_{}",
                name,
                UNIQUE_INT.with(|ui| ui.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
            )
            .into(),
        )
    }

    pub fn resolve(&mut self, loc: usize) {
        *self = Self::Resolved(loc)
    }
}
impl Debug for IASMLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IASMLabel::Resolved(v) => write!(f, "L{}:", v),
            IASMLabel::Named(v) => write!(f, "L{}:", v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IASMLine {
    Statement(IASMStatement),
    Label(IASMLabel),
    Comment(SharedString),
}
impl Default for IASMLine {
    fn default() -> Self {
        IASMLine::Statement(IASMStatement::default())
    }
}

#[derive(Default, Clone)]
pub struct IASMProg {
    pub text: VecDeque<IASMLine>,
}

impl Debug for IASMProg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, line) in self.text.iter().enumerate() {
            match line {
                IASMLine::Statement(s) => write!(f, "{i} {:?}\n", s),
                IASMLine::Label(l) => write!(f, "{i} {:?}\n", l),
                IASMLine::Comment(c) => write!(f, "{i} ; {}\n", c),
            }?;
        }
        Ok(())
    }
}


#[derive(Default, Clone)]
pub struct IASMProgSplit {
    pub text: VecDeque<VecDeque<IASMLine>>,
}

impl IASMProgSplit {
    pub fn new() -> IASMProgSplit {
        IASMProgSplit {
            text: VecDeque::new(),
        }
    }

    pub fn merge(&mut self) -> IASMProg {
        let mut prog = IASMProg::default();
        for t in &self.text {
            for line in t {
                prog.text.push_back(line.clone());
            }
        }
        prog
    }

    pub fn start_function(&mut self, name: &str) -> IASMLabel {
        self.text.push_front(VecDeque::new());
        let label = IASMLabel::new(name);
        self.text[0].push_back(IASMLine::Label(label.clone()));
        self.text[0].push_back(IASMLine::Comment(name.into()));
        label
    }

    pub fn end_function(&mut self) {
        let t = self.text.pop_front().unwrap();
        self.text.push_back(t);
    }

    pub fn write_statement(
        &mut self,
        command: IASMCommandType,
        params: Vec<IASMParam>,
        line: usize,
    ) {
        self.text[0].push_back(IASMLine::Statement(IASMStatement {
            command,
            params,
            line,
        }));
    }

    pub fn write_label(&mut self, label: IASMLabel) {
        self.text[0].push_back(IASMLine::Label(label));
    }

    pub fn write_constant(&mut self, value: Value, t: UValueType, line: usize) -> IASMRegister {
        let reg = IASMRegister::new(t);
        self.write_statement(
            IASMCommandType::LoadImediate,
            vec![IASMParam::Register(reg), IASMParam::Immediate(value)],
            line,
        );
        reg
    }

    pub fn write_comment(&mut self, comment: SharedString) {
        self.text[0].push_back(IASMLine::Comment(comment))
    }

    pub fn fill_break_continue(&mut self) {
        let mut break_label = IASMLabel::new("0");
        let mut continue_to_label = IASMLabel::new("0");

        for (i, line) in self.text[0].iter_mut().enumerate().rev() {
            match line {
                IASMLine::Label(IASMLabel::Named(n)) if n.starts_with("loop_end") => {
                    break_label = IASMLabel::Named(n.clone());
                }
                IASMLine::Label(IASMLabel::Named(n)) if n.starts_with("continue_to") => {
                    continue_to_label = IASMLabel::Named(n.clone());
                }
                IASMLine::Comment(s) if *s == "break".into() => {
                    *line = IASMLine::Statement(IASMStatement {
                        command: IASMCommandType::Jump,
                        params: vec![IASMParam::Label(break_label.clone())],
                        line: i,
                    })
                }
                IASMLine::Comment(s) if *s == "continue".into() => {
                    *line = IASMLine::Statement(IASMStatement {
                        command: IASMCommandType::Jump,
                        params: vec![IASMParam::Label(continue_to_label.clone())],
                        line: i,
                    })
                }
                _ => continue,
            }
        }
    }
}

impl Debug for IASMProgSplit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n")?;
        for t in &self.text {
            for line in t {
                match line {
                    IASMLine::Statement(s) => write!(f, "{:?}\n", s),
                    IASMLine::Label(l) => write!(f, "{:?}\n", l),
                    IASMLine::Comment(c) => write!(f, "; {}\n", c),
                }?;
            }
        }
        Ok(())
    }
}

// pub struct Function {
//     pub arity: usize,
//     pub asm: IASMProgSplit,
//     pub name: SharedString,
//     pub upvalue_count: usize,
//     pub return_type: UValueType,
//     pub return_size: usize,
// }

// impl Default for Function {
//     fn default() -> Self {
//         Function {
//             arity: 0,
//             asm: IASMProgSplit::default(),
//             name: "".into(),
//             upvalue_count: 0,
//             return_type: ValueType::Nil.intern(),
//             return_size: 0,
//         }
//     }
// }

// impl Debug for Function {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         if self.name.is_empty() || self.name == "script".into() {
//             return write!(f, "<script>");
//         }
//         write!(f, "Function: {}\n", self.name)?;
//         write!(f, "{:?}", self.asm)
//     }
// }

// impl PartialEq for Function {
//     fn eq(&self, other: &Self) -> bool {
//         self.arity == other.arity && self.name == other.name
//     }
// }

// impl Function {
//     pub fn new(arity: usize, name: SharedString) -> Function {
//         Function {
//             arity,
//             asm: IASMProgSplit::default(),
//             name,
//             upvalue_count: 0,
//             return_type: ValueType::Nil.intern(),
//             return_size: 0,
//         }
//     }
// }
