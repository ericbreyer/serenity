use std::{ rc::Rc, fmt::Debug };
use crate::{reg_compiler::Function, typing::{ UValueType, ValueType } };

pub struct StackVMFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: SharedString,
    pub upvalue_count: usize,
    pub return_type: UValueType,
    pub return_size: usize,
}

impl Default for StackVMFunction {
    fn default() -> Self {
        StackVMFunction {
            arity: 0,
            chunk: Chunk::new(),
            name: Rc::from(""),
            upvalue_count: 0,
            return_type: ValueType::Nil.intern(),
            return_size: 0,
        }
    }
}

impl Debug for StackVMFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.is_empty() || self.name == "script".into() {
            return write!(f, "<script>");
        }
        write!(f, "<fn {}>", self.name)
    }
}

impl PartialEq for StackVMFunction {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.name == other.name
    }
}

impl StackVMFunction {
    pub fn new(arity: usize, name: SharedString) -> StackVMFunction {
        StackVMFunction {
            arity,
            chunk: Chunk::new(),
            name,
            upvalue_count: 0,
            return_type: ValueType::Nil.intern(),
            return_size: 0,
        }
    }
}

#[derive(Clone)]
pub enum Runnable {
    StackVMFunction(Rc<StackVMFunction>),
    Function(Rc<Function>),
    // NativeFunction(fn(&mut VM, &[Word]) -> Value),
}

impl PartialEq for Runnable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Runnable::StackVMFunction(f1), Runnable::StackVMFunction(f2)) => f1 == f2,
            _ => false,
        }
    }
}

impl Debug for Runnable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Runnable::StackVMFunction(func) => write!(f, "{func:?}"),
            Runnable::Function(func) => write!(f, "{func:?}"),
            // Runnable::NativeFunction(_) => write!(f, "<native fn>"),
        }
    }
}
