use std::{rc::Rc, fmt::Debug, cell::RefCell};

use crate::chunk::Chunk;

use crate::value::{pointer::Pointer, Value, Word};
use crate::typing::{ValueType, ValueTypeK};

pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Rc<str>,
    pub upvalue_count: usize,
    pub return_type: ValueType,
    pub return_size: usize,
}

impl Default for Function {
    fn default() -> Self {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: Rc::from(""),
            upvalue_count: 0,
            return_type: ValueTypeK::Nil.intern(),
            return_size: 0
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.is_empty() || self.name == "script".into() {
            return write!(f, "<script>");
        }
        write!(f, "<fn {}>", self.name)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.name == other.name
    }
}

impl Function {
    pub fn new(arity: usize, name: Rc<str>) -> Function {
        Function {
            arity,
            chunk: Chunk::new(),
            name,
            upvalue_count: 0,
            return_type: ValueTypeK::Nil.intern(),
            return_size: 0
        }
    }
}

#[derive(Clone)]
pub enum Runnable {
    Function(Rc<Function>),
    NativeFunction(fn(&[Word]) -> Value),
}

impl PartialEq for Runnable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Runnable::Function(f1), Runnable::Function(f2)) => f1 == f2,
            _ => false,
        }
    }

}

impl Debug for Runnable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Runnable::Function(func) => write!(f, "{:?}", func),
            Runnable::NativeFunction(_) => write!(f, "<native fn>"),
        }
    }
}