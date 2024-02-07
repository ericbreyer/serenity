use std::{rc::Rc, fmt::Debug, cell::RefCell};

use crate::chunk::Chunk;

use super::{pointer::Pointer, value_type::{ValueType, ValueTypeK}, Value, Word};

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

#[derive(Clone, PartialEq, Debug)]
pub struct Upvalue {
    pub slize: usize,
    pub closed: Vec<Word>,
    pub idx: Option<usize>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    pub func : Pointer,
    pub upvalue_count: u32,
    pub upvalues: Option<Box<Vec<Rc<RefCell<Upvalue>>>>>,
}

impl Closure {
    pub fn to_words(self) -> Vec<Word> {
        let mut words = self.func.to_words();
        words.push((self.upvalue_count as u64).into());
        match self.upvalues {
            Some(u) => words.push((Box::into_raw(u) as u64).into()),
            None => words.push(0.into()),
        }
        words
    }
    pub fn new(f: Pointer, upvalue_count : u32) -> Closure {
        if upvalue_count > 0 {
            Closure { func: f.clone(), upvalue_count: upvalue_count, upvalues: Some(Box::new(vec![])) }
        } else {
            Closure { func: f.clone(), upvalue_count: upvalue_count, upvalues: None }
        }
    }
}

#[derive(Clone)]
pub enum Object {
    String(Rc<str>),
    Function(Rc<Function>),
    NativeFunction(fn(&[Word]) -> Value),
    // Upvalue(Rc<Upvalue>),
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(s1), Object::String(s2)) => s1 == s2,
            (Object::Function(f1), Object::Function(f2)) => f1 == f2,
            _ => false,
        }
    }

}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::Function(func) => write!(f, "{:?}", func),
            Object::NativeFunction(_) => write!(f, "<native fn>"),
        }
    }
}

pub fn is_string(value: &Value) -> Option<&str> {
    match value {
        Value::Object(o) => match o {
            Object::String(s) => Some(s),
            _ => None,
        },
        _ => None,
    }
}