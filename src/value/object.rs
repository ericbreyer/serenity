use std::{rc::Rc, fmt::Debug, cell::RefCell};

use crate::chunk::Chunk;

use super::{pointer::Pointer, value_type::{ValueType, ValueTypeK}, Value};

pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Rc<str>,
    pub upvalue_count: usize,
    pub return_type: ValueType,
}

impl Default for Function {
    fn default() -> Self {
        Function {
            arity: 0,
            chunk: Chunk::new(),
            name: Rc::from(""),
            upvalue_count: 0,
            return_type: ValueTypeK::Nil.intern()
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
            return_type: ValueTypeK::Nil.intern()
        }
    }
}

pub struct Upvalue {
    pub index: usize,
    pub next: RefCell<Option<Rc<Upvalue>>>,
    pub closed: RefCell<Option<Value>>
}

#[derive(Clone)]
pub struct Closure {
    pub func : Pointer,
    pub upvalue_count: u32,
    pub upvalues: Vec<Rc<Upvalue>>
}

impl Closure {
    pub fn new(f: Pointer, upvalue_count : u32) -> Closure {
        Closure { func: f.clone(), upvalue_count: upvalue_count, upvalues: Vec::new() }
    }
}

#[derive(Clone)]
pub enum Object {
    String(Rc<str>),
    Function(Rc<Function>),
    NativeFunction(fn(&[Value]) -> Value),
    Closure(Closure),
}

impl Object {
    pub fn to_word(&self) -> u64 {
        match self {
            Object::String(s) => unimplemented!("String to_word"),
            Object::Function(f) => unimplemented!("Function to_word"),
            Object::NativeFunction(_) => unimplemented!("NativeFunction to_word"),
            Object::Closure(c) => unimplemented!("Closure to_word"),
        }
    }

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
            Object::Closure(c) => write!(f, "{:?}", c.func),
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