pub mod number;
pub mod object;
pub mod pointer;
pub mod value_type;

use std::{fmt::Debug, ops::Index};

use self::{number::Number, object::Object, pointer::Pointer};

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(Number),
    Bool(bool),
    Nil,
    Object(Object),
    Pointer(Pointer),
}

impl Value {

    pub fn to_word(&self) -> u64 {
        match self {
            Value::Number(n) => match n {
                Number::Float(f) => f.to_bits() as u64,
                Number::Integer(i) => *i as u64,
            },
            Value::Bool(b) => *b as u64,
            Value::Nil => 0,
            Value::Object(o) => o.to_word(),
            Value::Pointer(p) => p.to_word(),
        }
    }

    pub fn is_falsy(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(b) => !b,
            Value::Number(n) => match n {
                Number::Float(f) => *f == 0.0,
                Number::Integer(i) => *i == 0,
            },
            _ => false,
        }
    }

    // pub fn type_of(&self) -> ValueType {
    //     match self {
    //         Value::Number(n) => match n {
    //             Number::Float(_) => ValueType::Float,
    //             Number::Integer(_) => ValueType::Integer,
    //         },
    //         Value::Bool(_) => ValueType::Bool,
    //         Value::Nil => ValueType::Nil,
    //         Value::Object(o) => match o {
    //             Object::String(_) => ValueType::String,
    //             Object::Function(_) => ValueType::Function(Box::new(ValueType::Nil)),
    //             Object::NativeFunction(_) => ValueType::NativeFunction,
    //             Object::Closure(_) => todo!(),
    //         },
    //         Value::Pointer(_) => ValueType::Pointer(Rc::new(ValueType::Nil)),
    //     }
    // }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{:?}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Object(o) => match o {
                Object::String(s) => write!(f, "'{}'", s),
                Object::Function(func) => write!(f, "{:?}", func),
                Object::NativeFunction(_) => write!(f, "<native fn>"),
                Object::Closure(c) => write!(f, "{:?}", c.func),
            },
            Value::Pointer(p) => write!(f, "{:?}", p),
        }
    }
}

pub struct ConstantPool(Vec<Value>);

impl ConstantPool {
    pub fn new() -> ConstantPool {
        ConstantPool(Vec::new())
    }

    pub fn write(&mut self, value: Value) -> u32 {
        self.0.push(value);
        (self.0.len() - 1) as u32
    }
}

impl Index<usize> for ConstantPool {
    type Output = Value;

    fn index(&self, index: usize) -> &Value {
        &self.0[index]
    }
}
