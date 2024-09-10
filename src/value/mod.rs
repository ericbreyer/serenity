use std::fmt::Debug;

#[derive(Clone, PartialEq, Copy)]
pub enum Value {
    Integer(i64),
    UInteger(u64),
    Float(f64),
    Char(u8),
    Bool(bool),
}


impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{i}"),
            Value::UInteger(i) => write!(f, "{i}u"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Char(c) => write!(f, "{}", *c as char),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}