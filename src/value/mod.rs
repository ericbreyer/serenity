pub mod pointer;

use core::fmt;
use std::fmt::Debug;

use self::pointer::Pointer;

#[derive(Clone, Copy, PartialEq)]
pub struct Word(pub u64);

impl From<u64> for Word {
    fn from(word: u64) -> Self {
        Word(word)
    }
}

impl From<usize> for Word {
    fn from(word: usize) -> Self {
        Word(word as u64)
    }
}

impl From<Word> for u64 {
    fn from(word: Word) -> Self {
        word.0
    }
}

impl Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.0)
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:x}", self.0)
    }
}

impl Word {
    pub fn new(word: u64) -> Word {
        Word(word)
    }
    pub fn to_u64(self) -> u64 {
        self.0
    }
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
    pub fn to_i64(self) -> i64 {
        unsafe { std::mem::transmute(self.0) }
    }
    pub fn to_float(self) -> f64 {
        f64::from_bits(self.0)
    }
    pub fn to_char(self) -> char {
        self.0 as u8 as char
    }
    pub fn to_bool(self) -> bool {
        self.0 != 0
    }
    pub fn to_pointer(self) -> Pointer {
        Pointer::from_word(self.0)
    }
    pub fn is_falsy(&self) -> bool {
        self.0 == 0
    }
}

#[derive(Clone, PartialEq, Copy)]
pub enum Value {
    Integer(i64),
    UInteger(u64),
    Float(f64),
    Char(u8),
    Bool(bool),
    Pointer(Pointer),
}

impl Value {
    pub fn to_word(&self) -> Word {
        let words = self.to_words();
        assert!(words.len() == 1, "cannot convert value to word");
        words[0]
    }

    pub fn to_words(&self) -> Vec<Word> {
        match self {
            Value::Integer(i) => vec![(*i as u64).into()],
            Value::UInteger(i) => vec![(*i as u64).into()],
            Value::Float(f) => vec![f.to_bits().into()],
            Value::Char(c) => vec![u64::from(*c).into()],
            Value::Bool(b) => vec![u64::from(*b).into()],
            Value::Pointer(p) => vec![p.to_word()],
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{i}"),
            Value::UInteger(i) => write!(f, "{i}u"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Char(c) => write!(f, "{}", *c as char),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Pointer(p) => write!(f, "{p:?}"),
        }
    }
}

// pub struct ConstantPool(Vec<Value>);

// impl ConstantPool {
//     pub fn new() -> ConstantPool {
//         ConstantPool(Vec::new())
//     }

//     pub fn write(&mut self, value: Value) -> u32 {
//         self.0.push(value);
//         (self.0.len() - 1) as u32
//     }
// }

// impl Index<usize> for ConstantPool {
//     type Output = Value;

//     fn index(&self, index: usize) -> &Value {
//         &self.0[index]
//     }
// }
