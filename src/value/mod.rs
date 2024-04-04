
pub mod pointer;

use core::fmt;
use std::{fmt::Debug, ops::Index, rc::Rc};

use self::pointer::Pointer;
use crate::common::runnable::Runnable;

#[derive(Clone, Copy, PartialEq)]
pub struct Word(u64);

impl From<u64> for Word {
	fn from(word: u64) -> Self {
		Word(word)
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
	pub fn new (word: u64) -> Word {
		Word(word)
	}
	pub fn to_u64(&self) -> u64 {
		self.0
	}
	pub fn to_i64(&self) -> i64 {
		self.0 as i64
	}
	pub fn to_float(&self) -> f64 {
		f64::from_bits(self.0)
	}
	pub fn to_char(&self) -> char {
		self.0 as u8 as char
	}
	pub fn to_bool(&self) -> bool {
		self.0 != 0
	}
	pub fn to_pointer(&self) -> Pointer {
		Pointer::from_word(self.0)
	}
	pub fn is_falsy(&self) -> bool {
		self.0 == 0
	}
}

#[derive(Clone, PartialEq)]
pub enum Value {
	Integer(i64),
	Float(f64),
	Char(u8),
	Bool(bool),
	Nil,
	Runnable(Runnable),
	Pointer(Pointer),
	Closure(Closure),
}

impl Value {

	pub fn to_word(&self) -> Word {
		let words = self.to_words();
		if words.len() != 1 {
			panic!("cannot convert value to word");
		}
		words[0]
	}

	pub fn to_words(&self) -> Vec<Word> {
		match self {
			Value::Integer(i) => vec![(*i as u64).into()],
			Value::Float(f) => vec![f.to_bits().into()],
			Value::Char(c) => vec![(*c as u64).into()],
			Value::Bool(b) => vec![(*b as u64).into()],
			Value::Nil => vec![0.into()],
			Value::Runnable(_) => panic!("cannot convert object to word"),
			Value::Pointer(p) => vec![p.to_word()],
			Value::Closure(c) => c.clone().to_words(),
		}
	}

	pub fn is_falsy(&self) -> bool {
		match self {
			Value::Nil => true,
			Value::Bool(b) => !b,
			Value::Integer(i) => *i == 0,
			Value::Float(f) => *f == 0.0,
			Value::Char(c) => *c == 0,
			_ => false,
		}
	}
}

impl Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Integer(i) => write!(f, "{}", i),
			Value::Float(fl) => write!(f, "{}", fl),
			Value::Char(c) => write!(f, "{}", *c as char),
			Value::Bool(b) => write!(f, "{}", b),
			Value::Nil => write!(f, "nil"),
			Value::Runnable(o) => match o {
				Runnable::Function(func) => write!(f, "{:?}", func),
				Runnable::NativeFunction(_) => write!(f, "<native fn>"),
			},
			Value::Pointer(p) => write!(f, "{:?}", p),
			Value::Closure(_c) => write!(f, "{:?}", "closure"),
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


#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    pub func : Pointer,
    pub upvalue_count: u32,
    pub upvalue_segment: Rc<Vec<Word>>
}
impl Closure {
    pub fn to_words(self) -> Vec<Word> {
        let mut words = vec![self.func.to_word()];
	words.push((self.upvalue_count as u64).into());
		let ptr = Rc::into_raw(self.upvalue_segment) ;
	words.push(
	    Word(ptr as u64)
	);
        words
    }
    pub fn new(f: Pointer, upvalue_count : u32, upvalue_segment: Rc<Vec<Word>>) -> Closure {
	Closure {
	    func: f,
	    upvalue_count,
	    upvalue_segment
	}
    }
}