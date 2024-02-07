pub mod object;
pub mod pointer;
pub mod value_type;

use std::{fmt::Debug, ops::Index};

use self::{object::{Closure, Object}, pointer::Pointer};

#[derive(Clone, Copy, PartialEq, Debug)]
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
	Object(Object),
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
		// match self {
		// 	Value::Integer(i) => (*i as u64).into(),
		// 	Value::Float(f) => f.to_bits().into(),
		// 	Value::Char(c) => (*c as u64).into(),
		// 	Value::Bool(b) => (*b as u64).into(),
		// 	Value::Nil => 0.into(),
		// 	Value::Object(_) => panic!("cannot convert object to word"),
		// 	Value::Pointer(p) => p.to_word(),
		// }
		match self {
			Value::Integer(i) => vec![(*i as u64).into()],
			Value::Float(f) => vec![f.to_bits().into()],
			Value::Char(c) => vec![(*c as u64).into()],
			Value::Bool(b) => vec![(*b as u64).into()],
			Value::Nil => vec![0.into()],
			Value::Object(_) => panic!("cannot convert object to word"),
			Value::Pointer(p) => p.to_words(),
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
			Value::Object(o) => match o {
				Object::String(s) => write!(f, "'{}'", s),
				Object::Function(func) => write!(f, "{:?}", func),
				Object::NativeFunction(_) => write!(f, "<native fn>"),
				// Object::Upvalue(_) => write!(f, "<upvalue>"),
			},
			Value::Pointer(p) => write!(f, "{:?}", p),
			Value::Closure(c) => write!(f, "{:?}", "closure"),
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
