use std::{fmt::Debug, ops::{self}};

use super::Word;

#[derive(Clone, Copy, PartialEq)]
pub enum Pointer {
    Local(usize),
    Upvalue(usize, usize),
    Static(usize),
    Heap(usize),
    Function(usize),
}

impl Pointer {
    const LOCAL_MASK: u64 = 0x1FFFFFFFFFFFFFFF;
    const UPPER_MASK: u64 = 0x1FFFFFFFFFFFFFFF;
    const LOWER_MASK: u64 = 0x1FFFFFFFFFFFFFFF;

    pub fn to_words(&self) -> Vec<Word> {
        // most significant two bits are to identify the type of pointer

        // match self {
        //     Pointer::Local(n) => (*n as u64).into(),
        //     Pointer::Upvalue(n) => ((1 << 61) | (*n as u64)).into(),
        //     Pointer::Static(n) => ((2 << 61) | (*n as u64)).into(),
        //     Pointer::Heap(n) => ((3 << 61) | (*n as u64)).into(),
        //     Pointer::Function(n) => ((4 << 61) | (*n as u64)).into(),
        // }
        vec![Word::new(match self {
            Pointer::Local(n) => *n as u64,
            Pointer::Upvalue(n, o) => (1 << 61) | (*n as u64) << 4 | *o as u64,
            Pointer::Static(n) => (2 << 61) | *n as u64,
            Pointer::Heap(n) => (3 << 61) | *n as u64,
            Pointer::Function(n) => (4 << 61) | *n as u64,
        })]
    }

    pub fn from_word(word: u64) -> Pointer {
        match word >> 61 {
            0 => Pointer::Local((word & 0x1FFFFFFFFFFFFFFF) as usize),
            1 => Pointer::Upvalue(((word & 0x1FFFFFFFFFFFFFFF) >> 4) as usize, (word & 0xF) as usize),
            2 => Pointer::Static((word & 0x1FFFFFFFFFFFFFFF) as usize),
            3 => Pointer::Heap((word & 0x1FFFFFFFFFFFFFFF) as usize),
            4 => Pointer::Function((word & 0x1FFFFFFFFFFFFFFF) as usize),
            _ => panic!("invalid pointer type, {:x}", word >> 61),
        }
    }
}

impl ops::Add<i64> for Pointer {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        let inc = rhs as i64;
        match self {
            Pointer::Local(n) => Pointer::Local((n as i64 + inc) as usize),
            Pointer::Upvalue(n, o) => Pointer::Upvalue(n as usize, (o as i64 + inc) as usize),
            Pointer::Static(n) => Pointer::Static((n as i64 + inc) as usize),
            Pointer::Heap(n) => Pointer::Heap((n as i64 + inc) as usize),
            Pointer::Function(n) => Pointer::Function((n as i64 + inc) as usize),
        }
    }
}

impl Debug for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pointer::Local(i) => write!(f, "local *{:x}", i),
            Pointer::Upvalue(i, o) => write!(f, "upvalue *{:x} *{:x}", i, o),
            Pointer::Static(i) => write!(f, "global *{:x}", i),
            Pointer::Heap(i) => write!(f, "heap *{:x}", i),
            Pointer::Function(i) => write!(f, "function *{:x}", i),
        }
    }
}