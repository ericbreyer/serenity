use std::{fmt::Debug, ops};

use super::number;

#[derive(Clone, Copy, PartialEq)]
pub enum Pointer {
    Local(usize),
    Upvalue(usize),
    Static(usize),
    Heap(usize),
}

impl Pointer {
    pub fn to_word(&self) -> u64 {
        // most significant two bits are to identify the type of pointer
        // 00 - local
        // 01 - upvalue
        // 10 - static
        // 11 - heap
        match self {
            Pointer::Local(n) => *n as u64,
            Pointer::Upvalue(n) => (1 << 62) | (*n as u64),
            Pointer::Static(n) => (2 << 62) | (*n as u64),
            Pointer::Heap(n) => (3 << 62) | (*n as u64),
        }
    }
}

impl ops::Add<number::Number> for Pointer {
    type Output = Self;

    fn add(self, rhs: number::Number) -> Self::Output {
        let number::Number::Integer(inc) = rhs else {
            return self;
        };
        match self {
            Pointer::Local(n) => Pointer::Local((n as i64 + inc) as usize),
            Pointer::Upvalue(n) => Pointer::Upvalue((n as i64 + inc) as usize),
            Pointer::Static(n) => Pointer::Static((n as i64 + inc) as usize),
            Pointer::Heap(n) => Pointer::Heap((n as i64 + inc) as usize),
        }
    }
}

impl Debug for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pointer::Local(i) => write!(f, "local *{}", i),
            Pointer::Upvalue(i) => write!(f, "upvalue *{}", i),
            Pointer::Static(i) => write!(f, "global *{}", i),
            Pointer::Heap(i) => write!(f, "heap *{}", i),
        }
    }
}