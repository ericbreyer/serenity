use std::{fmt::Debug, ops::{self}};

use super::Word;

#[derive(Clone, Copy, PartialEq)]
pub enum Pointer {
    Local(usize),
    Static(usize),
    Heap(usize),
    Function(usize),
}

impl Pointer {

    pub fn to_word(self) -> Word {
        // most significant two bits are to identify the type of pointer

        Word::new(match self {
            Pointer::Local(n) => n as u64,
            Pointer::Static(n) => (1 << 62) | n as u64,
            Pointer::Heap(n) => (2 << 62) | n as u64,
            Pointer::Function(n) => (3 << 62) | n as u64,
        })
    }

    pub fn from_word(word: u64) -> Pointer {
        match word >> 62 {
            0 => Pointer::Local((word & 0x3FFF_FFFF_FFFF_FFFF) as usize),
            1 => Pointer::Static((word & 0x3FFF_FFFF_FFFF_FFFF) as usize),
            2 => Pointer::Heap((word & 0x3FFF_FFFF_FFFF_FFFF) as usize),
            3 => Pointer::Function((word & 0x3FFF_FFFF_FFFF_FFFF) as usize),
            _ => panic!("invalid pointer type, {:x}", word >> 62),
        }
    }
}

impl ops::Add<i64> for Pointer {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        let inc = rhs;
        match self {
            Pointer::Local(n) => Pointer::Local((n as i64 + inc) as usize),
            Pointer::Static(n) => Pointer::Static((n as i64 + inc) as usize),
            Pointer::Heap(n) => Pointer::Heap((n as i64 + inc) as usize),
            Pointer::Function(n) => Pointer::Function((n as i64 + inc) as usize),
        }
    }
}

impl Debug for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pointer::Local(i) => write!(f, "local *{i:x}"),
            Pointer::Static(i) => write!(f, "global *{i:x}"),
            Pointer::Heap(i) => write!(f, "heap *{i:x}"),
            Pointer::Function(i) => write!(f, "function *{i:x}"),
        }
    }
}