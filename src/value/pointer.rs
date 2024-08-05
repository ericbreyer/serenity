use std::{
    fmt::Debug,
    ops::{self},
};

use super::Word;

#[derive(Clone, Copy, PartialEq)]
pub enum Pointer {
    Local(usize),
    Static(usize),
}

impl Pointer {
    const VMASK: u64 = 0x3FFF_FFFF_FFFF_FFFF;

    pub fn to_word(self) -> Word {
        // most significant two bits are to identify the type of pointer

        Word::new(match self {
            Pointer::Static(n) => (1 << 62) | n as u64,
            Pointer::Local(n) => (2 << 62) | n as u64,
        })
    }

    pub fn from_word(word: u64) -> Pointer {
        match word >> 62 {
            2 => Pointer::Local((word & Self::VMASK) as usize),
            1 => Pointer::Static((word & Self::VMASK) as usize),
            _ => panic!("invalid pointer type, {:x} 0x{word:x}", word >> 62),
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
        }
    }
}

impl Debug for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pointer::Local(i) => write!(f, "local *{i:}"),
            Pointer::Static(i) => write!(f, "global *{i:}"),
        }
    }
}
