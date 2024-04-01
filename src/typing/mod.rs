use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Mutex};

use pinvec::PinVec;
use pow_of_2::PowOf2;
use radix_trie::Trie;

pub type ValueType = &'static ValueTypeK;

#[derive(Clone, Debug)]
pub struct CustomStruct {
    pub name: String,
    pub fields: Rc<RefCell<HashMap<String, StructEntry>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructEntry {
    pub value: ValueType,
    pub offset: usize,
}

pub enum Assignable {
    Const,
    Uninit,
    Mut,
}

#[derive(Clone, Debug)]
#[repr(usize)]
pub enum ValueTypeK {
    Float,
    Integer,
    Char,
    Bool,
    Nil,
    String,
    Closure(Box<[ValueType]>),
    AnyFunction,
    Pointer(ValueType, bool),
    Array(ValueType, usize),
    Struct(CustomStruct),
    SelfStruct(String),
    AnyStruct,
    Undef,
    All,
    Err,
}

impl ValueTypeK {

    pub fn num_words(&self) -> usize {
        match self {
            Self::Float => 1,
            Self::Integer => 1,
            Self::Char => 1,
            Self::Bool => 1,
            Self::Nil => 1,
            Self::String => 1,
            Self::Closure(_) => 3,
            Self::AnyFunction => panic!("cannot convert anyfunction to word"),
            Self::Pointer(_, _) => 1,
            Self::Array(_, _c) => 1,
            Self::Struct(h) => {
                let mut sum = 0;
                for (_, v) in h.fields.borrow().iter() {
                    sum += v.value.num_words();
                }
                sum
            },
            Self::AnyStruct => panic!("cannot convert anystruct to word"),
            Self::SelfStruct(_) => panic!("cannot convert selfstruct to word"),
            Self::Undef => 1,
            Self::All => 1,
            Self::Err => 1,
        }
    }

    fn to_trie_string(&self) -> String {
        match self {
            Self::Float => "1".into(),
            Self::Integer => "2".into(),
            Self::Bool => "3".into(),
            Self::Nil => "4".into(),
            Self::String => "5".into(),
            Self::Closure(b) => format!("6{:?}", b.iter().map(|t| t.to_trie_string()).fold("".to_string(), |a, s| {let mut r = a; r.push_str(&s); r})).into(),
            Self::AnyFunction => "7".into(),
            Self::Pointer(p, s) => format!("9{}{}", p.to_trie_string(), s),
            Self::Array(p, c) => format!("10{};{}", p.to_trie_string(), c),
            Self::Struct(h) => format!("E{:?}", h.fields.borrow().iter().map(|(k, v)| format!("{}:{}", k, v.value.to_trie_string())).fold("".to_string(), |a, s| {let mut r = a; r.push_str(&s); r})),
            Self::Undef => "A".into(),
            Self::All => "B".into(),
            Self::Err => "C".into(),
            Self::Char => "D".into(),
            Self::AnyStruct => "F".into(),
            Self::SelfStruct(s) => format!("G{}", s),
        }
    }

    fn soft_compare(&self, other: &Self) -> bool {
        if [self, other].into_iter().any(Self::is_all) {
            return true;
        }
        match (self, other) {
            (Self::Closure(_), Self::AnyFunction) => true,
            (Self::AnyFunction, Self::Closure(_)) => true,
            (Self::Struct(_), Self::AnyStruct) => true,
            (Self::AnyStruct, Self::Struct(_)) => true,
            (Self::Struct(l0), Self::Struct(r0)) => {
                l0.name == r0.name
            }
            (Self::Closure(l0v), Self::Closure(r0v)) => {
                l0v.len() == r0v.len()
                    && l0v
                        .iter()
                        .zip(r0v.iter())
                        .all(|(l0, r0)| [(*l0), (*r0)].into_iter().any(Self::is_all) || l0 == r0)
            }
            (Self::Array(l0, l0c), Self::Array(r0, r0c)) => {
                ([*l0, *r0].into_iter().any(Self::is_all) || l0 == r0)
                    && ([l0c, r0c].into_iter().any(|u| *u == usize::MAX) || l0c == r0c)
            }
            (Self::Pointer(l0, _), Self::Pointer(r0, _)) => {
                [*l0, *r0].into_iter().any(Self::is_all) || l0 == r0
            }
            (Self::Pointer(_, _), Self::Nil) => true,
            (Self::Nil, Self::Pointer(_, _)) => true,
            (Self::SelfStruct(l0), Self::SelfStruct(r0)) => l0 == r0,
            (Self::SelfStruct(l0), Self::Struct(r0)) => l0 == &r0.name,
            (Self::Struct(l0), Self::SelfStruct(r0)) => &l0.name == r0,


            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialEq for ValueTypeK{
    fn eq(&self, other: &Self) -> bool {
        self.soft_compare(other)
    }
}


impl ValueTypeK {
    pub fn cast_to(&self, other: &Self) -> bool {
        if [self, other].into_iter().any(Self::is_all) {
            return true;
        }
        match (self.decay(None), other) {
            (Self::Float, Self::Integer) => true,
            (Self::Integer, Self::Float) => true,
            (Self::Pointer(_l0, _), Self::Pointer(_r0, _)) => {
                true
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }

    pub fn is_all(&self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(&Self::All)
    }

    pub fn decay(&self, custom_structs: Option<Rc<HashMap<String, CustomStruct>>>) -> ValueType {
        match (self, custom_structs) {
            (Self::Array(p, _), _) => &Self::Pointer(p, true).clone().intern(),
            (Self::String, _) => &Self::Pointer(&Self::Char, true).clone().intern(),
            (Self::Pointer(Self::SelfStruct(s), _), Some(cs))  => {
                if let Some(s) = cs.get(s) {
                    &Self::Pointer(Self::Struct(s.clone()).intern(), false).clone().intern()
                } else {
                    &Self::Err.intern()
                }
            }
            _ => self.clone().intern(),
        }
    }

    pub fn intern(self) -> ValueType {
        static LOCK: Mutex<()> = Mutex::new(());

        if let Ok(_lock) = LOCK.lock() {
            unsafe {
                static mut USED_TYPES: Option<PinVec<ValueTypeK>> = None;
                static mut USED_TYPES_INDECES: Option<Trie<String, usize>> = None;
                if USED_TYPES.is_none() {
                    USED_TYPES = Some(PinVec::new(PowOf2::from_exp(8)));
                    USED_TYPES_INDECES = Some(Trie::new());
                }
                let used_vec = USED_TYPES.as_mut().unwrap();
                let used_indeces = USED_TYPES_INDECES.as_mut().unwrap();

                if let Some(idx) = used_indeces.get(&self.to_trie_string()) {
                    return &used_vec.idx_ref(*idx).get_ref();
                }

                used_indeces.insert(self.to_trie_string(), used_vec.len());
                used_vec.push(self);
                &used_vec.idx_ref(used_vec.len() - 1).get_ref()
            }
        } else {
            panic!("Could not lock type intern lock");
        }
    }
}

#[derive(Clone, Debug)]
pub struct ValueTypeSet {
    set: Vec<ValueType>,
}

impl ValueTypeSet {
    pub fn new(types: Vec<ValueType>) -> ValueTypeSet {
        ValueTypeSet { set: types }
    }

    pub fn contains(&self, value: ValueType) -> bool {
        self.set.iter().any(|v| v.soft_compare(value))
    }
}

impl From<String> for ValueType {
    fn from(s: String) -> Self {
        match s.as_str() {
            "float" => ValueTypeK::Float.intern(),
            "int" => ValueTypeK::Integer.intern(),
            "bool" => ValueTypeK::Bool.intern(),
            "nil" => ValueTypeK::Nil.intern(),
            "string" => ValueTypeK::String.intern(),
            _ => ValueTypeK::Err.intern(),
        }
    }
}
