use std::sync::Mutex;

use pinvec::PinVec;
use pow_of_2::PowOf2;
use radix_trie::Trie;

pub type ValueType = &'static ValueTypeK;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(usize)]
pub enum ValueTypeK {
    Float,
    Integer,
    Bool,
    Nil,
    String,
    Function(Box<[ValueType]>),
    AnyFunction,
    Pointer(ValueType),
    Array(ValueType, usize),
    Undef,
    All,
    Err,
}

impl ValueTypeK {
    fn to_trie_string(&self) -> String {
        match self {
            Self::Float => "1".into(),
            Self::Integer => "2".into(),
            Self::Bool => "3".into(),
            Self::Nil => "4".into(),
            Self::String => "5".into(),
            Self::Function(b) => format!("6{:?}", b.iter().map(|t| t.to_trie_string()).fold("".to_string(), |a, s| {let mut r = a; r.push_str(&s); r})).into(),
            Self::AnyFunction => "7".into(),
            Self::Pointer(p) => format!("9{}", p.to_trie_string()),
            Self::Array(p, c) => format!("10{};{}", p.to_trie_string(), c),
            Self::Undef => "A".into(),
            Self::All => "B".into(),
            Self::Err => "C".into(),
        }
    }

    fn soft_compare(&self, other: &Self) -> bool {
        if [self, other].into_iter().any(Self::is_all) {
            return true;
        }
        match (self, other) {
            (Self::Function(_), Self::AnyFunction) => true,
            (Self::AnyFunction, Self::Function(_)) => true,
            (Self::Function(l0v), Self::Function(r0v)) => {
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
            (Self::Pointer(l0), Self::Pointer(r0)) => {
                [*l0, *r0].into_iter().any(Self::is_all) || l0 == r0
            }

            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

static LOCK: Mutex<()> = Mutex::new(());

impl ValueTypeK {
    pub fn cast_to(&self, other: &Self) -> bool {
        if [self, other].into_iter().any(Self::is_all) {
            return true;
        }
        match (self, other) {
            (Self::Float, Self::Integer) => true,
            (Self::Integer, Self::Float) => true,
            (Self::Pointer(l0), Self::Pointer(r0)) => {
                true
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }

    pub fn is_all(&self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(&Self::All)
    }

    pub fn decay(&self) -> ValueType {
        match self {
            Self::Array(p, _) => &Self::Pointer(p).clone().intern(),
            _ => self.clone().intern(),
        }
    }

    pub fn intern(self) -> ValueType {
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
