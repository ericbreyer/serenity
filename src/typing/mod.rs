use std::{ collections::HashMap, fmt::Debug, ops::Deref, rc::Rc, sync::{ Arc, Mutex, RwLock } };

use pinvec::PinVec;
use pow_of_2::PowOf2;
use lazy_static::lazy_static;

#[derive(Copy, Clone)]
pub struct UValueType(usize);

impl PartialEq for UValueType {
  fn eq(&self, other: &Self) -> bool {
    self.as_ref().eq(other.as_ref())
  }
}

#[derive(Clone)]
pub struct CustomStruct {
  pub name: String,
  pub fields: Arc<RwLock<HashMap<String, StructEntry>>>,
}

impl Debug for CustomStruct {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "struct {}", self.name)
  }
}

// impl Debug for CustomStruct {
//   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     write!(f, "struct {}", self.name)
//   }
// }

#[derive(Clone, Debug, PartialEq)]
pub struct StructEntry {
  pub value: UValueType,
  pub offset: usize,
}

#[derive(Clone, Debug)]
#[repr(usize)]
pub enum ValueType {
  Float,
  Integer,
  UInteger,
  Char,
  Bool,
  Nil,
  String,
  Closure(Box<[UValueType]>),
  AnyFunction,
  Pointer(UValueType, bool),
  Array(UValueType, usize),
  Struct(CustomStruct),
  SelfStruct(String),
  Undef,
  All,
  Err,
}

impl ValueType {
  pub fn num_words(&self) -> usize {
    match self {
      | Self::Float
      | Self::Integer
        | Self::UInteger
      | Self::Char
      | Self::Bool
      | Self::Nil
      | Self::String
      | Self::Pointer(_, _)
      | Self::Array(_, _)
      | Self::Undef
      | Self::All
      | Self::Err => 1,
      Self::Closure(_) => 3,
      Self::AnyFunction => panic!("cannot convert anyfunction to word"),
      Self::Struct(h) => {
        let mut sum = 0;
        for (_, v) in h.fields.read().as_ref().unwrap().iter() {
          sum += v.value.as_ref().num_words();
        }
        sum
      }
      Self::SelfStruct(_) => panic!("cannot convert selfstruct to word"),
    }
  }

  fn to_trie_string(&self) -> String {
    match self {
      Self::Float => "1".into(),
      Self::Integer => "2".into(),
        Self::UInteger => "8".into(),
      Self::Bool => "3".into(),
      Self::Nil => "4".into(),
      Self::String => "5".into(),
      Self::Closure(b) =>
        format!(
          "6{}",
          b
            .iter()
            .map(|t| t.as_ref().to_trie_string())
            .fold(String::new(), |a, s| {
              let mut r = a;
              r.push_str(&s);
              r
            })
        ),
      Self::AnyFunction => "7".into(),
      Self::Pointer(p, s) => format!("9{}{}", p.as_ref().to_trie_string(), s),
      Self::Array(p, c) => format!("10{};{}", p.as_ref().to_trie_string(), c),
      Self::Struct(h) =>
        format!(
          "E{}",
          h.fields
            .read()
            .as_ref()
            .unwrap()
            .iter()
            .map(|(k, v)| format!("{}:{}", k, v.value.as_ref().to_trie_string()))
            .fold(String::new(), |a, s| {
              let mut r = a;
              r.push_str(&s);
              r
            })
        ),
      Self::Undef => "A".into(),
      Self::All => "B".into(),
      Self::Err => "C".into(),
      Self::Char => "D".into(),
      Self::SelfStruct(s) => format!("F{s}"),
    }
  }

  fn soft_compare(&self, other: &Self) -> bool {
    if [self, other].into_iter().any(Self::is_all) {
      return true;
    }
    match (self, other) {
      | (Self::Closure(_), Self::AnyFunction)
      | (Self::AnyFunction, Self::Closure(_))
      | (Self::Pointer(_, _), Self::Nil)
      | (Self::Nil, Self::Pointer(_, _)) => true,
      (Self::Struct(l0), Self::Struct(r0)) => { l0.name == r0.name }
      (Self::Closure(l0v), Self::Closure(r0v)) => {
        l0v.len() == r0v.len() &&
          l0v
            .iter()
            .zip(r0v.iter())
            .all(|(l0, r0)| ([*l0, *r0].into_iter().any(|x| x.as_ref().is_all()) || l0 == r0))
      }
      (Self::Array(l0, l0c), Self::Array(r0, r0c)) => {
        ([*l0, *r0].into_iter().any(|x| x.as_ref().is_all()) || l0 == r0) &&
          ([l0c, r0c].into_iter().any(|u| *u == usize::MAX) || l0c == r0c)
      }
      (Self::Pointer(l0, _), Self::Pointer(r0, _)) => {
        [*l0, *r0].into_iter().any(|x| x.as_ref().is_all()) || l0 == r0
      }

      (Self::SelfStruct(l0), Self::SelfStruct(r0)) => l0 == r0,
      (Self::SelfStruct(l0), Self::Struct(r0)) => l0 == &r0.name,
      (Self::Struct(l0), Self::SelfStruct(r0)) => &l0.name == r0,

      _ => core::mem::discriminant(self) == core::mem::discriminant(other),
    }
  }
}

impl PartialEq for ValueType {
  fn eq(&self, other: &Self) -> bool {
    self.soft_compare(other)
  }
}

lazy_static! {
    static ref USED_TYPES: Arc<Mutex<PinVec<ValueType>>> = Arc::new(Mutex::new(PinVec::new(PowOf2::from_exp(8))));
    static ref USED_TYPES_INDECES: Arc<Mutex<HashMap<String, usize>>> = Arc::new(Mutex::new(HashMap::new()));
}

impl ValueType {
  pub fn is_all(&self) -> bool {
    core::mem::discriminant(self) == core::mem::discriminant(&Self::All)
  }

  pub fn decay(&self, custom_structs: Option<Rc<HashMap<String, CustomStruct>>>) -> UValueType {
    match (self, custom_structs) {
      (Self::Array(p, _), _) => Self::Pointer(*p, true).clone().intern(),
      (Self::String, _) => Self::Pointer(Self::Char.intern(), true).clone().intern(),
      (Self::Pointer(maybe_ss, _), Some(cs)) => {
        if let Self::SelfStruct(s) = maybe_ss.as_ref() {
          if let Some(s) = cs.get(s) {
            Self::Pointer(Self::Struct(s.clone()).intern(), false).clone().intern()
          } else {
            Self::Err.intern()
          }
        } else {
          self.clone().intern()
        }
      }
      _ => self.clone().intern(),
    }
  }

  pub fn intern(&self) -> UValueType {
    let sid = self.to_trie_string();

    let mut used_vec = USED_TYPES.lock().unwrap();
    let mut used_indeces = USED_TYPES_INDECES.lock().unwrap();

    if let Some(idx) = used_indeces.get(&sid) {
      return UValueType(*idx);
    }

    let value = self.clone();
    let idx = used_vec.len();
    used_indeces.insert(sid, idx);
    used_vec.push(value);
    UValueType(idx)
  }
}

impl AsRef<ValueType> for UValueType {
  fn as_ref(&self) -> &ValueType {
    let used_types = USED_TYPES.lock().unwrap();
    let value_type = &used_types.idx_ref(self.0);
    // Lock is released here, it is safe to dereference value_type
    unsafe {
      &*std::ptr::addr_of!(**value_type)
    }
  }
}

impl Deref for UValueType {
  type Target = ValueType;

  fn deref(&self) -> &Self::Target {
    self.as_ref()
  }
}

impl Debug for UValueType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.as_ref().fmt(f)
  }
}

impl From<String> for UValueType {
  fn from(s: String) -> Self {
    match s.as_str() {
      "float" => ValueType::Float.intern(),
      "int" => ValueType::Integer.intern(),
        "uint" => ValueType::UInteger.intern(),
      "bool" => ValueType::Bool.intern(),
      "nil" => ValueType::Nil.intern(),
      "string" => ValueType::String.intern(),
      _ => ValueType::Err.intern(),
    }
  }
}
