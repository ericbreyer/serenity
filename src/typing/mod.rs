use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
};

use inkwell::{context::Context, types::BasicType as _, AddressSpace};
use pinvec::PinVec;
use pow_of_2::PowOf2;

use crate::prelude::*;

#[derive(Copy, Clone, Hash, Eq)]
pub struct UValueType(usize);

impl PartialEq for UValueType {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

#[derive(Clone)]
pub struct CustomStruct {
    pub name: SharedString,
    pub fields: RefCell<HashMap<SharedString, StructEntry>>,
    pub embed: Option<SharedString>,
    pub methods: RefCell<HashSet<SharedString>>,
}

impl core::hash::Hash for CustomStruct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.embed.hash(state);
        self.fields
            .borrow()
            .iter()
            .collect::<Vec<_>>()
            .sort_by_key(|t| t.0)
            .hash(state);
        self.methods
            .borrow()
            .iter()
            .collect::<Vec<_>>()
            .sort()
            .hash(state);
    }
}

impl Debug for CustomStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct StructEntry {
    pub value: UValueType,
    pub offset: usize,
}

#[derive(Clone, Hash)]
#[repr(usize)]
pub enum ValueType {
    Float,
    Integer,
    UInteger,
    Char,
    Bool,
    Nil,
    Closure(Box<[UValueType]>, usize),
    FnPointer,
    Pointer(UValueType, bool),
    LValue(UValueType, bool),
    Array(UValueType, usize),
    Struct(CustomStruct),
    SelfStruct(SharedString),
    GenericParam(SharedString),
    Undef,
    All,
    Err,
}

impl Debug for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "f64"),
            Self::Integer => write!(f, "i64"),
            Self::UInteger => write!(f, "u64"),
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::Nil => write!(f, "nil"),
            Self::Closure(arg0, arg1) => f.debug_tuple("fn").field(arg0).field(arg1).finish(),
            Self::FnPointer => write!(f, "FnPointer"),
            Self::Pointer(arg0, _) => write!(f, "*{:?}", arg0),
            Self::LValue(arg0, _) => write!(f, "&{:?}", arg0),
            Self::Array(arg0, arg1) => write!(f, "{:?}[{}]", arg0, arg1),
            Self::Struct(arg0) => write!(f, "{{{arg0:?}}}"),
            Self::SelfStruct(arg0) => f.debug_tuple("SelfStruct").field(arg0).finish(),
            Self::GenericParam(arg0) => f.debug_tuple("GenericParam").field(arg0).finish(),
            Self::Undef => write!(f, "Undef"),
            Self::All => write!(f, "All"),
            Self::Err => write!(f, "Err"),
        }
    }
}

impl ValueType {
    pub fn is_aggregate(&self) -> bool {
        match self {
            Self::Struct(_) | Self::Closure(_, _) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Self::Array(_, _) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Self::Nil => true,
            _ => false,
        }
    }

    pub fn to_string(&self) -> SharedString {
        match self {
            ValueType::Float => "float".into(),
            ValueType::Integer => "int".into(),
            ValueType::UInteger => "uint".into(),
            ValueType::Char => "char".into(),
            ValueType::Bool => "bool".into(),
            ValueType::Nil => "nil".into(),
            ValueType::Closure(t, _) => {
                let mut s = String::new();
                s.push_str("fn(");
                for (i, v) in t[0..t.len() - 1].iter().enumerate() {
                    s.push_str(&v.to_string());
                    if i != t.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") -> ");
                s.push_str(&t[t.len() - 1].to_string());
                s.into()
            }
            ValueType::FnPointer => todo!(),
            ValueType::Pointer(t, _) => format!("*{}", t.to_string()).into(),
            ValueType::LValue(_, _) => todo!(),
            ValueType::Array(_, _) => todo!(),
            ValueType::Struct(_) => todo!(),
            ValueType::SelfStruct(_) => todo!(),
            ValueType::GenericParam(_) => todo!(),
            ValueType::Undef => todo!(),
            ValueType::All => todo!(),
            ValueType::Err => todo!(),
        }
    }

    pub fn num_words(&self) -> usize {
        match self {
            Self::Float
            | Self::Integer
            | Self::UInteger
            | Self::Char
            | Self::Bool
            | Self::Nil
            | Self::Pointer(_, _)
            | Self::LValue(_, _)
            | Self::Undef
            | Self::All
            | Self::Err => 1,
            Self::Closure(_, us) => us + 1,
            Self::FnPointer => 2,
            Self::Array(t, n) => t.num_words() * n,
            Self::Struct(h) => {
                let mut sum = 0;
                for (_, v) in h.fields.borrow().iter() {
                    sum += v.value.as_ref().num_words();
                }
                sum
            }
            Self::SelfStruct(_) => panic!("cannot convert selfstruct to word"),
            Self::GenericParam(_) => panic!("cannot convert generic param to word"),
        }
    }

    pub fn unique_string(&self) -> SharedString {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.hash(&mut hasher);
        format!("{}", hasher.finish()).into()
    }

    fn soft_compare(&self, other: &Self) -> bool {
        if [self, other].into_iter().any(Self::is_all) {
            return true;
        }
        match (self, other) {
            (Self::Pointer(_, _), Self::Pointer(nil, _))
            | (Self::Pointer(nil, _), Self::Pointer(_, _))
                if nil.is_nil() =>
            {
                true
            }
            (Self::Struct(l0), Self::Struct(r0)) => l0.name == r0.name,
            (Self::Closure(l0v, u0), Self::Closure(r0v, u1)) => {
                u0 == u1
                    && l0v.len() == r0v.len()
                    && l0v.iter().zip(r0v.iter()).all(|(l0, r0)| {
                        [*l0, *r0].into_iter().any(|x| x.as_ref().is_all()) || l0 == r0
                    })
            }
            (Self::Array(l0, l0c), Self::Array(r0, r0c)) => {
                ([*l0, *r0].into_iter().any(|x| x.as_ref().is_all()) || l0 == r0)
                    && ([l0c, r0c].into_iter().any(|u| *u == usize::MAX) || l0c == r0c)
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

    pub fn llvm(self, ctx: &Context) -> inkwell::types::BasicTypeEnum {
        match self {
            Self::Float => ctx.f64_type().as_basic_type_enum(),
            Self::Integer => ctx.i64_type().as_basic_type_enum(),
            Self::UInteger => ctx.i64_type().as_basic_type_enum(),
            Self::Char => ctx.i8_type().as_basic_type_enum(),
            Self::Bool => ctx.bool_type().as_basic_type_enum(),
            Self::Nil => ctx.i8_type().as_basic_type_enum(),
            Self::Closure(_, _) => ctx.i8_type().as_basic_type_enum(),
            Self::FnPointer => ctx.i8_type().as_basic_type_enum(),
            Self::Pointer(t, _) => ctx.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Self::LValue(t, _) => ctx.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Self::Array(t, n) => (*t).clone().llvm(ctx).array_type(n as u32).as_basic_type_enum(),
            Self::Struct(h) => {
                let mut types = Vec::new();
                let bg = h.fields.borrow();
                for (_, v) in bg.iter() {
                    types.push((*v.value).clone().llvm(ctx));
                }
                ctx.struct_type(&types, false).as_basic_type_enum()
            }
            Self::SelfStruct(_) => todo!(),
            Self::GenericParam(_) => todo!(),
            Self::Undef => todo!(),
            Self::All => todo!(),
            Self::Err => todo!(),
        }
    }
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        self.soft_compare(other)
    }
}

thread_local! {
    static USED_TYPES: RefCell<PinVec<ValueType>> =
        RefCell::new(PinVec::new(PowOf2::from_exp(8)));
    static USED_TYPES_INDECES: RefCell<HashMap<SharedString, usize>> =
        RefCell::new(HashMap::new());
}

impl ValueType {
    pub fn is_all(&self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(&Self::All)
    }

    pub fn decay(
        &self,
        custom_structs: Option<Rc<HashMap<SharedString, CustomStruct>>>,
    ) -> UValueType {
        match (self, custom_structs) {
            (Self::Array(p, _), _) => Self::Pointer(*p, true).clone().intern(),
            (Self::Pointer(maybe_ss, _), Some(cs)) => {
                if let Self::SelfStruct(s) = maybe_ss.as_ref() {
                    if let Some(s) = cs.get(s) {
                        Self::Pointer(Self::Struct(s.clone()).intern(), false)
                            .clone()
                            .intern()
                    } else {
                        Self::Err.intern()
                    }
                } else {
                    self.clone().intern()
                }
            }
            (Self::LValue(maybe_ss, _), Some(cs)) => {
                if let Self::SelfStruct(s) = maybe_ss.as_ref() {
                    if let Some(s) = cs.get(s) {
                        Self::LValue(Self::Struct(s.clone()).intern(), false)
                            .clone()
                            .intern()
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

    // pub fn fill_generic(&self, param: SharedString, value: UValueType) -> UValueType {
    //   match self {
    //     Self::GenericParam(p) if p == &param => value,
    //     Self::Closure(v, u) => {
    //       let mut new_v = Vec::with_capacity(v.len());
    //       for x in v.iter() {
    //         new_v.push(x.fill_generic(param.clone(), value));
    //       }
    //       Self::Closure(new_v.into_boxed_slice(), *u).intern()
    //     }
    //     Self::Array(v, u) => Self::Array(v.fill_generic(param, value), *u).intern(),
    //     Self::Pointer(v, b) => Self::Pointer(v.fill_generic(param, value), *b).intern(),
    //     Self::Struct(s) => {
    //       let mut new_fields = HashMap::new();
    //       for (k, v) in s.fields.read().as_ref().unwrap().iter() {
    //         new_fields.insert(k.clone(), v.fill_generic(param.clone(), value));
    //       }
    //       Self::Struct(CustomStruct {
    //         name: s.name.clone(),
    //         fields: Arc::new(RwLock::new(new_fields)),
    //         embed: s.embed.clone(),
    //         methods: s.methods.clone(),
    //       })
    //       .intern()
    //     }

    //     _ => self.clone(),
    //   }
    // }

    pub fn intern(&self) -> UValueType {
        let sid = self.unique_string();

        USED_TYPES.with_borrow_mut(|used_vec| {
            USED_TYPES_INDECES.with_borrow_mut(|used_indeces| {
                if let Some(idx) = used_indeces.get(&sid) {
                    return UValueType(*idx);
                }

                let value = self.clone();
                let idx = used_vec.len();
                used_indeces.insert(sid, idx);
                used_vec.push(value);
                UValueType(idx)
            })
        })
    }
}

impl AsRef<ValueType> for UValueType {
    fn as_ref(&self) -> &ValueType {
        USED_TYPES.with_borrow(|used_types| {
            let value_type = &used_types.idx_ref(self.0);
            // Lock is released here, it is safe to dereference value_type
            unsafe { &*std::ptr::addr_of!(**value_type) }
        })
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

impl From<SharedString> for UValueType {
    fn from(s: SharedString) -> Self {
        match s.as_ref() {
            "float" => ValueType::Float.intern(),
            "int" => ValueType::Integer.intern(),
            "uint" => ValueType::UInteger.intern(),
            "bool" => ValueType::Bool.intern(),
            "nil" => ValueType::Nil.intern(),
            "char" => ValueType::Char.intern(),
            _ => ValueType::Err.intern(),
        }
    }
}
