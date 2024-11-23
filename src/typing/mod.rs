use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
};

use anyhow::{Context as _, Result};
use arena_alloc::Arena;
use indexmap::IndexMap;
use inkwell::{context::Context, types::BasicType as _, AddressSpace};

use crate::prelude::*;

pub use custom_struct::*;
pub use closure::*;

mod closure;
mod custom_struct;
mod internment;
mod hindley_milner;
mod print;

pub type UValueType = &'static ValueType;

#[allow(clippy::derived_hash_with_manual_eq)]
#[derive(Clone, Hash)]
#[repr(usize)]
pub enum ValueType {
    Float,
    Integer,
    Char,
    Bool,
    Nil,
    Closure(Closure),
    ExternalFn(UValueType, SharedString),
    Pointer(UValueType, bool),
    LValue(UValueType, bool),
    Array(UValueType, Option<usize>),
    Struct(CustomStruct),
    SelfStruct(SharedString, Vec<UValueType>),
    GenericParam(SharedString),
    TypeVar(usize),
    Err,
}


impl ValueType {
    /// # Decay
    /// This is an identity function for all types except arrays, which decay
    /// to pointers as in `[T; N] -> *T`
    pub fn decay(&'static self) -> UValueType {
        match self {
            Self::Array(p, _) => Self::Pointer(p, true).intern(),
            _ => self,
        }
    }

    // fn is_nil(&self) -> bool {
    //     matches!(self, Self::Nil)
    // }

    pub fn fill_self_struct(
        &'static self,
        custom_structs: HashMap<SharedString, CustomStruct>,
    ) -> UValueType {
        match self {
            Self::Pointer(t, b) => Self::Pointer(t.fill_self_struct(custom_structs), *b).intern(),
            Self::LValue(t, b) => Self::LValue(t.fill_self_struct(custom_structs), *b).intern(),
            Self::Array(t, n) => Self::Array(t.fill_self_struct(custom_structs), *n).intern(),
            Self::SelfStruct(s, v) => {
                if let Some(s) = custom_structs.get(s) {
                    let mut tvs = HashMap::new();
                    for (n, t) in s.type_vars.borrow().iter().zip(v.iter()) {
                        tvs.insert(n.clone(), *t);
                    }
                    Self::Struct(s.clone())
                        .intern()
                        .instantiate_generic(&mut tvs)
                } else {
                    Self::Err.intern()
                }
            }
            _ => self,
        }
    }

    pub fn id_str(&self) -> SharedString {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.hash(&mut hasher);
        format!("{}", hasher.finish()).into()
    }

    fn soft_compare(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pointer(_, _), Self::Pointer(&Self::Nil, _))
            | (Self::Pointer(&Self::Nil, _), Self::Pointer(_, _))
                =>
            {
                true
            }
            (Self::Struct(l0), Self::Struct(r0)) => l0.name == r0.name,
            (
                ValueType::Closure(Closure {
                    args: l0v,
                    upvals: u0,
                    ret: r0,
                    generics: _,
                }),
                ValueType::Closure(Closure {
                    args: r0v,
                    upvals: u1,
                    ret: r1,
                    generics: _,
                }),
            ) => {
                l0v.len() == r0v.len()
                    && l0v.iter().zip(r0v.iter()).all(|(l0, r0)| l0 == r0)
                    && u0.len() == u1.len()
                    && u0.iter().zip(u1.iter()).all(|(l0, r0)| l0 == r0)
                    && r0 == r1
            }
            (Self::Array(l0, l0c), Self::Array(r0, r0c)) => l0 == r0 && (l0c == r0c),
            (Self::Pointer(l0, _), Self::Pointer(r0, _)) => l0 == r0,
            (Self::SelfStruct(l0,_), Self::SelfStruct(r0,_)) => l0 == r0,
            (Self::SelfStruct(l0,_), Self::Struct(r0)) => l0 == &r0.name,
            (Self::Struct(l0), Self::SelfStruct(r0,_)) => &l0.name == r0,
            (Self::GenericParam(l0), Self::GenericParam(r0)) => l0 == r0,
            (Self::TypeVar(l0), Self::TypeVar(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }

    pub fn llvm<'ctx>(
        &'static self,
        ctx: &'ctx Context,
        generics: &ScopedMap<SharedString, UValueType>,
    ) -> Result<inkwell::types::BasicTypeEnum<'ctx>> {
        Ok(match self.substitute(generics) {
            Self::Float => ctx.f64_type().as_basic_type_enum(),
            Self::Integer => ctx.i64_type().as_basic_type_enum(),
            Self::Char => ctx.i8_type().as_basic_type_enum(),
            Self::Bool => ctx.bool_type().as_basic_type_enum(),
            Self::Nil => ctx.i8_type().as_basic_type_enum(),
            ValueType::Closure(Closure {
                args: _,
                upvals,
                ret: _,
                generics: _,
            }) => {
                let mut types = Vec::new();
                for v in upvals.iter() {
                    types.push(v.llvm(ctx, generics)?);
                }
                types.push(ctx.ptr_type(AddressSpace::default()).as_basic_type_enum());
                ctx.struct_type(&types, false).as_basic_type_enum()
            }
            Self::ExternalFn(_, _) => ctx.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Self::Pointer(_t, _) => ctx.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Self::LValue(_t, _) => ctx.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Self::Array(t, n) => t
                .llvm(ctx, generics)?
                .array_type(n.unwrap_or(0) as u32)
                .as_basic_type_enum(),
            Self::Struct(h) => {
                let mut types = Vec::new();
                let bg = h.fields.borrow();
                for (_, v) in bg.iter() {
                    types.push(v.value.llvm(ctx, generics)?);
                }
                ctx.struct_type(&types, false).as_basic_type_enum()
            }
            Self::SelfStruct(_, _) => unreachable!("self struct should be replaced by struct"),
            Self::GenericParam(p) => {
                if let Ok(v) = generics.get(p) {
                    v.llvm(ctx, generics)?
                } else {
                    return Err(anyhow::anyhow!("generic param <{p}> not resolved"));
                }
            }
            Self::Err => unreachable!("err type should be replaced by type"),
            ValueType::TypeVar(v) => return Err(anyhow::anyhow!("type var ${v} not resolved")),
        })
    }
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        self.soft_compare(other)
    }
}


impl From<SharedString> for ValueType {
    fn from(s: SharedString) -> Self {
        match Into::<String>::into(s).as_str() {
            "float" => Self::Float,
            "int" => Self::Integer,
            "char" => Self::Char,
            "bool" => Self::Bool,
            "nil" => Self::Nil,
            "err" => Self::Err,
            s => Self::GenericParam(s.into()),
        }
    }
}
