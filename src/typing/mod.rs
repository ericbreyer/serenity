use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
};

use anyhow::{Context as _, Result};
use arena_alloc::Arena;
pub use closure::*;
pub use custom_struct::*;
use indexmap::IndexMap;
use inkwell::{context::Context, types::BasicType as _, AddressSpace};

use crate::prelude::*;

mod closure;
mod custom_struct;
mod hindley_milner;
mod internment;
mod print;

pub type UValueType = &'static ValueType;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constraint(pub Option<SharedString>);

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
    Struct(Box<CustomStruct>),
    SelfStructRef(SharedString, Vec<UValueType>),
    GenericParam(SharedString, Box<[Constraint]>),
    TypeVar(usize),
    Err,
}

impl ValueType {
    pub fn satisfies_constraints(
        &'static self,
        constraint: &[Constraint],
        generics: &ScopedMap<SharedString, UValueType>,
    ) -> bool {
        if constraint.is_empty() {
            return true;
        }
        let subedself = self.substitute(generics);
        match subedself {
            Self::Struct(s) => {
                for c in constraint.iter() {
                    if let Some(ref name) = c.0 {
                        if !s.implements.contains(name) {
                            return false;
                        }
                    }
                }
                true
            }
            _ => false,
        }
    }

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
            Self::SelfStructRef(s, v) => {
                if let Some(s) = custom_structs.get(s) {
                    let mut tvs = HashMap::new();
                    for (n, t) in s.type_vars.borrow().iter().zip(v.iter()) {
                        tvs.insert(n.0.clone(), *t);
                    }
                    Self::Struct(Box::new(s.clone()))
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
            | (Self::Pointer(&Self::Nil, _), Self::Pointer(_, _)) => true,
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
            (Self::SelfStructRef(l0, _), Self::SelfStructRef(r0, _)) => l0 == r0,
            (Self::SelfStructRef(l0, _), Self::Struct(r0)) => l0 == &r0.name,
            (Self::Struct(l0), Self::SelfStructRef(r0, _)) => &l0.name == r0,
            (Self::GenericParam(l0, l1), Self::GenericParam(r0, r1)) => l0 == r0 && l1 == r1,
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
            Self::SelfStructRef(_, _) => unreachable!("self struct should be replaced by struct"),
            Self::GenericParam(p, _) => {
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
            s if s.chars().nth(0).unwrap() == '$' => Self::TypeVar(s[1..].parse().unwrap()),
            s if s.contains(":") => {
                // generic param with constraints
                let parts: Vec<&str> = s.split(':').collect();
                let name = parts[0].to_string();
                let constraints = parts[1..]
                    .iter()
                    .map(|c| {
                        if c.is_empty() {
                            Constraint(None)
                        } else {
                            Constraint(Some(c.to_string().into()))
                        }
                    })
                    .collect::<Vec<_>>();
                Self::GenericParam(name.into(), constraints.into_boxed_slice())
            }
            s => Self::GenericParam(s.into(), Box::new([])),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_types_creation() {
        let int_type = ValueType::Integer;
        let float_type = ValueType::Float;
        let bool_type = ValueType::Bool;
        let char_type = ValueType::Char;
        let nil_type = ValueType::Nil;

        assert_eq!(format!("{}", int_type), "int");
        assert_eq!(format!("{}", float_type), "float");
        assert_eq!(format!("{}", bool_type), "bool");
        assert_eq!(format!("{}", char_type), "char");
        assert_eq!(format!("{}", nil_type), "nil");
    }

    #[test]
    fn test_type_from_string() {
        let int_type = ValueType::from(SharedString::from("int"));
        let float_type = ValueType::from(SharedString::from("float"));
        let bool_type = ValueType::from(SharedString::from("bool"));
        let char_type = ValueType::from(SharedString::from("char"));
        let nil_type = ValueType::from(SharedString::from("nil"));

        assert!(matches!(int_type, ValueType::Integer));
        assert!(matches!(float_type, ValueType::Float));
        assert!(matches!(bool_type, ValueType::Bool));
        assert!(matches!(char_type, ValueType::Char));
        assert!(matches!(nil_type, ValueType::Nil));
    }

    #[test]
    fn test_generic_param_from_string() {
        let generic_type = ValueType::from(SharedString::from("T"));
        assert!(matches!(generic_type, ValueType::GenericParam(_, _)));
    }

    #[test]
    fn test_type_var_from_string() {
        let type_var = ValueType::from(SharedString::from("$0"));
        assert!(matches!(type_var, ValueType::TypeVar(0)));
    }

    #[test]
    fn test_constraint_creation() {
        let constraint_some = Constraint(Some("Iterator".into()));
        let constraint_none = Constraint(None);

        assert!(constraint_some.0.is_some());
        assert!(constraint_none.0.is_none());
    }

    #[test]
    fn test_constraint_equality() {
        let c1 = Constraint(Some("Iterator".into()));
        let c2 = Constraint(Some("Iterator".into()));
        let c3 = Constraint(None);

        assert_eq!(c1, c2);
        assert_ne!(c1, c3);
    }

    #[test]
    fn test_value_type_equality_basic() {
        let int1 = ValueType::Integer;
        let int2 = ValueType::Integer;
        let float = ValueType::Float;

        assert_eq!(int1, int2);
        assert_ne!(int1, float);
    }

    #[test]
    fn test_err_type() {
        let err_type = ValueType::Err;
        assert_eq!(format!("{}", err_type), "err");
    }

    #[test]
    fn test_id_str_generation() {
        let int_type = ValueType::Integer;
        let id_str = int_type.id_str();
        assert!(!id_str.is_empty());
    }

    #[test]
    fn test_different_types_different_id_str() {
        let int_type = ValueType::Integer;
        let float_type = ValueType::Float;

        let int_id = int_type.id_str();
        let float_id = float_type.id_str();

        assert_ne!(int_id, float_id);
    }

    #[test]
    fn test_type_soft_compare() {
        let int1 = ValueType::Integer;
        let int2 = ValueType::Integer;
        let float = ValueType::Float;

        assert!(int1.soft_compare(&int2));
        assert!(!int1.soft_compare(&float));
    }

    #[test]
    fn test_type_display() {
        let types = vec![
            (ValueType::Integer, "int"),
            (ValueType::Float, "float"),
            (ValueType::Bool, "bool"),
            (ValueType::Char, "char"),
            (ValueType::Nil, "nil"),
            (ValueType::Err, "err"),
        ];

        for (value_type, expected) in types {
            assert_eq!(format!("{}", value_type), expected);
        }
    }

    #[test]
    fn test_value_type_debug() {
        let int_type = ValueType::Integer;
        let debug_str = format!("{:?}", int_type);
        assert!(!debug_str.is_empty());
    }

    #[test]
    fn test_multiple_generic_params() {
        let generic = ValueType::from(SharedString::from("T:Iterator:Clone"));
        if let ValueType::GenericParam(name, constraints) = generic {
            assert_eq!(name, "T".into());
            assert_eq!(constraints.len(), 2);
        } else {
            panic!("Expected GenericParam");
        }
    }

    #[test]
    fn test_constraint_with_none() {
        let type_str = SharedString::from("T:");
        let generic = ValueType::from(type_str);
        if let ValueType::GenericParam(_, constraints) = generic {
            assert!(!constraints.is_empty());
        } else {
            panic!("Expected GenericParam");
        }
    }

    #[test]
    fn test_type_clone() {
        let original = ValueType::Integer;
        let cloned = original.clone();
        assert_eq!(original, cloned);
    }

    #[test]
    fn test_primitive_types_distinct() {
        let types = vec![
            ValueType::Integer,
            ValueType::Float,
            ValueType::Bool,
            ValueType::Char,
            ValueType::Nil,
            ValueType::Err,
        ];

        for i in 0..types.len() {
            for j in (i + 1)..types.len() {
                assert_ne!(types[i], types[j]);
            }
        }
    }

    #[test]
    fn test_generic_param_display() {
        let generic = ValueType::GenericParam("T".into(), Box::new([]));
        let display_str = format!("{}", generic);
        assert!(display_str.contains("T"));
    }

    #[test]
    fn test_err_type_from_string() {
        let err = ValueType::from(SharedString::from("err"));
        assert!(matches!(err, ValueType::Err));
    }

    #[test]
    fn test_type_var_display() {
        let type_var = ValueType::TypeVar(42);
        let display_str = format!("{}", type_var);
        assert_eq!(display_str, "$42");
    }

    #[test]
    fn test_constraint_clone() {
        let original = Constraint(Some("Trait".into()));
        let cloned = original.clone();
        assert_eq!(original, cloned);
    }

    #[test]
    fn test_constraint_debug() {
        let c = Constraint(Some("Iterator".into()));
        let debug_str = format!("{:?}", c);
        assert!(!debug_str.is_empty());
    }
}
