use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
    hash::{Hash, Hasher},
};

use anyhow::Result;
use arena_alloc::Arena;
use indexmap::IndexMap;
use inkwell::{context::Context, types::BasicType as _, AddressSpace};

use crate::prelude::*;

pub type UValueType = &'static ValueType;

#[derive(Clone)]
pub struct CustomStruct {
    pub name: SharedString,
    pub fields: RefCell<IndexMap<SharedString, StructEntry>>,
    pub embed: Option<SharedString>,
    pub methods: RefCell<HashSet<SharedString>>,
    pub type_vars: RefCell<Vec<SharedString>>,
}

impl core::hash::Hash for CustomStruct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.embed.hash(state);
        for (k, v) in self.fields.borrow().iter() {
            k.hash(state);
            v.value.unique_string().hash(state);
        }
    }
}

impl Debug for CustomStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct")?;
        write!(f, " {}", self.name)?;
        if let Some(embed) = &self.embed {
            write!(f, " {}", embed)?;
        }
        write!(f, " {{")?;
        let bg = self.fields.borrow();
        for (k, v) in bg.iter() {
            write!(f, "{}: {}, ", k, v.value.to_string())?;
        }
        write!(f, "}}")
    }
}

impl<'ctx> CustomStruct {
    pub fn llvm(&self, ctx: &'ctx Context, generics: &HashMap<SharedString, UValueType>) -> Result<inkwell::types::StructType<'ctx>> {
        let mut types = Vec::new();
        let bg = self.fields.borrow();
        for (_, v) in bg.iter() {
            types.push(v.value.llvm(ctx, generics)?);
        }
        Ok(ctx.struct_type(&types, false))
    }

    pub fn to_value_type(&self) -> UValueType {
        ValueType::Struct(self.clone()).intern()
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct StructEntry {
    pub value: UValueType,
    pub offset: usize,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Closure {
    pub args: Box<[UValueType]>,
    pub upvals: Box<[UValueType]>,
    pub ret: UValueType,
    pub generics: BTreeMap<SharedString, UValueType>,
}

impl Closure {
    pub fn new(
        args: Box<[UValueType]>,
        upvals: Box<[UValueType]>,
        ret: UValueType,
        generics: impl Into<Option<BTreeMap<SharedString, UValueType>>>,
    ) -> Self {
        Self {
            args,
            upvals,
            ret,
            generics: generics.into().unwrap_or_default(),
        }
    }
}

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

impl Debug for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

thread_local! {
    static TVAR: Cell<usize> = 0.into();
    static SUBSTITUTIONS: RefCell<Vec<UValueType>> = vec![ValueType::TypeVar(0).intern()].into();
}

impl ValueType {
    pub fn new_type_var() -> UValueType {
        ValueType::TypeVar(TVAR.with(|u| {
            u.set(u.get() + 1);
            SUBSTITUTIONS.with_borrow_mut(|v| v.push(ValueType::TypeVar(u.get()).intern()));
            u.get()
        }))
        .intern()
    }

    pub fn decay(&self) -> UValueType {
        match self {
            Self::Array(p, _) => Self::Pointer(p, true).intern(),
            _ => self.clone().intern(),
        }
    }

    pub fn unify(t1: UValueType, t2: UValueType, generics: &HashMap<SharedString, UValueType>) -> Result<()> {
        match (t1.substitute(generics), t2.substitute(generics)) {
            (ValueType::GenericParam(s0), _) => {
                if let Some(v) = generics.get(s0) {
                    ValueType::unify(v, t2, generics)?;
                } else {
                    return Err(anyhow::anyhow!("generic param <{s0}> not resolved"));
                }
            }
            (_, ValueType::GenericParam(s0)) => {
                if let Some(v) = generics.get(s0) {
                    ValueType::unify(t1, v, generics)?;
                } else {
                    return Err(anyhow::anyhow!("generic param <{s0}> not resolved"));
                }
            }
            (ValueType::TypeVar(x), ValueType::TypeVar(y)) if x == y => {}
            (
                ValueType::Closure(Closure {
                    args: a0,
                    upvals: c0,
                    ret: r0,
                    generics: _,
                }),
                ValueType::Closure(Closure {
                    args: a1,
                    upvals: c1,
                    ret: r1,
                    generics: _,
                }),
            ) => {
                if c0.len() != c1.len() || a0.len() != a1.len() {
                    return Err(anyhow::anyhow!(
                        "closure types do not match {:?} {:?}",
                        t1.substitute(generics),
                        t2.substitute(generics)
                    ));
                }
                for (x, y) in c0.iter().zip(c1.iter()) {
                    ValueType::unify(x, y, generics)?;
                }
                for (x, y) in a0.iter().zip(a1.iter()) {
                    ValueType::unify(x, y, generics)?;
                }
                ValueType::unify(r0, r1, generics)?;
            }
            (ValueType::Pointer(p0, _b0), ValueType::Pointer(p1, _b1)) => {
                if *p0 == &ValueType::Nil || *p1 == &ValueType::Nil {
                    return Ok(());
                }
                ValueType::unify(p0, p1, generics)?;
            }
            (ValueType::LValue(p0, _b0), ValueType::LValue(p1, _b1)) => {
                ValueType::unify(p0, p1, generics)?;
            }
            (ValueType::Array(p0, n0), ValueType::Array(p1, n1)) => {
                if n0.unwrap_or(n1.unwrap_or(0)) != n1.unwrap_or(0) {
                    return Err(anyhow::anyhow!("array types do not match"));
                }
                ValueType::unify(p0, p1, generics)?;
            }
            (ValueType::Struct(s0), ValueType::Struct(s1)) => {
                if s0.name != s1.name {
                    return Err(anyhow::anyhow!("struct types do not match"));
                }
                let s0 = s0.fields.borrow();
                let s1 = s1.fields.borrow();
                if s0.len() != s1.len() {
                    return Err(anyhow::anyhow!("struct types do not match"));
                }
                for (k, v) in s0.iter() {
                    if let Some(v1) = s1.get(k) {
                        ValueType::unify(v.value, v1.value, generics)?;
                    } else {
                        return Err(anyhow::anyhow!("struct types do not match"));
                    }
                }
            }
            (ValueType::SelfStruct(s0, v0), ValueType::SelfStruct(s1, v1)) => {
                if s0 != s1 {
                    return Err(anyhow::anyhow!("self struct types do not match"));
                }
                if v0.len() != v1.len() {
                    return Err(anyhow::anyhow!("self struct types do not match"));
                }
                for (x, y) in v0.iter().zip(v1.iter()) {
                    ValueType::unify(x, y, generics)?;
                }
            }
            (ValueType::Pointer(_p, _), ValueType::Integer)
            | (ValueType::Integer, ValueType::Pointer(_p, _)) => {
                // ok
            }
            // (ValueType::Pointer(p, _), ValueType::Array(a, _))
            //     | (ValueType::Array(p, _), ValueType::Pointer(a, _)) if p == a => {
            //     // ok
            // }
            (ValueType::TypeVar(x), _)
                if SUBSTITUTIONS.with_borrow(|v| v[*x]) != &ValueType::TypeVar(*x) =>
            {
                ValueType::unify(SUBSTITUTIONS.with_borrow(|v| v[*x]), t2, generics)?;
            }
            (_, ValueType::TypeVar(x))
                if SUBSTITUTIONS.with_borrow(|v| v[*x]) != &ValueType::TypeVar(*x) =>
            {
                ValueType::unify(SUBSTITUTIONS.with_borrow(|v| v[*x]), t1, generics)?;
            }
            (ValueType::TypeVar(x), _) => {
                if ValueType::occurs_in(*x, t2) {
                    return Err(anyhow::anyhow!("occurs check failed"));
                }

                SUBSTITUTIONS.with_borrow_mut(|v| v[*x] = t2);
            }
            (_, ValueType::TypeVar(x)) => {
                if ValueType::occurs_in(*x, t1) {
                    return Err(anyhow::anyhow!("occurs check failed"));
                }
                SUBSTITUTIONS.with_borrow_mut(|v| v[*x] = t1);
            }
            (a, b) if a != b => {
                return Err(anyhow::anyhow!("types do not match {:?} {:?}", a, b));
            }
            (_, _) => {}
        };
        Ok(())
    }

    fn occurs_in(tv1: usize, t2: UValueType) -> bool {
        match t2 {
            ValueType::TypeVar(x)
                if SUBSTITUTIONS.with_borrow(|v| v[*x]) != &ValueType::TypeVar(*x) =>
            {
                ValueType::occurs_in(tv1, SUBSTITUTIONS.with_borrow(|v| v[*x]))
            }
            ValueType::TypeVar(x) => tv1 == *x,
            ValueType::Closure(Closure {
                args: a,
                upvals: c,
                ret: r,
                generics: _,
            }) => {
                c.iter().any(|x| ValueType::occurs_in(tv1, x))
                    || a.iter().any(|x| ValueType::occurs_in(tv1, x))
                    || ValueType::occurs_in(tv1, r)
            }
            ValueType::Pointer(p, _) => ValueType::occurs_in(tv1, p),
            ValueType::LValue(p, _) => ValueType::occurs_in(tv1, p),
            ValueType::Array(p, _) => ValueType::occurs_in(tv1, p),
            ValueType::Struct(s) => {
                let s = s.fields.borrow();
                s.values().any(|x| ValueType::occurs_in(tv1, x.value))
            }
            _ => false,
        }
    }

    pub fn substitute<'a>(&self, generics: impl Into<Option<&'a HashMap<SharedString, UValueType>>>) -> UValueType {
        let empty = HashMap::new();
        let generics = generics.into().unwrap_or(&empty);
        let new = match self {
            ValueType::Float
            | ValueType::Integer
            | ValueType::Char
            | ValueType::Bool
            | ValueType::Nil
            | ValueType::Err => self.clone().intern(),
            ValueType::Closure(Closure {
                args: a,
                upvals: c,
                ret: r,
                generics: local_generics,
            }) => {
                let mut new_c = Vec::with_capacity(c.len());
                for x in c.iter() {
                    new_c.push(x.substitute(generics));
                }
                let mut new_a = Vec::with_capacity(a.len());
                for x in a.iter() {
                    new_a.push(x.substitute(generics));
                }

                let mut new_generics = BTreeMap::new();
                for (k, v) in local_generics.iter() {
                    new_generics.insert(k.clone(), v.substitute(generics));
                }

                Self::Closure(Closure::new(
                    new_a.into_boxed_slice(),
                    new_c.into_boxed_slice(),
                    r.substitute(generics),
                    new_generics,
                ))
                .intern()
            }
            ValueType::ExternalFn(r, n) => Self::ExternalFn(r.substitute(generics), n.clone()).intern(),
            ValueType::Pointer(p, b) => Self::Pointer(p.substitute(generics), *b).intern(),
            ValueType::LValue(p, b) => Self::LValue(p.substitute(generics), *b).intern(),
            ValueType::Array(p, n) => Self::Array(p.substitute(generics), *n).intern(),
            ValueType::Struct(s) => {
                let bg = s.fields.borrow();
                let mut new_fields = IndexMap::new();
                for (k, v) in bg.iter() {
                    new_fields.insert(
                        k.clone(),
                        StructEntry {
                            value: v.value.substitute(generics),
                            offset: v.offset,
                        },
                    );
                }

                Self::Struct(CustomStruct {
                    name: s.name.clone(),
                    fields: RefCell::new(new_fields),
                    embed: s.embed.clone(),
                    methods: s.methods.clone(),
                    type_vars: s.type_vars.borrow().clone().into(),
                })
                .intern()
            }
            ValueType::SelfStruct(s, v) => {
                Self::SelfStruct(s.clone(), v.iter().map(|t| t.substitute(generics)).collect()).intern()
            }
            ValueType::GenericParam(s) => {
                if let Some(v) = generics.get(s) {
                    v
                } else {
                    ValueType::GenericParam(s.clone()).intern()
                }
            }
            ValueType::TypeVar(x) => SUBSTITUTIONS.with_borrow(|v| v[*x]),
        };
        if let ValueType::TypeVar(x) = new {
            if SUBSTITUTIONS.with_borrow(|v| v[*x]) != &ValueType::TypeVar(*x) {
                return new.substitute(generics);
            }
        };
        new
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    pub fn to_string(&self) -> SharedString {
        match self {
            ValueType::Float => "float".into(),
            ValueType::Integer => "int".into(),
            ValueType::Char => "char".into(),
            ValueType::Bool => "bool".into(),
            ValueType::Nil => "nil".into(),
            ValueType::Closure(Closure { args, upvals, ret, generics: _ }) => {
                let mut s = String::new();
                s.push_str("fn[");
                for (i, v) in upvals.iter().enumerate() {
                    s.push_str(&v.to_string());
                    if i != upvals.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str("](");
                for (i, v) in args.iter().enumerate() {
                    s.push_str(&v.to_string());
                    if i != args.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") -> ");
                s.push_str(&ret.to_string());
                s.into()
            }
            ValueType::ExternalFn(ret, _) => {
                let mut s = String::new();
                s.push_str("fn");
                s.push_str(" -> ");
                s.push_str(&ret.to_string());
                s.into()
            }
            ValueType::Pointer(t, _) => format!("*{}", t.to_string()).into(),
            ValueType::LValue(t, _) => format!("&{}", t.to_string()).into(),
            ValueType::Array(t, s) => {
                if let Some(s) = s {
                    format!("[{}; {}]", t.to_string(), s).into()
                } else {
                    format!("[{}]", t.to_string()).into()
                }
            }
            ValueType::Struct(st) => {
                let mut s = String::new();
                s.push_str("struct ");
                s.push_str(&st.name);
                s.push_str(" { ");
                let bg = st.fields.borrow();
                for (k, v) in bg.iter() {
                    s.push_str(&format!("{}: {}, ", k, v.value.to_string()));
                }
                s.push('}');
                s.into()
            }
            ValueType::SelfStruct(s, v) => format!("self{:?} {}", v, s).into(),
            ValueType::GenericParam(n) => format!("<{n}>").into(),
            ValueType::Err => "err".into(),
            ValueType::TypeVar(i) => format!("${i}").into(),
        }
    }

    pub fn _has_size(&self) -> bool {
        match self {
            Self::Float
            | Self::Integer
            | Self::Char
            | Self::Bool
            | Self::Nil
            | Self::Pointer(_, _)
            | Self::ExternalFn(_, _)
            | Self::LValue(_, _)
            | Self::Err
            | ValueType::Closure(Closure { args: _, upvals: _, ret: _, generics: _ }) => true,
            Self::Array(t, n) => t._has_size() && n.is_some(),
            Self::Struct(h) => {
                let bg = h.fields.borrow();
                bg.iter().all(|(_, v)| v.value._has_size())
            }
            Self::SelfStruct(_, _) => false,
            Self::GenericParam(_) => false,
            Self::TypeVar(_) => false,
        }
    }

    pub fn fill_self_struct(
        &self,
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
                    Self::Struct(s.clone()).instantiate_generic(&mut tvs)
                } else {
                    Self::Err.intern()
                }
            }
            _ => self.clone().intern(),
        }
    }

    pub fn unique_string(&self) -> SharedString {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.hash(&mut hasher);
        format!("{}", hasher.finish()).into()
    }

    fn soft_compare(&self, other: &Self) -> bool {
        // if [self, other].into_iter().any(Self::is_all) {
        //     return true;
        // }
        match (self, other) {
            (Self::Pointer(_, _), Self::Pointer(nil, _))
            | (Self::Pointer(nil, _), Self::Pointer(_, _))
                if nil.is_nil() =>
            {
                true
            }
            (Self::Struct(l0), Self::Struct(r0)) => l0.name == r0.name,
            (ValueType::Closure(Closure { args: l0v, upvals: u0, ret: r0, generics: _ }), ValueType::Closure(Closure { args: r0v, upvals: u1, ret: r1, generics: _ })) => {
                l0v.len() == r0v.len()
                    && l0v.iter().zip(r0v.iter()).all(|(l0, r0)| l0 == r0)
                    && u0.len() == u1.len()
                    && u0.iter().zip(u1.iter()).all(|(l0, r0)| l0 == r0)
                    && r0 == r1
            }
            (Self::Array(l0, l0c), Self::Array(r0, r0c)) => l0 == r0 && (l0c == r0c),
            (Self::Pointer(l0, _), Self::Pointer(r0, _)) => l0 == r0,

            (Self::SelfStruct(l0, _v0), Self::SelfStruct(r0, _v1)) => l0 == r0,
            (Self::SelfStruct(l0, _v0), Self::Struct(r0)) => l0 == &r0.name,
            (Self::Struct(l0), Self::SelfStruct(r0, _v0)) => &l0.name == r0,

            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }

    pub fn llvm<'ctx>(&self, ctx: &'ctx Context, generics: &HashMap<SharedString, UValueType>) -> Result<inkwell::types::BasicTypeEnum<'ctx>> {
        Ok(match self.substitute(generics) {
            Self::Float => ctx.f64_type().as_basic_type_enum(),
            Self::Integer => ctx.i64_type().as_basic_type_enum(),
            Self::Char => ctx.i8_type().as_basic_type_enum(),
            Self::Bool => ctx.bool_type().as_basic_type_enum(),
            Self::Nil => ctx.i8_type().as_basic_type_enum(),
            ValueType::Closure(Closure { args: _, upvals, ret: _, generics: _ }) => {
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
                if let Some(v) = generics.get(p) {
                    v.llvm(ctx, generics)?
                } else {
                    return Err(anyhow::anyhow!("generic param <{p}> not resolved"));
                }
            }
            Self::Err => unreachable!("err type should be replaced by type"),
            ValueType::TypeVar(v) => return Err(anyhow::anyhow!("type var ${v} not resolved")),
        })
    }

    pub fn instantiate_generic(
        &self,
        generics: &mut HashMap<SharedString, UValueType>,
    ) -> UValueType {
        match self {
            Self::GenericParam(s) => {
                if let Some(v) = generics.get(s) {
                    v
                } else {
                    let tv = ValueType::new_type_var();
                    generics.insert(s.clone(), tv);
                    tv
                }
            }
            ValueType::Closure(Closure { args, upvals, ret, generics: _ }) => {
                let mut new_args = Vec::with_capacity(args.len());
                for x in args.iter() {
                    new_args.push(x.instantiate_generic(generics));
                }
                let mut new_upvals = Vec::with_capacity(upvals.len());
                for x in upvals.iter() {
                    new_upvals.push(x.instantiate_generic(generics));
                }
                let new_ret = ret.instantiate_generic(generics);
                Self::Closure(Closure::new(
                    new_args.into_boxed_slice(),
                    new_upvals.into_boxed_slice(),
                    new_ret,
                    generics.clone().into_iter().collect::<BTreeMap<_, _>>(),
                ))
                .intern()
            }
            Self::Pointer(t, b) => Self::Pointer(t.instantiate_generic(generics), *b).intern(),
            Self::LValue(t, b) => Self::LValue(t.instantiate_generic(generics), *b).intern(),
            Self::Array(t, n) => Self::Array(t.instantiate_generic(generics), *n).intern(),
            Self::Struct(s) => {
                let bg = s.fields.borrow();
                let mut new_fields = IndexMap::new();
                for (k, v) in bg.iter() {
                    new_fields.insert(
                        k.clone(),
                        StructEntry {
                            value: v.value.instantiate_generic(generics),
                            offset: v.offset,
                        },
                    );
                }

                Self::Struct(CustomStruct {
                    name: s.name.clone(),
                    fields: RefCell::new(new_fields),
                    embed: s.embed.clone(),
                    methods: s.methods.clone(),
                    type_vars: vec![].into(),
                })
                .intern()
            }
            Self::SelfStruct(s, v) => {
                let mut new_v = Vec::with_capacity(v.len());
                for x in v.iter() {
                    new_v.push(x.instantiate_generic(generics));
                }
                Self::SelfStruct(s.clone(), new_v).intern()
            }
            _ => self.intern(),
        }
    }
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        self.soft_compare(other)
    }
}

static USED_TYPES_ARENA: Arena<10000000> = Arena::new();

thread_local! {
    static USED_TYPES: RefCell<HashMap<SharedString, &'static ValueType>> =
        RefCell::new(HashMap::new());
}

impl ValueType {
    pub fn _decay(&self) -> UValueType {
        match self {
            Self::Array(p, _) => Self::Pointer(p, true).clone().intern(),
            _ => self.clone().intern(),
        }
    }

    pub fn intern(&self) -> UValueType {
        let sid = self.unique_string();

        USED_TYPES.with_borrow_mut(|types| {
            if let Some(t) = types.get(&sid) {
                return *t;
            }

            let value = USED_TYPES_ARENA.acquire(self.clone()).expect("arena full");
            types.insert(sid, value);
            value
        })
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
