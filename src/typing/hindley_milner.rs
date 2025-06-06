use super::*;

thread_local! {
    static TVAR: Cell<usize> = 0.into();
    static SUBSTITUTIONS: RefCell<Vec<UValueType>> = vec![ValueType::TypeVar(0).intern()].into();
}

impl ValueType {
    /// # New Type Var
    /// Create a new type var with a unique id
    pub fn new_type_var() -> UValueType {
        ValueType::TypeVar(TVAR.with(|u| {
            u.set(u.get() + 1);
            SUBSTITUTIONS.with_borrow_mut(|v| v.push(ValueType::TypeVar(u.get()).intern()));
            u.get()
        }))
        .intern()
    }

    /// # Unify
    /// Unify two types
    ///
    /// This function takes two types and a hashmap of generic substitutions
    /// and attempts to unify the two types. If the types are unifiable, the
    /// function returns `Ok(())` and updates the type var substitutions. If the
    /// types are not unifiable, the function returns an error.
    ///
    /// ## Arguments
    /// - `t1` - The first type to unify
    /// - `t2` - The second type to unify
    /// - `generics` - A hashmap of generic substitutions
    ///
    /// ### Restrictions
    /// Neither `t1` nor `t2` should contain any generic parameters that are
    /// not resolved in the `generics` hashmap
    pub fn unify(
        t1: UValueType,
        t2: UValueType,
        generics: &ScopedMap<SharedString, UValueType>,
    ) -> Result<()> {
        match (t1.substitute(generics), t2.substitute(generics)) {
            // Chill stuff, fo sho
            (ValueType::TypeVar(x), ValueType::TypeVar(y)) if x == y => {}
            (ValueType::Pointer(_, _), ValueType::Integer)
            | (ValueType::Integer, ValueType::Pointer(_, _)) => {}

            // Generic params should be resolved
            (ValueType::GenericParam(s0), _) | (_, ValueType::GenericParam(s0)) => {
                if let Ok(v) = generics.get(s0) {
                    ValueType::unify(v, t2, generics)?;
                } else {
                    return Err(anyhow::anyhow!("generic param <{s0}> not resolved"));
                }
            }

            // Type vars
            // If the type var already has a substitution, unify the substitution with the other type
            (ValueType::TypeVar(x), t) | (t, ValueType::TypeVar(x))
                if SUBSTITUTIONS.with_borrow(|v| v[*x]) != &ValueType::TypeVar(*x) =>
            {
                ValueType::unify(SUBSTITUTIONS.with_borrow(|v| v[*x]), t, generics)?;
            }
            // Otherwise, set the substitution to the other type (if it doesn't cause a cycle)
            (ValueType::TypeVar(x), t) | (t, ValueType::TypeVar(x)) => {
                if ValueType::occurs_in(*x, t) {
                    return Err(anyhow::anyhow!("occurs check failed"));
                }

                SUBSTITUTIONS.with_borrow_mut(|v| v[*x] = t);
            }

            // Unify structural types recursively (structs, arrays, closures)
            (ValueType::Closure(c0), ValueType::Closure(c1)) => {
                if c0.args.len() != c1.args.len() || c0.upvals.len() != c1.upvals.len() {
                    return Err(anyhow::anyhow!(
                        "closure types do not match {:?} {:?}",
                        t1,
                        t2
                    ));
                }
                for (x, y) in c0.upvals.iter().zip(c1.upvals.iter()) {
                    ValueType::unify(x, y, generics)?;
                }
                for (x, y) in c0.args.iter().zip(c1.args.iter()) {
                    ValueType::unify(x, y, generics)?;
                }
                ValueType::unify(c0.ret, c1.ret, generics)?;
            }
            (ValueType::Pointer(p0, _), ValueType::Pointer(p1, _)) => {
                if *p0 == &ValueType::Nil || *p1 == &ValueType::Nil {
                    return Ok(());
                }
                ValueType::unify(p0, p1, generics)?;
            }
            (ValueType::LValue(p0, _), ValueType::LValue(p1, _)) => {
                ValueType::unify(p0, p1, generics)?;
            }
            (ValueType::Array(p0, n0), ValueType::Array(p1, n1)) => {
                if n0.unwrap_or(n1.unwrap_or(usize::MAX)) != n1.unwrap_or(usize::MAX) {
                    return Err(anyhow::anyhow!("array types do not match"));
                }
                ValueType::unify(p0, p1, generics)?;
            }
            (ValueType::Struct(s0), ValueType::Struct(s1)) => {
                let (f0, f1) = (s0.fields.borrow(), s1.fields.borrow());
                if f0.len() != f1.len() || s0.name != s1.name {
                    return Err(anyhow::anyhow!("struct types do not match"));
                }
                for (v0, v1) in f0
                    .iter()
                    .map(|(k, v)| f1.get(k).map(|v1| (v, v1)))
                    .collect::<Option<Vec<_>>>()
                    .context("struct types do not match")?
                {
                    ValueType::unify(v0.value, v1.value, generics)?;
                }
            }
            (ValueType::SelfStruct(s0, v0), ValueType::SelfStruct(s1, v1)) => {
                if s0 != s1 || v0.len() != v1.len() {
                    return Err(anyhow::anyhow!("self struct types do not match"));
                }
                for (x, y) in v0.iter().zip(v1.iter()) {
                    ValueType::unify(x, y, generics)?;
                }
            }

            // If we can't unify the types, return an error
            (a, b) if a != b => {
                return Err(anyhow::anyhow!("types do not match {:#?} {:#?}", a, b));
            }
            // Otherwise, types match we good
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

    /// # Substitute
    /// Substitute all type vars in the type with their substitutions
    ///
    /// This function substitutes
    /// all type vars in the type with their substitutions. If a type var does
    /// not have a substitution, it is left as is.
    ///
    /// It also takes an optional hashmap of generic substitutions. If a generic
    /// parameter is found in the type, it is substituted with the corresponding
    /// value in the hashmap. If the generic parameter is not found in the hashmap,
    /// it is left as is.
    pub fn substitute<'a>(
        &'static self,
        generics: impl Into<Option<&'a ScopedMap<SharedString, UValueType>>>,
    ) -> UValueType {
        let empty = ScopedMap::new();
        let generics = generics.into().unwrap_or(&empty);
        let new = match self {
            ValueType::Float
            | ValueType::Integer
            | ValueType::Char
            | ValueType::Bool
            | ValueType::Nil
            | ValueType::Err => self,
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
            ValueType::ExternalFn(r, n) => {
                Self::ExternalFn(r.substitute(generics), n.clone()).intern()
            }
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

                Self::Struct(Box::new(CustomStruct {
                                    name: s.name.clone(),
                                    fields: RefCell::new(new_fields),
                                    embed: s.embed.clone(),
                                    methods: s.methods.clone(),
                                    parametric_methods: s.parametric_methods.clone(),
                                    type_vars: s.type_vars.borrow().clone().into(),
                                }))
                .intern()
            }
            ValueType::SelfStruct(s, v) => Self::SelfStruct(
                s.clone(),
                v.iter().map(|t| t.substitute(generics)).collect(),
            )
            .intern(),
            ValueType::GenericParam(s) => {
                if let Ok(v) = generics.get(s) {
                    v
                } else {
                    self
                }
            }
            ValueType::TypeVar(x) => SUBSTITUTIONS.with_borrow(|v| v[*x]),
        };
        // if the result is a type var, we need to try to substitute it again
        if let ValueType::TypeVar(x) = new {
            // if the type var has a valid substitution, substitute it
            if SUBSTITUTIONS.with_borrow(|v| v[*x]) != &ValueType::TypeVar(*x) {
                let new_new = new.substitute(generics);
                // also update the substitution to the new substitution
                // to avoid recomputing the same thing
                SUBSTITUTIONS.with_borrow_mut(|v| v[*x] = new_new);
                return new_new;
            }
        };
        new
    }

    pub fn instantiate_generic(
        &'static self,
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
            ValueType::Closure(Closure {
                args,
                upvals,
                ret,
                generics: _,
            }) => {
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
                    generics
                        .iter()
                        .map(|(k, v)| (k.clone(), *v))
                        .collect::<BTreeMap<_, _>>(),
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

                Self::Struct(Box::new(CustomStruct {
                                    name: s.name.clone(),
                                    fields: RefCell::new(new_fields),
                                    embed: s.embed.clone(),
                                    methods: s.methods.clone(),
                                    parametric_methods: s.parametric_methods.clone(),
                                    type_vars: vec![].into(),
                                }))
                .intern()
            }
            Self::SelfStruct(s, v) => {
                let mut new_v = Vec::with_capacity(v.len());
                for x in v.iter() {
                    new_v.push(x.instantiate_generic(generics));
                }
                Self::SelfStruct(s.clone(), new_v).intern()
            }
            _ => self,
        }
    }
}
