use super::*;

thread_local! {
    static TVAR: Cell<usize> = 0.into();
    static SUBSTITUTIONS: RefCell<Vec<UValueType>> = vec![ValueType::TypeVar(0).intern()].into();
    static CONSTRAINTS: RefCell<Vec<Box<[Constraint]>>> = vec![vec![Constraint(None)].into_boxed_slice()].into();
}

impl ValueType {
    /// # New Type Var
    /// Create a new type var with a unique id
    pub fn new_type_var(constraints: Box<[Constraint]>) -> UValueType {
        ValueType::TypeVar(TVAR.with(|u| {
            u.set(u.get() + 1);
            SUBSTITUTIONS.with_borrow_mut(|v| v.push(ValueType::TypeVar(u.get()).intern()));
            CONSTRAINTS.with_borrow_mut(|c| c.push(constraints));
            u.get()
        }))
        .intern()
    }

    pub fn get_constraints(&'static self) -> Option<Box<[Constraint]>> {
        println!("get_constraints for {}", self);
        match self {
            ValueType::TypeVar(x) => CONSTRAINTS.with_borrow(|c| c.get(*x).cloned()),
            _ => None,
        }
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
            (ValueType::GenericParam(s0, _), _) | (_, ValueType::GenericParam(s0, _)) => {
                if let Ok(v) = generics.get(s0) {
                    ValueType::unify(v, t2, generics)?;
                } else {
                    return Err(anyhow::anyhow!("generic param <{s0}> not resolved"));
                }
            }

            // Type vars
            // If the type var already has a substitution, unify the substitution with the other
            // type
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

                // check for constraints
                if let Some(c) = CONSTRAINTS.with_borrow(|c| c.get(*x).cloned()) {
                    if !t.satisfies_constraints(&c, generics) {
                        return Err(anyhow::anyhow!(
                            "type {} does not satisfy constraints {:?}",
                            t.id_str(),
                            c
                        ));
                    }
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
            (ValueType::SelfStructRef(s0, v0), ValueType::SelfStructRef(s1, v1)) => {
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
    /// value in the hashmap. If the generic parameter is not found in the
    /// hashmap, it is left as is.
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
                    implements: s.implements.clone(),
                }))
                .intern()
            }
            ValueType::SelfStructRef(s, v) => Self::SelfStructRef(
                s.clone(),
                v.iter().map(|t| t.substitute(generics)).collect(),
            )
            .intern(),
            ValueType::GenericParam(s, _) => {
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
            Self::GenericParam(s, c) => {
                if let Some(v) = generics.get(s) {
                    v
                } else {
                    let tv = ValueType::new_type_var(c.clone());
                    generics.insert(s.clone(), tv);
                    println!(
                        "Instantiated generic {s} {c:?} to {tv} {:?}",
                        tv.get_constraints()
                    );
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
                    type_vars: IndexMap::new().into(),
                    implements: s.implements.clone(),
                }))
                .intern()
            }
            Self::SelfStructRef(s, v) => {
                let mut new_v = Vec::with_capacity(v.len());
                for x in v.iter() {
                    new_v.push(x.instantiate_generic(generics));
                }
                Self::SelfStructRef(s.clone(), new_v).intern()
            }
            _ => self,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::ScopedMap;

    #[test]
    fn test_new_type_var_creates_unique_ids() {
        let tv1 = ValueType::new_type_var(vec![].into_boxed_slice());
        let tv2 = ValueType::new_type_var(vec![].into_boxed_slice());

        match (tv1, tv2) {
            (ValueType::TypeVar(id1), ValueType::TypeVar(id2)) => {
                assert_ne!(id1, id2, "Type vars should have unique IDs");
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_new_type_var_with_constraints() {
        let constraints = vec![Constraint(Some("Numeric".into()))].into_boxed_slice();
        let tv = ValueType::new_type_var(constraints.clone());

        match tv {
            ValueType::TypeVar(id) => {
                let retrieved = CONSTRAINTS.with_borrow(|c| c.get(*id).cloned());
                assert!(retrieved.is_some());
                assert_eq!(retrieved.unwrap().len(), constraints.len());
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_unify_identical_type_vars() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let generics = ScopedMap::new();

        let result = ValueType::unify(tv, tv, &generics);
        assert!(result.is_ok());
    }

    #[test]
    fn test_unify_pointer_with_integer() {
        let ptr = ValueType::Pointer(ValueType::Integer.intern(), false).intern();
        let int = ValueType::Integer.intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(ptr, int, &generics);
        assert!(result.is_ok(), "Pointer and Integer should unify");
    }

    #[test]
    fn test_unify_integer_with_pointer() {
        let int = ValueType::Integer.intern();
        let ptr = ValueType::Pointer(ValueType::Integer.intern(), false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(int, ptr, &generics);
        assert!(result.is_ok(), "Integer and Pointer should unify");
    }

    #[test]
    fn test_unify_type_var_with_concrete_type() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let int = ValueType::Integer.intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(tv, int, &generics);
        assert!(result.is_ok());

        // Check that the type var was substituted
        let substituted = tv.substitute(&generics);
        assert_eq!(substituted, int);
    }

    #[test]
    fn test_unify_concrete_type_with_type_var() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let int = ValueType::Integer.intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(int, tv, &generics);
        assert!(result.is_ok());

        // Check that the type var was substituted
        let substituted = tv.substitute(&generics);
        assert_eq!(substituted, int);
    }

    #[test]
    fn test_unify_occurs_check_prevents_infinite_types() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let ptr_to_tv = ValueType::Pointer(tv, false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(tv, ptr_to_tv, &generics);
        assert!(
            result.is_err(),
            "Occurs check should prevent infinite types"
        );
    }

    #[test]
    fn test_unify_pointers_with_nil() {
        let ptr1 = ValueType::Pointer(ValueType::Integer.intern(), false).intern();
        let nil_ptr = ValueType::Pointer(ValueType::Nil.intern(), false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(ptr1, nil_ptr, &generics);
        assert!(result.is_ok(), "Pointer should unify with nil pointer");
    }

    #[test]
    fn test_unify_nil_with_pointers() {
        let nil_ptr = ValueType::Pointer(ValueType::Nil.intern(), false).intern();
        let ptr1 = ValueType::Pointer(ValueType::Float.intern(), false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(nil_ptr, ptr1, &generics);
        assert!(result.is_ok(), "Nil pointer should unify with any pointer");
    }

    #[test]
    fn test_unify_compatible_pointers() {
        let ptr1 = ValueType::Pointer(ValueType::Integer.intern(), false).intern();
        let ptr2 = ValueType::Pointer(ValueType::Integer.intern(), false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(ptr1, ptr2, &generics);
        assert!(result.is_ok());
    }

    #[test]
    fn test_unify_incompatible_pointers() {
        let ptr1 = ValueType::Pointer(ValueType::Integer.intern(), false).intern();
        let ptr2 = ValueType::Pointer(ValueType::Float.intern(), false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(ptr1, ptr2, &generics);
        assert!(
            result.is_err(),
            "Incompatible pointer types should not unify"
        );
    }

    #[test]
    fn test_unify_lvalues() {
        let lval1 = ValueType::LValue(ValueType::Integer.intern(), false).intern();
        let lval2 = ValueType::LValue(ValueType::Integer.intern(), false).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(lval1, lval2, &generics);
        assert!(result.is_ok());
    }

    #[test]
    fn test_unify_arrays_same_size() {
        let arr1 = ValueType::Array(ValueType::Integer.intern(), Some(5)).intern();
        let arr2 = ValueType::Array(ValueType::Integer.intern(), Some(5)).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(arr1, arr2, &generics);
        assert!(result.is_ok());
    }

    #[test]
    fn test_unify_arrays_different_sizes() {
        let arr1 = ValueType::Array(ValueType::Integer.intern(), Some(5)).intern();
        let arr2 = ValueType::Array(ValueType::Integer.intern(), Some(10)).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(arr1, arr2, &generics);
        assert!(
            result.is_err(),
            "Arrays with different sizes should not unify"
        );
    }

    #[test]
    fn test_unify_arrays_unknown_size_with_known() {
        let arr1 = ValueType::Array(ValueType::Integer.intern(), None).intern();
        let arr2 = ValueType::Array(ValueType::Integer.intern(), Some(10)).intern();
        let generics = ScopedMap::new();

        let result = ValueType::unify(arr1, arr2, &generics);
        assert!(
            result.is_ok(),
            "Array with unknown size should unify with known size"
        );
    }

    #[test]
    fn test_unify_closures_different_arg_count() {
        let closure1 = ValueType::Closure(Closure::new(
            vec![ValueType::Integer.intern()].into_boxed_slice(),
            vec![].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        let closure2 = ValueType::Closure(Closure::new(
            vec![ValueType::Integer.intern(), ValueType::Float.intern()].into_boxed_slice(),
            vec![].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        let generics = ScopedMap::new();
        let result = ValueType::unify(closure1, closure2, &generics);
        assert!(
            result.is_err(),
            "Closures with different arg counts should not unify"
        );
    }

    #[test]
    fn test_unify_closures_different_upval_count() {
        let closure1 = ValueType::Closure(Closure::new(
            vec![ValueType::Integer.intern()].into_boxed_slice(),
            vec![ValueType::Integer.intern()].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        let closure2 = ValueType::Closure(Closure::new(
            vec![ValueType::Integer.intern()].into_boxed_slice(),
            vec![].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        let generics = ScopedMap::new();
        let result = ValueType::unify(closure1, closure2, &generics);
        assert!(
            result.is_err(),
            "Closures with different upval counts should not unify"
        );
    }

    #[test]
    fn test_unify_closures_compatible() {
        let closure1 = ValueType::Closure(Closure::new(
            vec![ValueType::Integer.intern()].into_boxed_slice(),
            vec![].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        let closure2 = ValueType::Closure(Closure::new(
            vec![ValueType::Integer.intern()].into_boxed_slice(),
            vec![].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        let generics = ScopedMap::new();
        let result = ValueType::unify(closure1, closure2, &generics);
        assert!(result.is_ok());
    }

    #[test]
    fn test_occurs_in_simple_type_var() {
        let tv1 = ValueType::new_type_var(vec![].into_boxed_slice());

        match tv1 {
            ValueType::TypeVar(id1) => {
                assert!(ValueType::occurs_in(*id1, tv1));
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_occurs_in_pointer() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let ptr = ValueType::Pointer(tv, false).intern();

        match tv {
            ValueType::TypeVar(id) => {
                assert!(ValueType::occurs_in(*id, ptr));
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_occurs_in_array() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let arr = ValueType::Array(tv, Some(10)).intern();

        match tv {
            ValueType::TypeVar(id) => {
                assert!(ValueType::occurs_in(*id, arr));
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_occurs_in_closure_args() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let closure = ValueType::Closure(Closure::new(
            vec![tv].into_boxed_slice(),
            vec![].into_boxed_slice(),
            ValueType::Integer.intern(),
            BTreeMap::new(),
        ))
        .intern();

        match tv {
            ValueType::TypeVar(id) => {
                assert!(ValueType::occurs_in(*id, closure));
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_occurs_in_closure_ret() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let closure = ValueType::Closure(Closure::new(
            vec![].into_boxed_slice(),
            vec![].into_boxed_slice(),
            tv,
            BTreeMap::new(),
        ))
        .intern();

        match tv {
            ValueType::TypeVar(id) => {
                assert!(ValueType::occurs_in(*id, closure));
            }
            _ => panic!("Expected TypeVar"),
        }
    }

    #[test]
    fn test_substitute_primitives() {
        let generics = ScopedMap::new();

        assert_eq!(
            ValueType::Integer.substitute(&generics),
            ValueType::Integer.intern()
        );
        assert_eq!(
            ValueType::Float.substitute(&generics),
            ValueType::Float.intern()
        );
        assert_eq!(
            ValueType::Bool.substitute(&generics),
            ValueType::Bool.intern()
        );
        assert_eq!(
            ValueType::Char.substitute(&generics),
            ValueType::Char.intern()
        );
        assert_eq!(
            ValueType::Nil.substitute(&generics),
            ValueType::Nil.intern()
        );
    }

    #[test]
    fn test_substitute_pointer() {
        let generics = ScopedMap::new();
        let ptr = ValueType::Pointer(ValueType::Integer.intern(), false).intern();

        let result = ptr.substitute(&generics);
        assert_eq!(result, ptr);
    }

    #[test]
    fn test_substitute_array() {
        let generics = ScopedMap::new();
        let arr = ValueType::Array(ValueType::Float.intern(), Some(5)).intern();

        let result = arr.substitute(&generics);
        assert_eq!(result, arr);
    }

    #[test]
    fn test_substitute_generic_param() {
        let mut generics = ScopedMap::new();
        let param_name: SharedString = "T".into();
        generics.set(param_name.clone(), ValueType::Integer.intern());

        let generic_param =
            ValueType::GenericParam(param_name.clone(), vec![].into_boxed_slice()).intern();
        let result = generic_param.substitute(&generics);

        assert_eq!(result, ValueType::Integer.intern());
    }

    #[test]
    fn test_substitute_generic_param_not_in_scope() {
        let generics = ScopedMap::new();
        let param_name: SharedString = "T".into();

        let generic_param = ValueType::GenericParam(param_name, vec![].into_boxed_slice()).intern();
        let result = generic_param.substitute(&generics);

        // Should return itself when not found
        assert_eq!(result, generic_param);
    }

    #[test]
    fn test_substitute_type_var_with_substitution() {
        let tv = ValueType::new_type_var(vec![].into_boxed_slice());
        let generics = ScopedMap::new();

        // Set up a substitution
        ValueType::unify(tv, ValueType::Integer.intern(), &generics).unwrap();

        let result = tv.substitute(&generics);
        assert_eq!(result, ValueType::Integer.intern());
    }

    #[test]
    fn test_substitute_closure() {
        let mut generics = ScopedMap::new();
        let param_name: SharedString = "T".into();
        generics.set(param_name.clone(), ValueType::Integer.intern());

        let generic_param = ValueType::GenericParam(param_name, vec![].into_boxed_slice()).intern();
        let closure = ValueType::Closure(Closure::new(
            vec![generic_param].into_boxed_slice(),
            vec![].into_boxed_slice(),
            generic_param,
            BTreeMap::new(),
        ))
        .intern();

        let result = closure.substitute(&generics);

        match result {
            ValueType::Closure(c) => {
                assert_eq!(c.args[0], ValueType::Integer.intern());
                assert_eq!(c.ret, ValueType::Integer.intern());
            }
            _ => panic!("Expected Closure"),
        }
    }

    #[test]
    fn test_instantiate_generic_new_type_var() {
        let mut generics = HashMap::new();
        let param_name: SharedString = "T".into();

        let generic_param =
            ValueType::GenericParam(param_name.clone(), vec![].into_boxed_slice()).intern();
        let result = generic_param.instantiate_generic(&mut generics);

        // Should create a new type var
        assert!(matches!(result, ValueType::TypeVar(_)));
        assert!(generics.contains_key(&param_name));
    }

    #[test]
    fn test_instantiate_generic_reuses_existing() {
        let mut generics = HashMap::new();
        let param_name: SharedString = "T".into();

        let generic_param =
            ValueType::GenericParam(param_name.clone(), vec![].into_boxed_slice()).intern();
        let result1 = generic_param.instantiate_generic(&mut generics);
        let result2 = generic_param.instantiate_generic(&mut generics);

        // Should reuse the same type var
        assert_eq!(result1, result2);
    }

    #[test]
    fn test_instantiate_generic_pointer() {
        let mut generics = HashMap::new();
        let param_name: SharedString = "T".into();

        let generic_param =
            ValueType::GenericParam(param_name.clone(), vec![].into_boxed_slice()).intern();
        let ptr = ValueType::Pointer(generic_param, false).intern();
        let result = ptr.instantiate_generic(&mut generics);

        match result {
            ValueType::Pointer(inner, _) => {
                assert!(matches!(inner, ValueType::TypeVar(_)));
            }
            _ => panic!("Expected Pointer"),
        }
    }

    #[test]
    fn test_instantiate_generic_array() {
        let mut generics = HashMap::new();
        let param_name: SharedString = "T".into();

        let generic_param =
            ValueType::GenericParam(param_name.clone(), vec![].into_boxed_slice()).intern();
        let arr = ValueType::Array(generic_param, Some(10)).intern();
        let result = arr.instantiate_generic(&mut generics);

        match result {
            ValueType::Array(inner, size) => {
                assert!(matches!(inner, ValueType::TypeVar(_)));
                assert_eq!(*size, Some(10));
            }
            _ => panic!("Expected Array"),
        }
    }

    #[test]
    fn test_unify_mismatched_primitives() {
        let generics = ScopedMap::new();

        let result = ValueType::unify(
            ValueType::Integer.intern(),
            ValueType::Float.intern(),
            &generics,
        );
        assert!(result.is_err(), "Integer and Float should not unify");
    }

    #[test]
    fn test_unify_chain_type_vars() {
        let tv1 = ValueType::new_type_var(vec![].into_boxed_slice());
        let tv2 = ValueType::new_type_var(vec![].into_boxed_slice());
        let generics = ScopedMap::new();

        // Unify tv1 with tv2
        ValueType::unify(tv1, tv2, &generics).unwrap();
        // Then unify tv2 with Integer
        ValueType::unify(tv2, ValueType::Integer.intern(), &generics).unwrap();

        // Both should now substitute to Integer
        assert_eq!(tv1.substitute(&generics), ValueType::Integer.intern());
        assert_eq!(tv2.substitute(&generics), ValueType::Integer.intern());
    }
}
