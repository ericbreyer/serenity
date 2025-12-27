use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use anyhow::Result;

use crate::{prelude::*, typing::UValueType};

pub type Generics = ScopedMap<SharedString, UValueType>;

pub struct ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    variables: RefCell<VecDeque<HashMap<K, V>>>,
    as_hashmap: RefCell<HashMap<K, V>>,
    dirty: Cell<bool>,
}

impl<K, V> Debug for ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SM{:?}", self.as_hashmap())
    }
}

impl<K, V> Default for ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    pub fn new() -> Self {
        let mut v = Self {
            variables: VecDeque::new().into(),
            as_hashmap: HashMap::new().into(),
            dirty: true.into(),
        };
        v.begin_scope();
        v
    }

    pub fn begin_scope(&mut self) {
        self.variables.borrow_mut().push_front(HashMap::new());
    }
    pub fn end_scope(&mut self) {
        self.variables.borrow_mut().pop_front();
        self.dirty.set(true);
    }

    pub fn get(&self, name: impl Borrow<K>) -> Result<V> {
        let name = name.borrow();
        for scope in self.variables.borrow().iter() {
            if let Some(v) = scope.get(name) {
                return Ok(v.clone());
            }
        }
        Err(anyhow::anyhow!(
            "Variable {:?} not found in {:?}",
            name,
            self.as_hashmap().keys()
        ))
    }

    pub fn set(&mut self, name: K, value: V) {
        self.variables
            .borrow_mut()
            .front_mut()
            .unwrap()
            .insert(name, value);
        self.dirty.set(true);
    }

    pub fn as_hashmap(&self) -> HashMap<K, V> {
        if self.dirty.get() {
            let mut map = HashMap::new();
            for scope in self.variables.borrow().iter() {
                for (k, v) in scope.iter() {
                    map.insert(k.clone(), v.clone());
                }
            }
            self.as_hashmap.replace(map.clone());
            self.dirty.set(false);
        }
        self.as_hashmap.borrow().clone()
    }
}

impl<K, V> From<ScopedMap<K, V>> for HashMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    fn from(val: ScopedMap<K, V>) -> Self {
        val.as_hashmap()
    }
}

impl<K, V> Iterator for ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        self.as_hashmap().into_iter().next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_scope() {
        let map: ScopedMap<String, i32> = ScopedMap::new();
        assert!(!map.variables.borrow().is_empty());
    }

    #[test]
    fn test_set_and_get() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("x".to_string(), 42);
        let val = map.get("x".to_string()).unwrap();
        assert_eq!(val, 42);
    }

    #[test]
    fn test_get_nonexistent() {
        let map: ScopedMap<String, i32> = ScopedMap::new();
        let result = map.get("nonexistent".to_string());
        assert!(result.is_err());
    }

    #[test]
    fn test_scope_nesting() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("x".to_string(), 1);

        map.begin_scope();
        map.set("y".to_string(), 2);
        assert_eq!(map.get("x".to_string()).unwrap(), 1);
        assert_eq!(map.get("y".to_string()).unwrap(), 2);

        map.end_scope();
        assert_eq!(map.get("x".to_string()).unwrap(), 1);
        assert!(map.get("y".to_string()).is_err());
    }

    #[test]
    fn test_scope_shadowing() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("x".to_string(), 1);

        map.begin_scope();
        map.set("x".to_string(), 2);
        assert_eq!(map.get("x".to_string()).unwrap(), 2);

        map.end_scope();
        assert_eq!(map.get("x".to_string()).unwrap(), 1);
    }

    #[test]
    fn test_multiple_nested_scopes() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("a".to_string(), 1);

        map.begin_scope();
        map.set("b".to_string(), 2);

        map.begin_scope();
        map.set("c".to_string(), 3);
        assert_eq!(map.get("a".to_string()).unwrap(), 1);
        assert_eq!(map.get("b".to_string()).unwrap(), 2);
        assert_eq!(map.get("c".to_string()).unwrap(), 3);

        map.end_scope();
        assert_eq!(map.get("a".to_string()).unwrap(), 1);
        assert_eq!(map.get("b".to_string()).unwrap(), 2);
        assert!(map.get("c".to_string()).is_err());

        map.end_scope();
        assert_eq!(map.get("a".to_string()).unwrap(), 1);
        assert!(map.get("b".to_string()).is_err());
    }

    #[test]
    fn test_as_hashmap() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("x".to_string(), 1);
        map.set("y".to_string(), 2);

        let hashmap = map.as_hashmap();
        assert_eq!(hashmap.get("x"), Some(&1));
        assert_eq!(hashmap.get("y"), Some(&2));
        assert_eq!(hashmap.len(), 2);
    }

    #[test]
    fn test_as_hashmap_with_scopes() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("x".to_string(), 1);

        map.begin_scope();
        map.set("y".to_string(), 2);

        let hashmap = map.as_hashmap();
        assert_eq!(hashmap.get("x"), Some(&1));
        assert_eq!(hashmap.get("y"), Some(&2));
    }

    #[test]
    fn test_into_hashmap() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        map.set("x".to_string(), 10);
        map.set("y".to_string(), 20);

        let hashmap: HashMap<String, i32> = map.into();
        assert_eq!(hashmap.get("x"), Some(&10));
        assert_eq!(hashmap.get("y"), Some(&20));
    }

    #[test]
    fn test_default() {
        let map: ScopedMap<String, i32> = ScopedMap::default();
        assert!(!map.variables.borrow().is_empty());
    }

    #[test]
    fn test_dirty_flag() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        assert!(map.dirty.get());

        map.set("x".to_string(), 1);
        let _ = map.as_hashmap();
        assert!(!map.dirty.get());

        map.set("y".to_string(), 2);
        assert!(map.dirty.get());
    }

    #[test]
    fn test_complex_scenario() {
        let mut map: ScopedMap<String, String> = ScopedMap::new();

        // Global scope
        map.set("global".to_string(), "value1".to_string());

        // Function scope 1
        map.begin_scope();
        map.set("local1".to_string(), "value2".to_string());

        // Inner block scope
        map.begin_scope();
        map.set("innerLocal".to_string(), "value3".to_string());
        assert_eq!(map.get("global".to_string()).unwrap(), "value1".to_string());
        assert_eq!(map.get("local1".to_string()).unwrap(), "value2".to_string());
        assert_eq!(
            map.get("innerLocal".to_string()).unwrap(),
            "value3".to_string()
        );
        map.end_scope();

        // Back to function scope 1
        assert_eq!(map.get("global".to_string()).unwrap(), "value1".to_string());
        assert_eq!(map.get("local1".to_string()).unwrap(), "value2".to_string());
        assert!(map.get("innerLocal".to_string()).is_err());

        map.end_scope();

        // Back to global scope
        assert_eq!(map.get("global".to_string()).unwrap(), "value1".to_string());
        assert!(map.get("local1".to_string()).is_err());
    }

    #[test]
    fn test_multiple_keys_same_scope() {
        let mut map: ScopedMap<String, i32> = ScopedMap::new();
        for i in 0..100 {
            map.set(format!("key_{}", i), i);
        }

        for i in 0..100 {
            assert_eq!(map.get(format!("key_{}", i)).unwrap(), i);
        }
    }
}
