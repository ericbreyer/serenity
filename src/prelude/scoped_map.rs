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
