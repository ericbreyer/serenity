use std::cell::{Cell, RefCell};
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

use crate::prelude::*;
use crate::typing::UValueType;

use anyhow::Result;

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

impl<K, V> ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    pub fn new() -> Self {
        let v = Self {
            variables: VecDeque::new().into(),
            as_hashmap: HashMap::new().into(),
            dirty: true.into(),
        };
        v.begin_scope();
        v
    }

    pub fn begin_scope(&self) {
        self.variables.borrow_mut().push_front(HashMap::new());
    }
    pub fn end_scope(&self) {
        self.variables.borrow_mut().pop_front();
        self.dirty.set(true);
    }

    pub fn get(&self, name: K) -> Result<V> {
        for scope in self.variables.borrow().iter() {
            if let Some(v) = scope.get(&name) {
                return Ok(v.clone());
            }
        }
        Err(anyhow::anyhow!("Variable {:?} not found", name))
    }

    pub fn set(&self, name: K, value: V) {
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

impl<K, V> Into<HashMap<K, V>> for ScopedMap<K, V>
where
    K: Eq + Hash + Debug + Clone,
    V: Clone,
{
    fn into(self) -> HashMap<K, V> {
        self.as_hashmap()
    }
}