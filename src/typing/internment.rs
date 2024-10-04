use super::*;

static USED_TYPES_ARENA: Arena<1000000> = Arena::new();

thread_local! {
    static USED_TYPES: RefCell<HashMap<SharedString, UValueType>> =
        RefCell::new(HashMap::new());
}

impl ValueType {
    /// # Intern
    /// Intern a type
    ///
    /// This function takes a type and interns it.
    ///
    /// If the type has already been interned, the function returns the interned 
    /// type. If the type has not been interned, the function places the type 
    /// in static memory and returns a shared reference to it.
    ///
    /// This function is used to ensure that all types are unique and that
    /// the same type is not duplicated in memory. It also allows us to avoid 
    /// needless clones when passing types around, by ensuring that all equivalant 
    /// types are references to the same memory location. The static lifetime 
    /// allows us to bypass Rc and Arc, which would generate a lot of overhead.
    ///
    /// ## Arguments
    /// - `self` - The type to intern, taken by value as to avoid needless re-interning
    ///
    pub fn intern(self) -> UValueType {
        // generate a unique string for the type
        let sid = self.id_str();

        // check if the type has already been interned in the hashmap
        // if it has, return the interned type
        // if it hasn't, intern the type and return the interned type
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