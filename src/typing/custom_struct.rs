use super::*;

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
            v.value.id_str().hash(state);
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
    pub fn llvm(
        &self,
        ctx: &'ctx Context,
        generics: &HashMap<SharedString, UValueType>,
    ) -> Result<inkwell::types::StructType<'ctx>> {
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
