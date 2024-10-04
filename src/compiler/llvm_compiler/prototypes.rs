use super::*;


impl<'a, 'ctx> LLVMFunctionCompiler<'a, 'ctx> {
    /// Construct the serenity type of a function from a prototype
    ///
    /// # Arguments
    /// * `self` - The LLVMFunctionCompiler
    /// * `prototype` - The prototype of the function
    ///
    /// # Returns
    /// The serenity type of the function
    pub (super) fn function_type_serenity(&self, prototype: &Prototype) -> UValueType {
        ValueType::Closure(Closure::new(
            prototype
                .params
                .iter()
                .map(|(_s, t,_)| (t.decay().instantiate_generic(&mut self.generics_in_scope.as_hashmap())))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype
                .captures
                .iter()
                .map(|capture| self.get_variable(capture).unwrap().1.instantiate_generic(&mut self.generics_in_scope.as_hashmap()))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype.return_type.instantiate_generic(&mut self.generics_in_scope.as_hashmap()),
            None
        ))
        .intern()
    }

    /// Construct the LLVM type of a closure from a prototype
    /// A closure is a struct containing the captures and a pointer to the function
    ///
    /// # Arguments
    /// * `self` - The LLVMFunctionCompiler
    /// * `prototype` - The prototype of the function
    ///
    /// # Returns
    /// The LLVM type of the closure
    ///
    /// # Errors
    /// If the LLVM type of a capture cannot be constructed
    pub (super) fn closure_type_llvm(&self, prototype: &Prototype) -> Result<StructType<'ctx>> {
        Ok(self.context.struct_type(
            [
                prototype
                    .captures
                    .iter()
                    .map(|capture| {
                        self.get_variable(capture)
                            .unwrap()
                            .1
                            .llvm(self.context, &self.generics_in_scope.as_hashmap())
                    })
                    .collect::<Result<Vec<_>>>()?
                    .as_slice(),
                &[self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()],
            ]
            .concat()
            .as_slice(),
            false,
        ))
    }

    /// Construct the LLVM type of a function from a prototype
    /// A function is a pointer to a function with the captures as the first arguments
    /// and the function arguments as the rest
    ///
    /// # Arguments
    /// * `self` - The LLVMFunctionCompiler
    /// * `prototype` - The prototype of the function
    ///
    /// # Returns
    /// The LLVM type of the function
    ///
    /// # Errors
    /// If an LLVM type cannot be constructed
    pub (super) fn function_prototype_llvm(&self, prototype: &Prototype) -> Result<FunctionType<'ctx>> {
        Ok(prototype
            .return_type
            .substitute(None)
            .llvm(self.context, &self.generics_in_scope.as_hashmap())
            .context("Function return type")?
            .fn_type(
                prototype
                    .captures
                    .iter()
                    .map(|capture| self.get_variable(capture).unwrap().1)
                    .map(|t| t.llvm(self.context, &self.generics_in_scope.as_hashmap()))
                    .map(|t| Ok(BasicMetadataTypeEnum::from(t?)))
                    .chain(prototype.params.iter().map(|(_s, t,_)| {
                        Ok(BasicMetadataTypeEnum::from(
                            t.substitute(None).decay().llvm(self.context, &self.generics_in_scope.as_hashmap())?,
                        ))
                    }))
                    .collect::<Result<Vec<_>>>()?
                    .as_slice(),
                false,
            ))
    }
}