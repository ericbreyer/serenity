use tracing::{ instrument, warn };

use crate::{
    common::ast::{
        ArrayDeclaration,
        Expression,
        FunctionDeclaration,
        StructDeclaration,
        VarDeclaration,
    },
    typing::ValueType,
};

use super::{ OptimizationWalker, Local };

use crate::error;

impl OptimizationWalker {
    #[instrument(level = "trace", skip(self))]
    pub fn var_declaration(&mut self, v: &mut VarDeclaration) {
        if let Some(ref mut e) = v.initializer {
            let m_e = self.visit_expression(*e.clone(), false);
            *e = Box::new(m_e);
        }
        
    }

    pub fn parse_variable(&mut self, name: String, mutable: bool) -> Option<String> {
        self.declare_variable(name.clone(), mutable);
        if self.function_compiler.scope_depth > 0 {
            let last_local = self.last_local();
            self.function_compiler.locals.get_mut(&last_local).unwrap().depth =
                self.function_compiler.scope_depth;
            return None;
        }

        return Some(name);
    }

    pub fn declare_variable(&mut self, name: String, mutable: bool) {
        if self.function_compiler.scope_depth == 0 {
            return;
        }

        for i in self.function_compiler.locals.keys() {
            let local = &self.function_compiler.locals.get(&i).unwrap();
            if local.depth != -1 && local.depth < self.function_compiler.scope_depth {
                break;
            }

            if name == local.name {
                error!(self, "Already variable with this name in this scope.");
            }
        }

        self.add_local(name, mutable);
    }

    fn add_local(&mut self, name: String, mutable: bool) {
        let local = Local {
            name: name.clone(),
            depth: -1,
            mutable: mutable,
            captured: false,
            local_type: ValueType::Undef.intern(),
        };
        self.function_compiler.locals.insert(self.function_compiler.local_count, local);
    }

    #[instrument(level = "trace", skip(self))]
    pub fn fun_declaration(&mut self, f: &mut FunctionDeclaration) {
        let name = f.name.clone();
        let _global_id = self.parse_variable(name.clone(), false);
        if self.function_compiler.scope_depth > 0 {
            // self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
            let last_local = self.last_local();
            self.function_compiler.locals.get_mut(&last_local).unwrap().depth =
                self.function_compiler.scope_depth;
        }

        let t = self.function(f.body.clone());

        let Expression::Function(ft) = t else {
            unreachable!();
        };

        f.body = ft;

    }

    #[instrument(level = "trace", skip(self))]
    pub fn struct_declaration(&mut self, sd: &mut StructDeclaration) {
        let s = sd.s.clone();
        let _name = s.name;
        let _fields = s.fields;
        let _offset = 0;
    }

    #[instrument(level = "trace", skip(self))]
    pub fn array_declaration(&mut self, a: &mut ArrayDeclaration) {

        self.define_array(a);
    }

    fn define_array(&mut self, a: &mut ArrayDeclaration) {
        let _line = 0;
        for v in &mut a.elements {
            let m_v = self.visit_expression(v.clone(), false);
            *v = m_v;
        }
    }
}
