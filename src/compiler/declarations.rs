use tracing::{instrument, span, Level};

use crate::{
    chunk::Opcode,
    common::ast::{
        ArrayDeclaration, Expression, FunctionDeclaration, StructDeclaration, VarDeclaration
    },
    value::{pointer::Pointer, Value},
    typing::ValueTypeK,
};

use super::{Compiler, Local};

use crate::error;

impl Compiler {

    #[instrument(level = "trace", skip(self))]
    pub fn var_declaration(&mut self, mut v: VarDeclaration) {
        let _guard = span!(Level::TRACE, "var_declaration");
        let line = v.line;

        let name = v.name.clone();
        let mut global_id = self.parse_variable(v.name, v.mutable);
        

        if let Some(var_type) = v.tipe {
            if global_id == -1 {
                let last_local = self.last_local();
                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = var_type;
            } else {
                self.gloabls.insert(name.clone(), global_id as usize);
                self.global_types
                    .insert(global_id as usize, (var_type, v.mutable, true));
            }
        }
        if let Some(expr) = v.initializer {
            let expr_type = self.visit_expression(*expr, false);
            global_id = self.static_data_segment.len() as i64;

            if let Some(var_type) = v.tipe {
                if expr_type != var_type {
                    error!(self, &format!(
                        "Type mismatch, expected {:?} got {:?}",
                        var_type, expr_type
                    ));
                }
            }

            v.tipe = Some(expr_type);

            if global_id == -1 {
                let last_local = self.last_local();
                self.function_compiler.locals.get_mut(&last_local).unwrap().assigned = true;
                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = expr_type;
            } else {
                self.gloabls.insert(name, global_id as usize);
                self.global_types
                    .insert(global_id as usize, (expr_type, v.mutable, true));
            }
        } else {
            for _ in 0..v.tipe.unwrap().num_words() {
                self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
            }
        }

        if global_id != -1 {
            for _ in 0..v.tipe.unwrap().num_words() {
                self.static_data_segment.push(Value::Nil.to_word());
            }
        }
        self.define_variable(global_id as u32, v.tipe.unwrap());
    }

    pub fn parse_variable(&mut self, name: String, mutable: bool) -> i64 {
        self.declare_variable(name.clone(), mutable);
        if self.function_compiler.scope_depth > 0 {
            let last_local = self.last_local();
            self.function_compiler.locals.get_mut(&last_local).unwrap().depth = self.function_compiler.scope_depth;
            return -1;
        }

        self.gloabls
            .insert(name.clone(), self.static_data_segment.len());
        self.global_types.insert(
            self.static_data_segment.len(),
            (ValueTypeK::Undef.intern(), mutable, false),
        );
        return *self.gloabls.get(&name).unwrap() as i64;
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
            assigned: false,
            captured: false,
            local_type: ValueTypeK::Undef.intern(),
        };
        self.function_compiler
            .locals
            .insert(self.function_compiler.local_count, local);
    }

    #[instrument(level = "trace", skip(self))]
    pub fn fun_declaration(&mut self, f: FunctionDeclaration) {
        let name = f.name;
        let global_id = self.parse_variable(name.clone(), false);
        if self.function_compiler.scope_depth > 0 {
            // self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
            let last_local = self.last_local();
            self.function_compiler.locals.get_mut(&last_local).unwrap().depth = self.function_compiler.scope_depth;
        }

        if global_id != -1 {
            for _ in 0..3 {
                self.static_data_segment.push(Value::Nil.to_word());
            }
        }

        let t = self.function(f.body);

        if global_id == -1 {
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = t;
        } else {
            self.gloabls.insert(name.clone(), global_id as usize);
            self.global_types
                .insert(global_id as usize, (t, false, true));
        }
        self.define_variable(global_id as u32, t);
    }

    #[instrument(level = "trace", skip(self))]
    pub fn struct_declaration(&mut self, sd: StructDeclaration) {
        let s = sd.s;
        let _name = s.name;
        let _fields = s.fields;
        let _offset = 0;
    }

    #[instrument(level = "trace", skip(self))]
    pub fn array_declaration(&mut self, a: ArrayDeclaration) {

        let global_id = self.parse_variable(a.name.clone(), false);
        let var_type = a.elem_tipe.expect("elem_tipe is none");
        let n = a.elements.len();

        self.mark_initialized();

        if global_id == -1 {
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = ValueTypeK::Array(var_type, n as usize).intern();
            self.function_compiler.locals.get_mut(&last_local).unwrap().mutable = false;
            self.function_compiler.locals.get_mut(&last_local).unwrap().assigned = true;

        } else {
            self.global_types.insert(
                global_id as usize,
                (
                    ValueTypeK::Array(var_type, n as usize).intern(),
                    false,
                    true,
                ),
            );
        }

        self.define_array(global_id, Some(n as u32), a);

    }

    fn define_array(&mut self, global_id: i64, size: Option<u32>, a: ArrayDeclaration) {
        
        let line = 0;

        if global_id == -1 {
            self.function_compiler
                .func
                .chunk
                .write(Opcode::DefineStackArray as u8, line);

            let last_local = self.last_local();
            let elem_type = self.function_compiler.locals.get(&last_local).unwrap().local_type;
            let mut pointee_type = match elem_type.decay(self.custom_structs.clone().into()) {
                ValueTypeK::Pointer(t, _) => t,
                _ => ValueTypeK::Undef.intern(),
            };

            if let ValueTypeK::Array(_, n) =
                self.function_compiler.locals.get(&last_local).unwrap().local_type
            {
                if *n != a.elements.len() {
                    error!(self, &format!(
                        "Args in initializer don't match, expected {} got {}",
                        n,
                        a.elements.len()
                    ));
                }
            

                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = ValueTypeK::Array(pointee_type, *n as usize).intern();
                self.function_compiler.local_count += 1;
            } else {
                let n = size.expect("size is none");
                for _ in 0..n {
                    for _ in 0..elem_type.num_words() {
                        self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
                        self.function_compiler.local_count += 1;
                    }
                }
            }

            if let Expression::Empty = a.elements[0] {
                for _ in 0..size.unwrap() {
                    for _ in 0..pointee_type.num_words() {
                        self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
                        self.function_compiler.local_count += 1;
                    }
                }
            }

            for v in &a.elements {
                let pt = self.visit_expression(v.clone(), false);
                if pointee_type == ValueTypeK::Undef.intern() {
                    pointee_type = pt;
                }
                self.function_compiler.local_count += pointee_type.num_words() as usize;

                if pt != pointee_type && pt != ValueTypeK::Nil.intern() {
                    error!(self, 
                        format!(
                            "b Element must have type {:?}, got {:?} instead",
                            pointee_type, pt
                        )
                        .as_str()
                    );
                }
            }
        }else {
            // global array
            let elem_type = self.global_types.get(&(global_id as usize)).unwrap().0;
            let mut pointee_type = match elem_type.decay(self.custom_structs.clone().into()) {
                ValueTypeK::Pointer(t, _) => t,
                _ => ValueTypeK::Undef.intern(),
            };

            if let Expression::Empty = a.elements[0] {
                for _ in 0..size.unwrap() {
                    for _ in 0..pointee_type.num_words() {
                        self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
                    }
                }
            }

            for v in a.elements.clone() {
                let pt = self.visit_expression(v, false);
                if pointee_type == ValueTypeK::Undef.intern() {
                    pointee_type = pt;
                }

                if pt != pointee_type && pt != ValueTypeK::Nil.intern() {
                    error!(self, 
                        format!(
                            "a Element must have type {:?}, got {:?} instead",
                            pointee_type, pt
                        )
                        .as_str()
                    );
                }
            }

            if let ValueTypeK::Array(_, n) =
                self.global_types.get(&(global_id as usize)).unwrap().0
            {
                if *n != a.elements.len() {
                    error!(self, &format!(
                        "Args in initializer don't match, expected {} got {}",
                        n,
                        a.elements.len()
                    ));
                }
            }

            self.static_data_segment.extend(Value::Nil.to_words());

            let words = Value::Pointer(Pointer::Static(self.static_data_segment.len())).to_words();

            for word in 0..words.len() {
                self.static_data_segment[global_id as usize + word] = words[word];
            }
            for _ in 0..size.unwrap() {
                for _ in 0..pointee_type.num_words() {
                    self.static_data_segment.extend(Value::Nil.to_words());
                }
            }

            self.function_compiler.func.chunk.write_pool_opcode(
                Opcode::DefineGlobalArray,
                global_id as u32,
                line,
            );
            self.function_compiler.func.chunk.write(size.unwrap() as u8, line);
            self.function_compiler
                .func
                .chunk
                .write(pointee_type.num_words() as u8, line);
        }
    }
}
