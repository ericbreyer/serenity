

use tracing::instrument;

use crate::{
    prelude::*,
    reg_compiler::iasm::{IASMParam, IASMRegister},
    value::{pointer::Pointer, Value},
};

use super::{
    EmitRegWalker, Local,
    iasm::{self, IASMCommandType},
};

use crate::error;
use crate::rvalue;

impl EmitRegWalker {
    #[instrument(level = "trace", skip(self))]
    pub fn var_declaration(&mut self, mut v: VarDeclaration, line: usize) {
        let name = v.name.clone();
        let global_id = self.parse_variable(v.name, v.mutable);
 
        if !v.mutable && v.initializer.is_none() {
            error!(
                self,
                &format!("[line {line}] Immutable variable must be initialized")
            );
            return;
        }

        if let Some(var_type) = v.tipe {
            if global_id == -1 {
                let last_local = self.last_local();
                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = var_type;
                // self.function_compiler.local_count += var_type.num_words() as usize;
            } else {
                self.gloabls.insert(name.clone(), global_id as usize);
                self.global_types
                    .insert(global_id as usize, (var_type, v.mutable));
            }
        }

        if let Some(expr) = v.initializer {
            let expr_res = self.visit_expression(*expr);
            let init_loc = rvalue!(self, expr_res.0, line);
            let stack_increased_by = expr_res.1;
            let expr_type = init_loc.get_type();

            if let Some(var_type) = v.tipe {
                if expr_type != var_type {
                    error!(
                        self,
                        &format!("Type mismatch, expected {var_type:?} got {expr_type:?}")
                    );
                }
            }

            v.tipe = Some(expr_type);

            if global_id == -1 {
                let last_local = self.last_local();
                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .assigned = true;
                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = expr_type;

                if !(expr_type.is_array() || expr_type.is_aggregate()) || stack_increased_by == 0{
                    self.make_room_on_stack_for(expr_type, line);
                    
                    let to_loc_reg = self.write_constant(
                        Value::Pointer(Pointer::Local(last_local)),
                        ValueType::Pointer(expr_type, true).intern(),
                        line,
                    );
                    self.write_store(to_loc_reg, init_loc, expr_type, line);
                    
                }
                if expr_type.is_aggregate() {
                    self.function_compiler.local_count += expr_type.num_words();
                }
            } else {
                self.gloabls.insert(name.clone(), global_id as usize);
                self.global_types
                    .insert(global_id as usize, (expr_type, v.mutable));

                let to_loc_reg = self.write_constant(
                    Value::Pointer(Pointer::Static(global_id as usize)),
                    ValueType::Pointer(expr_type, true).intern(),
                    line,
                );
                self.write_store(to_loc_reg, init_loc, expr_type, line);

                if expr_type.is_aggregate() {
                    for _ in 0..expr_type.num_words() {
                        self.write_statement(IASMCommandType::Pop, vec![], line);
                        self.function_compiler.push_count -= 1;
                    }
                }
            }
        } else {
            for _ in 0..v.tipe.expect("must have type").num_words() {
                let nil_reg = IASMRegister::nil_reg();
                if global_id == -1 {
                    self.write_statement(
                        iasm::IASMCommandType::Push,
                        vec![iasm::IASMParam::Register(nil_reg)],
                        line,
                    );
                    self.function_compiler.push_count += 1;
                }
            }
        }

        self.define_variable(global_id as u32, v.tipe.unwrap());

    }

    pub fn parse_variable(&mut self, name: SharedString, mutable: bool) -> i64 {
        self.declare_variable(name.clone(), mutable);
        if self.function_compiler.scope_depth > 0 {
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .depth = self.function_compiler.scope_depth;
            return -1;
        }

        let id = self
            .global_types
            .iter()
            .map(|(_, t)| t.0.num_words())
            .sum::<usize>();
        self.gloabls.insert(name.clone(), id);
        self.global_types
            .insert(id, (ValueType::Undef.intern(), mutable));
        return *self.gloabls.get(&name).unwrap() as i64;
    }

    pub fn declare_variable(&mut self, name: SharedString, mutable: bool) {
        if self.function_compiler.scope_depth == 0 {
            return;
        }

        for i in self.function_compiler.locals.keys() {
            let local = &self.function_compiler.locals.get(i).unwrap();
            if local.depth != -1 && local.depth < self.function_compiler.scope_depth {
                break;
            }

            if name == local.name {
                error!(
                    self,
                    &format!("Variable {name} already declared in this scope")
                );
            }
        }

        self.add_local(name, mutable);
    }

    fn add_local(&mut self, name: SharedString, mutable: bool) {
        let local = Local {
            name: name.clone(),
            depth: -1,
            mutable,
            assigned: false,
            captured: false,
            local_type: ValueType::Undef.intern(),
        };
        self.function_compiler
            .locals
            .insert(self.function_compiler.local_count, local);
    }

    #[instrument(level = "trace", skip(self))]
    pub fn fun_declaration(&mut self, f: FunctionDeclaration, line: usize) {
        let name = f.name;

        let global_id = self.parse_variable(name.clone(), false);
        if self.function_compiler.scope_depth > 0 {
            // self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .depth = self.function_compiler.scope_depth;
        }

        let f_loc = self.function(f.body, line).0;
        let t = f_loc.get_type();

        if global_id == -1 {
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = t;
            // self.write_statement(
            //     iasm::IASMCommandType::Push,
            //     vec![iasm::IASMParam::Register(f_loc)],
            //     line,
            // );
            self.function_compiler.local_count += t.num_words();
        } else {
            self.gloabls.insert(name.clone(), global_id as usize);
            self.global_types.insert(global_id as usize, (t, false));
            let loc_reg = self.write_constant(
                Value::Pointer(Pointer::Static(global_id as usize)),
                ValueType::Pointer(t, true).intern(),
                line,
            );

            self.write_store(loc_reg, f_loc, t, line);

            for _ in 0..t.num_words() {
                self.write_statement(IASMCommandType::Pop, vec![], line);
                self.function_compiler.push_count -= 1;
                // self.function_compiler.local_count -= 1;
            }
        }
        self.define_variable(global_id as u32, t);
    }

    #[instrument(level = "trace", skip(self))]
    pub fn array_declaration(&mut self, a: ArrayDeclaration, line: usize) {
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
                .local_type = ValueType::Array(var_type, n).intern();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .mutable = false;
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .assigned = true;
        } else {
            self.gloabls.insert(a.name.clone(), global_id as usize);
            self.global_types.insert(
                global_id as usize,
                (ValueType::Array(var_type, n).intern(), false),
            );
        }

        self.define_array(global_id, Some(n as u32), a, line)
    }

    fn define_array(
        &mut self,
        global_id: i64,
        size: Option<u32>,
        a: ArrayDeclaration,
        line: usize,
    ) {
        if global_id == -1 {
            let last_local = self.last_local();
            let elem_type = self
                .function_compiler
                .locals
                .get(&last_local)
                .unwrap()
                .local_type;
            let mut pointee_type =
                match elem_type.decay(self.custom_structs.clone().into()).as_ref() {
                    ValueType::Pointer(t, _) => *t,
                    _ => ValueType::Undef.intern(),
                };

            if let ValueType::Array(_, n) = self
                .function_compiler
                .locals
                .get(&last_local)
                .unwrap()
                .local_type
                .as_ref()
            {
                if *n != a.elements.len() {
                    error!(
                        self,
                        &format!(
                            "Args in initializer don't match, expected {} got {}",
                            n,
                            a.elements.len()
                        )
                    );
                }

                self.function_compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = ValueType::Array(pointee_type, *n).intern();
            } else {
                let n = size.expect("size is none");
                for _ in 0..n {
                        self.function_compiler.local_count += elem_type.num_words();
                }
            }

            if let Expression::Empty = a.elements[0] {
                for _ in 0..size.unwrap() {
                    self.function_compiler.local_count += pointee_type.num_words();
                }
            }
            for v in &a.elements {
                let reg = rvalue!(self, self.visit_expression(v.clone()).0, line);
                let pt = reg.get_type();
                if pointee_type == ValueType::Undef.intern() {
                    pointee_type = pt;
                }
                if !pointee_type.is_aggregate() {
                 
                
                self.write_statement(IASMCommandType::Push, vec![IASMParam::Register(reg)], line);
                self.function_compiler.push_count += 1;
                }
                self.function_compiler.local_count += pointee_type.num_words();

                if pt != pointee_type && pt != ValueType::Nil.intern() {
                    error!(
                        self,
                        format!("b Element must have type {pointee_type:?}, got {pt:?} instead")
                            .as_str()
                    );
                } else if pt == ValueType::Nil.intern() {
                    continue;
                }
            }
        } else {
            // global array
            let elem_type = self.global_types.get(&(global_id as usize)).unwrap().0;
            let mut pointee_type =
                match elem_type.decay(self.custom_structs.clone().into()).as_ref() {
                    ValueType::Pointer(t, _) => *t,
                    _ => ValueType::Undef.intern(),
                };

            for (i, v) in a.elements.clone().iter().enumerate() {
                let reg = rvalue!(self, self.visit_expression(v.clone()).0, line);
                let pt = reg.get_type();
                if pointee_type == ValueType::Undef.intern() {
                    pointee_type = pt;
                }

                if pt != pointee_type && pt != ValueType::Nil.intern() {
                    error!(
                        self,
                        format!("a Element must have type {pointee_type:?}, got {pt:?} instead")
                            .as_str()
                    );
                } else if pt == ValueType::Nil.intern() {
                    continue;
                }

                let to_reg = self.write_constant(
                    Value::Pointer(Pointer::Static((global_id as usize) + i)),
                    ValueType::Pointer(pointee_type, true).intern(),
                    line,
                );
                self.write_store(to_reg, reg, pointee_type, line);

                if pointee_type.is_aggregate() {
                    for _ in 0..pointee_type.num_words() {
                        self.write_statement(IASMCommandType::Pop, vec![], line);
                        self.function_compiler.push_count -= 1;
                    }
                }
            }

            if let ValueType::Array(_, n) = self
                .global_types
                .get(&(global_id as usize))
                .unwrap()
                .0
                .as_ref()
            {
                if *n != a.elements.len() {
                    error!(
                        self,
                        &format!(
                            "Args in initializer don't match, expected {} got {}",
                            n,
                            a.elements.len()
                        )
                    );
                }
            }
        }
    }
}
