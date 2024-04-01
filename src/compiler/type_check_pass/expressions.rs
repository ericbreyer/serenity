use std::{ cell::Cell, collections::HashMap };

use tracing::{debug, instrument};

use crate::{
    common::ast::{ Expression, FunctionExpression },
    lexer::{ Token, TokenType },
    value::Value,
    typing::{ CustomStruct, ValueType, ValueTypeK },
};

use super::{ FunctionCompiler, Local, TypeCheckWalker, Upvalue };

use crate::error;

impl TypeCheckWalker {
    #[instrument(level = "trace", skip_all)]
    pub fn literal(&mut self, v: Value) -> ValueType {
        let _line = 0;
        match v {
            Value::Bool(false) => { ValueTypeK::Bool.intern() }
            Value::Nil => { ValueTypeK::Nil.intern() }
            Value::Bool(true) => { ValueTypeK::Bool.intern() }
            Value::Integer(_i) => { ValueTypeK::Integer.intern() }
            Value::Float(_f) => { ValueTypeK::Float.intern() }
            Value::Char(_c) => { ValueTypeK::Char.intern() }
            _ => {
                error!(self, "Cannot compile this type of literal");
                ValueTypeK::Err.intern()
            }
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn string(&mut self, _s: String) -> ValueType {
        return ValueTypeK::String.intern();
    }

    #[instrument(level = "trace", skip_all)]
    pub fn unary(&mut self, token_t: TokenType, e: Expression) -> ValueType {
        let _line = 0;
        let operator_type = token_t;
        // Compile the operand.
        let t = self.visit_expression(e, false);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus =>
                match t {
                    ValueTypeK::Integer | ValueTypeK::Float => {}
                    _ => {
                        error!(self, "Operand must be a number.");
                    }
                }
            TokenType::Bang =>
                match t {
                    ValueTypeK::Bool => {}
                    _ => {
                        error!(self, "Operand must be a boolean.");
                    }
                }
            _ => unreachable!(),
        }
        t
    }

    #[instrument(level = "trace", skip_all)]
    pub fn deref(&mut self, e: Expression, at: bool) -> ValueType {
        let _line = 0;
        // Compile the operand.
        let raw_t = self.visit_expression(e, false);
        let (t, _is_const) = match raw_t.decay(self.custom_structs.clone().into()) {
            &ValueTypeK::Pointer(pointee_type, is_const) => (pointee_type, is_const),
            _ => {
                error!(self, "Expected pointer type");
                (ValueTypeK::Err.intern(), false)
            }
        };

        if !at {
            t
        } else {
            raw_t
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn addr_of(&mut self, e: Expression) -> ValueType {
        // Compile the operand.
        let t = self.visit_expression(e, true);
        match t {
            ValueTypeK::Pointer(_, _) => t,
            _ => {
                error!(self, "Expected pointer type");
                ValueTypeK::Err.intern()
            }
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn index(&mut self, e: Expression, i: Expression, at: bool) -> ValueType {
        let _line = 0;

        // Compile the operand.
        let raw_t = self.visit_expression(e, false);
        let pointer_type = raw_t.decay(self.custom_structs.clone().into());
        let &ValueTypeK::Pointer(pointee_type, _) = pointer_type else {
            error!(self, "Expected pointer type");
            return ValueTypeK::Err.intern();
        };

        let i_type = self.visit_expression(i, false);
        if i_type != ValueTypeK::Integer.intern() {
            error!(self, "Index must be an integer");
        }

        if !at {
            pointee_type
        } else {
            raw_t
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn binary(&mut self, l: Expression, operator: TokenType, r: Expression) -> ValueType {
        let operator_type = operator;
        let _line = 0;

        // Compile the left operand.
        let lt = self.visit_expression(l, false).decay(Some(self.custom_structs.clone()));

        // Compile the right operand.
        let rt = self.visit_expression(r, false);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Plus =>
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        return ValueTypeK::Integer.intern();
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        return ValueTypeK::Float.intern();
                    }
                    (ValueTypeK::Pointer(_, _), ValueTypeK::Integer) => {
                        return lt;
                    }
                    _ => {
                        error!(self, format!("Operands must be two numbers or a pointer and an integer got {:?} and {:?}", lt, rt).as_str());
                        return ValueTypeK::Err.intern();
                    }
                }
            TokenType::Minus =>
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        return ValueTypeK::Integer.intern();
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        return ValueTypeK::Float.intern();
                    }
                    (ValueTypeK::Pointer(_, _), ValueTypeK::Integer) => {
                        return lt;
                    }
                    _ => {
                        error!(self, "Operands must be two numbers or a pointer and an integer.");
                        return ValueTypeK::Err.intern();
                    }
                }
            TokenType::Star =>
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        return ValueTypeK::Integer.intern();
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        return ValueTypeK::Float.intern();
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                }
            TokenType::Slash =>
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        return ValueTypeK::Integer.intern();
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        return ValueTypeK::Float.intern();
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                }
            TokenType::EqualEqual => {
                if rt != lt {
                    self.warn(
                        &format!(
                            "Comparison of different types ({:?} and {:?}) is always false, please cast to the same type",
                            lt,
                            rt
                        )
                    );

                    return ValueTypeK::Bool.intern();
                }

                return ValueTypeK::Bool.intern();
            }
            TokenType::BangEqual => {
                if rt != lt {
                    self.warn(
                        &format!(
                            "Comparison of different types ({:?} and {:?}) is always true, please cast to the same type",
                            lt,
                            rt
                        )
                    );

                    return ValueTypeK::Bool.intern();
                }

                return ValueTypeK::Bool.intern();
            }
            TokenType::Greater => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) |
                    (ValueTypeK::Float, ValueTypeK::Float) => {}
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            TokenType::GreaterEqual => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {}
                    (ValueTypeK::Float, ValueTypeK::Float) => {}
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            TokenType::Less => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {}
                    (ValueTypeK::Float, ValueTypeK::Float) => {}
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            TokenType::LessEqual => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {}
                    (ValueTypeK::Float, ValueTypeK::Float) => {}
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            _ => unreachable!(),
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn ternary(&mut self, c: Expression, t: Expression, f: Expression) -> ValueType {
        let _line = 0;
        let c_type = self.visit_expression(c, false);
        if c_type != ValueTypeK::Bool.intern() {
            error!(self, "Condition must be a boolean.");
        }
        
        // compile the then branch
        let t_type = self.visit_expression(t, false);

        // compile the else branch
        let f_type = self.visit_expression(f, false);

        if t_type != f_type {
            error!(self, "Both branches of the ternary operator must have the same type.");
        }
        t_type
    }

    #[instrument(level = "trace", skip_all)]
    pub fn variable(&mut self, tok: Token, assignment_target: bool) -> ValueType {
        let _line = 0;

        debug!(?tok, ?assignment_target, "variable");

        let (arg, assignable) = self.resolve_local(0, &tok);
        if let Some(i) = arg {
            if assignment_target {
                // if !assignable {
                //     error!(self, "Can't assign to const variable");
                // }

                return ValueTypeK::Pointer(
                    self.function_compiler.locals[&i].local_type,
                    assignable
                ).intern();
            } else {

                let t = self.function_compiler.locals.get(&i).expect("local").local_type;

                return t;
            }
        } else if let Some(i) = self.resolve_upvalue(0, &tok) {
            if assignment_target {
                // if !assignable {
                //     error!(self, "Can't assign to const variable");
                // }

                return ValueTypeK::Pointer(
                    self.function_compiler.locals[&i].local_type,
                    assignable
                ).intern();
            } else {

                let t = self.function_compiler.locals.get(&i).expect("local").local_type;

                return t;
            }
        } else if let Some(i) = self.gloabls.get(&tok.lexeme) {
            if assignment_target {
                return ValueTypeK::Pointer(
                    self.global_types.get(i).unwrap().0,
                    self.global_types.get(i).unwrap().1
                ).intern();
            } else {
                let t = self.global_types.get(i).unwrap().0;

                return t;
            }
        } else {
            error!(self, format!("Undefined variable '{}'.", tok.lexeme).as_str());
            ValueTypeK::Err.intern()
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn assign(&mut self, l: Expression, r: Expression) -> ValueType {
        let _line = 0;
        let t = self.visit_expression(l, true);

        let t2 = self.visit_expression(r, false);

        let ValueTypeK::Pointer(s, assignable) = t.decay(self.custom_structs.clone().into()) else {
            error!(self, format!("Can only assign to a pointer type, got {:?}", t).as_str());
            return ValueTypeK::Err.intern();
        };

        if !assignable {
            error!(self, "Can't assign to const variable");
        }

        if t2 != *s {
            error!(self, "Type mismatch");
        }

        t2
    }

    #[instrument(level = "trace", skip_all)]
    pub fn logical(&mut self, l: Expression, operator: TokenType, r: Expression) -> ValueType {
        let _line = 0;
        let operator_type = operator;
        let t = self.visit_expression(l, false);
        if t != ValueTypeK::Bool.intern() {
            error!(self, "Operand must be a boolean.");
        }
        if operator_type == TokenType::Or {
            let t2 = self.visit_expression(r, false);
            if t2 != ValueTypeK::Bool.intern() {
                error!(self, "Operand must be a boolean.");
            }
            return ValueTypeK::Bool.intern();
        } else {
            let t2 = self.visit_expression(r, false);
            if t2 != ValueTypeK::Bool.intern() {
                error!(self, "Operand must be a boolean.");
            }
            return ValueTypeK::Bool.intern();
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn call(&mut self, callee: Expression, args: Vec<Expression>) -> ValueType {
        let _line = 0;
        let t = self.visit_expression(callee, false);

        let mut ts = Vec::new();
        for arg in args.clone() {
            ts.push(self.visit_expression(arg, false));
        }

        if let ValueTypeK::Closure(f) = t {

            if ts.iter().map(|x| x.decay(None)).collect::<Vec<ValueType>>() != f[0..f.len() - 1] {
                error!(self, format!("Argument types do not match function signature. Expected {:?}, got {:?}", f[0..f.len() - 1].to_vec(), ts).as_str());
            }

            let return_type = f.last().unwrap();
            return return_type;
        } else {
            error!(self, format!("Can only call function types, got {:?}", t).as_str());
            return ValueTypeK::Err.intern();
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn dot(&mut self, e: Expression, tok: Token, at: bool) -> ValueType {
        let _line = 0;
        let t = self.visit_expression(e, true);

        let ValueTypeK::Pointer(ValueTypeK::Struct(s), _) = t.decay(
            self.custom_structs.clone().into()
        ) else {
            error!(self, format!("Can only access fields of struct types, got {:?}", t).as_str());
            return ValueTypeK::Err.intern();
        };

        let temp = s.fields.borrow();
        let Some(field) = temp.get(&tok.lexeme) else {
            error!(
                self,
                format!("Field '{}' not found have fields {:?}", tok.lexeme, s.fields).as_str()
            );
            return ValueTypeK::Err.intern();
        };

        if !at {
            return field.value;
        } else {
            return ValueTypeK::Pointer(field.value, true).intern();
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn function(&mut self, function_expr: FunctionExpression) -> ValueType {
        let c = FunctionCompiler::new(
            Some(std::mem::take(&mut self.function_compiler)),
            &function_expr.name.clone()
        );
        self.function_compiler = Box::new(c);
        self.function_compiler.scope_depth += 1;

        for (_, param) in function_expr.params.iter().enumerate() {
            let _mutable = true;

            let p_type = param.1;

            self.parse_variable(param.0.clone(), _mutable);
            let last_local = self.last_local();
            self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = p_type;

            self.define_variable(0, p_type);
        }

        let return_type = function_expr.return_type;
        self.function_compiler.return_type = return_type;

        let mut param_types = function_expr.params
            .iter()
            .map(|(_, t)| *t)
            .collect::<Vec<_>>();
        param_types.push(return_type);

        let func_name = function_expr.name.clone();

        let temp = param_types.clone();
        let ft = temp.as_slice();
        self.function_compiler.locals.insert(0, Local {
            name: func_name,
            depth: 0,
            mutable: false,
            assigned: false,
            captured: false,
            local_type: &ValueTypeK::Closure(ft.into()).intern(),
        });
        // self.compiler.local_count += ft.num_words() as usize;
        let mut does_return = false;
        for d in &function_expr.body {
            let (_, r) = self.visit(d.clone());
            does_return = does_return || r;
        }

        if !does_return && self.function_compiler.return_type != ValueTypeK::Nil.intern() {
            error!(self, format!("Function \"{}\" must return a value in all code paths", function_expr.name).as_str());
        }

        *param_types.last_mut().unwrap() = self.function_compiler.return_type;
        let c = std::mem::take(&mut self.function_compiler);

        let (maybe_enclosing, _upvalues) = c.recover_values();
        let Some(enclosing) = maybe_enclosing else {
            error!(self, "Cannot return from top-level code.");
            return ValueTypeK::Err.intern();
        };

        self.function_compiler = enclosing;


        // if any upvalues are non-const we can not escape

        return ValueTypeK::Closure(ft.into()).intern();
    }

    #[instrument(level = "trace", skip_all)]
    pub fn cast(&mut self, value: Expression, cast_type: ValueType, from_type: &Cell<ValueType>) -> ValueType {
        // cast(value, type)
        let _line = 0;

        let t = self.visit_expression(value, false);
        from_type.set(t);

        match (t.decay(self.custom_structs.clone().into()), cast_type) {
            (x, y) if x == y => x,
            (ValueTypeK::Integer, ValueTypeK::Float) => {
                ValueTypeK::Float.intern()
            }
            (ValueTypeK::Float, ValueTypeK::Integer) => {
                ValueTypeK::Integer.intern()
            }
            (ValueTypeK::Integer, ValueTypeK::Bool) => {
                ValueTypeK::Bool.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::Integer) => {
                ValueTypeK::Integer.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::Float) => {
                ValueTypeK::Float.intern()
            }
            (ValueTypeK::Integer, ValueTypeK::String) => {
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Float, ValueTypeK::String) => {
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::String) => {
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Array(x, _), ValueTypeK::Pointer(y, _)) if x == y => {
                ValueTypeK::Pointer(x, false).intern()
            }
            (ValueTypeK::Pointer(_, _), ValueTypeK::Pointer(_, _)) => cast_type,
            _ => {
                error!(self, &format!("Cannot cast {:?} to {:?}", t, cast_type));
                ValueTypeK::Err.intern()
            }
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn array_literal(&mut self, elements: Vec<Expression>) -> ValueType {
        let _line = 0;
        let mut element_types = Vec::new();
        for e in elements {
            let t = self.visit_expression(e, false);
            element_types.push(t);
        }
        let array_type = element_types[0];
        for t in &element_types {
            if *t != array_type {
                error!(self, "All elements of an array must have the same type.");
            }
        }
        let array_type = ValueTypeK::Array(array_type, element_types.len()).intern();
        array_type
    }

    #[instrument(level = "trace", skip_all)]
    pub fn struct_literal(
        &mut self,
        t: CustomStruct,
        fields: HashMap<String, Expression>
    ) -> ValueType {
        let temp = t.fields.borrow();
        let mut orig_fields = temp.iter().collect::<Vec<_>>();
        orig_fields.sort_by(|a, b| a.1.offset.cmp(&b.1.offset));
        for (name, entry) in orig_fields.iter() {
            let Some(exp) = fields.get(*name) else {
                for _ in 0..entry.value.num_words() {
                }
                continue;
            };
            let ft = self.visit_expression(exp.clone(), false);
            if ft != entry.value {
                error!(
                    self,
                    format!("Field {} has type {:?}, expected {:?}", name, ft, entry.value).as_str()
                );
                return ValueTypeK::Err.intern();
            }
        }
        drop(temp);
        ValueTypeK::Struct(t).intern()
    }

    fn resolve_upvalue(&mut self, compiler_level: u32, token: &Token) -> Option<usize> {
        if let None = Self::compiler_at(compiler_level + 1, &mut self.function_compiler) {
            return None;
        }

        if let (Some(local), _) = self.resolve_local(compiler_level + 1, token) {
            let Some(compiler) = Self::compiler_at(
                compiler_level + 1,
                &mut self.function_compiler
            ) else {
                return None;
            };

            compiler.locals.get_mut(&local).expect("Local not found").captured = true;
            let t = compiler.locals.get(&local).expect("Local not found").local_type;
            let n = compiler.locals.get(&local).expect("Local not found").name.clone();
            let Some((upval_place, local_place)) =  Some(self.add_upvalue(local, compiler_level, t, n)) else {
                return None;
            };
            return Some(local_place);
        }
        if let Some(upvalue) = self.resolve_upvalue(compiler_level + 1, token) {
            let Some(compiler) = Self::compiler_at(
                compiler_level + 1,
                &mut self.function_compiler
            ) else {
                return None;
            };
            let t = compiler.locals.get(&upvalue).expect("Local not found").local_type;
            let n = compiler.locals.get(&upvalue).expect("Local not found").name.clone();
            return Some(self.add_upvalue(upvalue, compiler_level, t, n).1);
        }

        None
    }

    fn compiler_at(level: u32, compiler: &mut FunctionCompiler) -> Option<&mut FunctionCompiler> {
        if level == 0 {
            return Some(compiler);
        } else {
            return compiler.enclosing.as_mut().and_then(|e| Self::compiler_at(level - 1, e));
        }
    }

    fn add_upvalue(
        &mut self,
        up_idx: usize,
        compiler_level: u32,
        upvalue_type: ValueType,
        name: String
    ) -> (usize, usize) {
        let compiler = Self::compiler_at(compiler_level, &mut self.function_compiler).unwrap();
        let upvalue_count = compiler.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &compiler.upvalues[i];
            if upvalue.name == name {
                return (i, upvalue.local_idx);
            }
        }

        if upvalue_count == (u8::MAX as usize) {
            error!(self, "Trying to capture too many variables in closure");
            return (0,0);
        }
        
        if compiler.locals.contains_key(&compiler.local_count) {
            compiler.locals.insert(compiler.local_count + upvalue_type.num_words() as usize, compiler.locals.get(&compiler.local_count).unwrap().clone());
        }
        
        compiler.locals.insert(compiler.local_count, Local {
            name: name.to_owned(),
            depth: compiler.scope_depth,
            mutable: true,
            assigned: true,
            captured: true,
            local_type: upvalue_type,
        });

        compiler.upvalues.push(Upvalue {
            local_idx: up_idx,
            upvalue_idx: compiler.upvalue_count,
            upvalue_type: upvalue_type,
            name: name.to_owned(),
        });
        compiler.local_count += upvalue_type.num_words() as usize;

        compiler.upvalue_count += upvalue_type.num_words() as usize;

        return (compiler.upvalue_count - upvalue_type.num_words(), compiler.local_count - upvalue_type.num_words() as usize);
    }

    fn resolve_local(&mut self, compiler_level: u32, token: &Token) -> (Option<usize>, bool) {
        let Some(compiler) = Self::compiler_at(compiler_level, &mut self.function_compiler) else {
            return (None, false);
        };
        if compiler.locals.len() == 0 {
            return (None, true);
        }
        for (k, local) in compiler.locals.iter() {
            if token.lexeme == local.name {
                if local.depth == -1 {
                    error!(self, "Cannot read local variable in its own initializer.");
                    return (None, true);
                }
                return (Some(*k), local.mutable || !local.assigned);
            }
        }
        return (None, true);
    }
}
