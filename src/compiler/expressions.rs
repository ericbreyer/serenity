use std::{collections::HashMap, rc::Rc};

use tracing::{instrument, Level};

use crate::{
    chunk::Opcode,
    common::{ast::{Expression, FunctionExpression}, runnable::Runnable},
    lexer::{Token, TokenType},
    value::{
        pointer::Pointer,
        
        Value,
    },
    typing::{CustomStruct, ValueType, ValueTypeK},
};

use super::{Compiler, Local, FunctionCompiler};

use crate::error;

impl Compiler {
    #[instrument(level = "trace", skip_all)]
    pub fn literal(&mut self, v: Value) -> ValueType {
        let line = 0;
        match v {
            Value::Bool(false) => {
                self.function_compiler.func.chunk.write(Opcode::False.into(), line);
                ValueTypeK::Bool.intern()
            }
            Value::Nil => {
                self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
                ValueTypeK::Nil.intern()
            }
            Value::Bool(true) => {
                self.function_compiler.func.chunk.write(Opcode::True.into(), line);
                ValueTypeK::Bool.intern()
            }
            Value::Integer(i) => {
                self.function_compiler
                    .func
                    .chunk
                    .write_constant(Value::Integer(i), line);
                ValueTypeK::Integer.intern()
            }
            Value::Float(f) => {
                self.function_compiler
                    .func
                    .chunk
                    .write_constant(Value::Float(f), line);
                ValueTypeK::Float.intern()
            }
            Value::Char(c) => {
                self.function_compiler
                    .func
                    .chunk
                    .write_constant(Value::Char(c), line);
                ValueTypeK::Char.intern()
            }
            _ => {
                error!(self, "Cannot compile this type of literal");
                ValueTypeK::Err.intern()
            }
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn string(&mut self, s: String) -> ValueType {
        let line = 0;
        let mut value = s;
        value = value.trim_matches('"').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\a", "\x07");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        let start = self.static_data_segment.len();
        self.static_data_segment
            .extend(Value::Integer(value.len() as i64).to_words());
        for c in value.chars() {
            self.static_data_segment
                .extend(Value::Char(c as u8).to_words());
        }
        self.function_compiler
            .func
            .chunk
            .write_constant(Value::Pointer(Pointer::Static(start)), line);
        return ValueTypeK::String.intern();
    }

    #[instrument(level = "trace", skip_all)]
    pub fn unary(&mut self, token_t: TokenType, e: Expression) -> ValueType {
        let line = 0;
        let operator_type = token_t;
        // Compile the operand.
        let t = self.visit_expression(e, false);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus => match t {
                ValueTypeK::Integer => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::NegateInt.into(), line);
                }
                ValueTypeK::Float => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::NegateFloat.into(), line);
                }
                _ => {
                    error!(self, "Operand must be a number.");
                }
            },
            TokenType::Bang => self.function_compiler.func.chunk.write(Opcode::Not.into(), line),
            _ => unreachable!(),
        }
        t
    }

    #[instrument(level = "trace", skip_all)]
    pub fn deref(&mut self, e: Expression, at: bool) -> ValueType {
        let line = 0;
        // Compile the operand.
        let raw_t = self.visit_expression(e, false);
        let (t, _is_const) = match raw_t.decay(self.custom_structs.clone().into()) {
            &ValueTypeK::Pointer(pointee_type, is_const) => (pointee_type, is_const),
            _ => {
                error!(self, "Expected pointer type");
                (ValueTypeK::Err.intern(), false)
            }
        };

        if raw_t == ValueTypeK::String.intern() {
            self.function_compiler
                .func
                .chunk
                .write_constant(Value::Integer(1), line);
            self.function_compiler
                .func
                .chunk
                .write(Opcode::PointerAdd as u8, line);
        }

        if !at {
            self.function_compiler
                .func
                .chunk
                .write(Opcode::DerefGet.into(), line);
            self.function_compiler.func.chunk.write(t.num_words() as u8, line);
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
        let line = 0;

        // Compile the operand.
        let raw_t = self.visit_expression(e, false);
        let pointer_type = raw_t.decay(self.custom_structs.clone().into());
        let &ValueTypeK::Pointer(pointee_type, _) = pointer_type else {
            error!(self, "Expected pointer type");
            return ValueTypeK::Err.intern();
        };

        if raw_t == ValueTypeK::String.intern() {
            self.function_compiler
                .func
                .chunk
                .write_constant(Value::Integer(1), line);
            self.function_compiler
                .func
                .chunk
                .write(Opcode::PointerAdd as u8, line);
        }

        let i_type = self.visit_expression(i, false);
        if i_type != ValueTypeK::Integer.intern() {
            error!(self, "Index must be an integer");
        }

        self.function_compiler
            .func
            .chunk
            .write_constant(Value::Integer(pointee_type.num_words() as i64), line);
        self.function_compiler
            .func
            .chunk
            .write(Opcode::MultiplyInt as u8, line);
        self.function_compiler
            .func
            .chunk
            .write(Opcode::PointerAdd as u8, line);

        if !at {
            self.function_compiler
                .func
                .chunk
                .write(Opcode::DerefGet.into(), line);
            self.function_compiler
                .func
                .chunk
                .write(pointee_type.num_words() as u8, line);
            pointee_type
        } else {
            raw_t
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn binary(&mut self, l: Expression, operator: TokenType, r: Expression) -> ValueType {
        let operator_type = operator;
        let line = 0;

        // Compile the left operand.
        let lt = self.visit_expression(l, false);

        // Compile the right operand.
        let rt = self.visit_expression(r, false);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Plus => match (lt, rt) {
                (ValueTypeK::Integer, ValueTypeK::Integer) => {
                    self.function_compiler.func.chunk.write(Opcode::AddInt.into(), line);
                    return ValueTypeK::Integer.intern();
                }
                (ValueTypeK::Float, ValueTypeK::Float) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::AddFloat.into(), line);
                    return ValueTypeK::Float.intern();
                }
                (ValueTypeK::Pointer(_, _), ValueTypeK::Integer) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::PointerAdd.into(), line);
                    return lt;
                }
                _ => {
                    error!(
                        self,
                        "Operands must be two numbers or a pointer and an integer."
                    );
                    return ValueTypeK::Err.intern();
                }
            },
            TokenType::Minus => match (lt, rt) {
                (ValueTypeK::Integer, ValueTypeK::Integer) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::SubtractInt.into(), line);
                    return ValueTypeK::Integer.intern();
                }
                (ValueTypeK::Float, ValueTypeK::Float) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::SubtractFloat.into(), line);
                    return ValueTypeK::Float.intern();
                }
                (ValueTypeK::Pointer(_, _), ValueTypeK::Integer) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::PointerSubtract.into(), line);
                    return lt;
                }
                _ => {
                    error!(
                        self,
                        "Operands must be two numbers or a pointer and an integer."
                    );
                    return ValueTypeK::Err.intern();
                }
            },
            TokenType::Star => match (lt, rt) {
                (ValueTypeK::Integer, ValueTypeK::Integer) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::MultiplyInt.into(), line);
                    return ValueTypeK::Integer.intern();
                }
                (ValueTypeK::Float, ValueTypeK::Float) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::MultiplyFloat.into(), line);
                    return ValueTypeK::Float.intern();
                }
                _ => {
                    error!(self, "Operands must be two numbers.");
                    return ValueTypeK::Err.intern();
                }
            },
            TokenType::Slash => match (lt, rt) {
                (ValueTypeK::Integer, ValueTypeK::Integer) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::DivideInt.into(), line);
                    return ValueTypeK::Integer.intern();
                }
                (ValueTypeK::Float, ValueTypeK::Float) => {
                    self.function_compiler
                        .func
                        .chunk
                        .write(Opcode::DivideFloat.into(), line);
                    return ValueTypeK::Float.intern();
                }
                _ => {
                    error!(self, "Operands must be two numbers.");
                    return ValueTypeK::Err.intern();
                }
            },
            TokenType::EqualEqual => {
                if rt != lt {
                    self.warn(&format!(
                        "Comparison of different types ({:?} and {:?}) is always false, please cast to the same type",
                        lt, rt
                    ));
                    self.function_compiler.func.chunk.write_pop(rt, line);
                    self.function_compiler.func.chunk.write_pop(lt, line);
                    self.function_compiler.func.chunk.write(Opcode::False.into(), line);
                    return ValueTypeK::Bool.intern();
                }
                self.function_compiler.func.chunk.write(Opcode::Equal.into(), line);
                return ValueTypeK::Bool.intern();
            }
            TokenType::BangEqual => {
                if rt != lt {
                    self.warn(&format!(
                        "Comparison of different types ({:?} and {:?}) is always true, please cast to the same type",
                        lt, rt
                    ));
                    self.function_compiler.func.chunk.write_pop(rt, line);
                    self.function_compiler.func.chunk.write_pop(lt, line);
                    self.function_compiler.func.chunk.write(Opcode::True.into(), line);
                    return ValueTypeK::Bool.intern();
                }
                self.function_compiler.func.chunk.write(Opcode::Equal.into(), line);
                self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
                return ValueTypeK::Bool.intern();
            }
            TokenType::Greater => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        self.function_compiler
                            .func
                            .chunk
                            .write(Opcode::GreaterInt.into(), line);
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        self.function_compiler
                            .func
                            .chunk
                            .write(Opcode::GreaterFloat.into(), line);
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                };
                return ValueTypeK::Bool.intern();
            }
            TokenType::GreaterEqual => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        self.function_compiler.func.chunk.write(Opcode::LessInt.into(), line);
                        self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        self.function_compiler
                            .func
                            .chunk
                            .write(Opcode::LessFloat.into(), line);
                        self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                };
                return ValueTypeK::Bool.intern();
            }
            TokenType::Less => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        self.function_compiler.func.chunk.write(Opcode::LessInt.into(), line);
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        self.function_compiler
                            .func
                            .chunk
                            .write(Opcode::LessFloat.into(), line);
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                };
                return ValueTypeK::Bool.intern();
            }
            TokenType::LessEqual => {
                match (lt, rt) {
                    (ValueTypeK::Integer, ValueTypeK::Integer) => {
                        self.function_compiler
                            .func
                            .chunk
                            .write(Opcode::GreaterInt.into(), line);
                        self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    (ValueTypeK::Float, ValueTypeK::Float) => {
                        self.function_compiler
                            .func
                            .chunk
                            .write(Opcode::GreaterFloat.into(), line);
                        self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return ValueTypeK::Err.intern();
                    }
                };
                return ValueTypeK::Bool.intern();
            }
            _ => unreachable!(),
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn ternary(&mut self, c: Expression, t: Expression, f: Expression) -> ValueType {
        let line = 0;
        let c_type = self.visit_expression(c, false);
        if c_type != ValueTypeK::Bool.intern() {
            error!(self, "Condition must be a boolean.");
        }
        // set up the jump to the else branch
        let jump_to_else = self.emit_jump(Opcode::JumpIfFalse.into());
        // pop the condition
        self.function_compiler.func.chunk.write_pop(c_type, line);
        // compile the then branch
        let t_type = self.visit_expression(t, false);
        // set up the jump to the end
        let jump_to_end = self.emit_jump(Opcode::Jump.into());
        // patch the jump to the else branch
        self.patch_jump(jump_to_else);
        // pop the condition
        self.function_compiler.func.chunk.write_pop(c_type, line);
        // compile the else branch
        let f_type = self.visit_expression(f, false);
        // patch the jump to the end
        self.patch_jump(jump_to_end);

        if t_type != f_type {
            error!(
                self,
                "Both branches of the ternary operator must have the same type."
            );
        }
        t_type
    }

    #[instrument(level = "trace", skip_all)]
    pub fn variable(&mut self, tok: Token, assignment_target: bool) -> ValueType {
        let line = 0;

        let (arg, assignable) = self.resolve_local(0, &tok);
        if let Some(i) = arg {
            if assignment_target {
                // if !assignable {
                //     error!(self, "Can't assign to const variable");
                // }
                self.function_compiler
                    .func
                    .chunk
                    .write_constant(Value::Pointer(Pointer::Local(i)), line);
                return ValueTypeK::Pointer(self.function_compiler.locals[&i].local_type, !assignable)
                    .intern();
            } else {
                self.function_compiler
                    .func
                    .chunk
                    .write_pool_opcode(Opcode::GetLocal, i as u32, line);
                let t = self.function_compiler.locals.get(&i).expect("local").local_type;
                self.function_compiler.func.chunk.write(t.num_words() as u8, line);
                return t;
            }
        } else if let Some(i) = self.resolve_upvalue(0, &tok) {
            if assignment_target {
                // if !assignable {
                //     error!(self, "Can't assign to const variable");
                // }
                self.function_compiler
                    .func
                    .chunk
                    .write_constant(Value::Pointer(Pointer::Upvalue(i)), line);
                return ValueTypeK::Pointer(self.function_compiler.upvalues[i].upvalue_type, !assignable)
                    .intern();
            } else {
                self.function_compiler
                    .func
                    .chunk
                    .write_pool_opcode(Opcode::GetUpvalue, i as u32, line);
                let t = self.function_compiler.upvalues[i].upvalue_type;
                self.function_compiler.func.chunk.write(t.num_words() as u8, line);
                return t;
            }
        } else if let Some(i) = self.gloabls.get(&tok.lexeme) {
            if assignment_target {
                if self.global_types.get(i).unwrap().1 == false
                    && self.global_types.get(&i).unwrap().2 == true
                {
                    error!(self, "Can't assign to const variable");
                }
                self.function_compiler
                    .func
                    .chunk
                    .write_constant(Value::Pointer(Pointer::Static(*i)), line);
                return ValueTypeK::Pointer(self.global_types.get(i).unwrap().0, !assignable)
                    .intern();
            } else {
                self.function_compiler
                    .func
                    .chunk
                    .write_pool_opcode(Opcode::GetGlobal, *i as u32, line);
                let t = self.global_types.get(i).unwrap().0;
                self.function_compiler.func.chunk.write(t.num_words() as u8, line);
                return t;
            }
        } else {
            error!(
                self,
                format!("Undefined variable '{}'.", tok.lexeme).as_str()
            );
            ValueTypeK::Err.intern()
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn assign(&mut self, l: Expression, r: Expression) -> ValueType {
        let line = 0;
        let t = self.visit_expression(l, true);

        let t2 = self.visit_expression(r, false);

        let ValueTypeK::Pointer(s, _is_const) = t.decay(self.custom_structs.clone().into()) else {
            error!(
                self,
                format!("Can only assign to a pointer type, got {:?}", t).as_str()
            );
            return ValueTypeK::Err.intern();
        };

        // if *is_const {
        //     error!(self, "Can't assign to const variable");
        // }

        if t2 != *s {
            error!(self, "Type mismatch");
        }

        self.function_compiler.func.chunk.write_pool_opcode(
            Opcode::DerefAssign,
            t2.num_words() as u32,
            line,
        );
        t2
    }

    #[instrument(level = "trace", skip_all)]
    pub fn logical(&mut self, l: Expression, operator: TokenType, r: Expression) -> ValueType {
        let line = 0;
        let operator_type = operator;
        let t = self.visit_expression(l, false);
        if t != ValueTypeK::Bool.intern() {
            error!(self, "Operand must be a boolean.");
        }
        if operator_type == TokenType::Or {
            let jump_to_end = self.emit_jump(Opcode::JumpIfTrue.into());
            self.function_compiler.func.chunk.write_pop(t, line);
            let t2 = self.visit_expression(r, false);
            if t2 != ValueTypeK::Bool.intern() {
                error!(self, "Operand must be a boolean.");
            }
            self.patch_jump(jump_to_end);
            return ValueTypeK::Bool.intern();
        } else {
            let jump_to_else = self.emit_jump(Opcode::JumpIfFalse.into());
            self.function_compiler.func.chunk.write_pop(t, line);
            let t2 = self.visit_expression(r, false);
            if t2 != ValueTypeK::Bool.intern() {
                error!(self, "Operand must be a boolean.");
            }
            self.patch_jump(jump_to_else);
            return ValueTypeK::Bool.intern();
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn call(&mut self, callee: Expression, args: Vec<Expression>) -> ValueType {
        let line = 0;
        let t = self.visit_expression(callee, false);

        let mut ts = Vec::new();
        for arg in args.clone() {
            ts.push(self.visit_expression(arg, false));
        }

        if let ValueTypeK::Closure(f) = t {
            self.function_compiler.func.chunk.write_pool_opcode(
                Opcode::Call,
                ts.iter().fold(0, |a, b| a + b.num_words() as u32),
                line,
            );

            let return_type = f.last().unwrap();
            return return_type;
        } else {
            error!(
                self,
                format!("Can only call function types, got {:?}", t).as_str()
            );
            return ValueTypeK::Err.intern();
        };
    }

    #[instrument(level = "trace", skip_all)]
    pub fn dot(&mut self, e: Expression, tok: Token, at: bool) -> ValueType {
        let line = 0;
        let t = self.visit_expression(e, true);

        let ValueTypeK::Pointer(ValueTypeK::Struct(s), false) = t.decay(self.custom_structs.clone().into()) else {
            error!(
                self,
                format!("Can only access fields of struct types, got {:?}", t).as_str()
            );
            return ValueTypeK::Err.intern();
        };

        let temp = s.fields.borrow();
        let Some(field) = temp.get(&tok.lexeme) else {
            error!(self, format!("Field '{}' not found have fields {:?}", tok.lexeme, s.fields).as_str());
            return ValueTypeK::Err.intern();
        };


        if !at {
            self.function_compiler
                .func
                .chunk
                .write_constant(Value::Integer(field.offset as i64), line);
            self.function_compiler
                .func
                .chunk
                .write(Opcode::PointerAdd.into(), line);
            self.function_compiler
                .func
                .chunk
                .write(Opcode::DerefGet.into(), line);
            self.function_compiler
                .func
                .chunk
                .write(field.value.num_words() as u8, line);
            return field.value;
        } else {
            self.function_compiler
                .func
                .chunk
                .write_constant(Value::Integer(field.offset as i64), line);
            self.function_compiler
                .func
                .chunk
                .write(Opcode::PointerAdd.into(), line);
            return ValueTypeK::Pointer(field.value, false).intern();
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn function(&mut self, function_expr: FunctionExpression) -> ValueType {
        let c = FunctionCompiler::new(
            super::FunctionType::Function,
            Some(std::mem::take(&mut self.function_compiler)),
            &function_expr.name.clone(),
        );
        self.function_compiler = Box::new(c);
        self.function_compiler.scope_depth += 1;

        //     loop {
        //         self.compiler.func.arity += 1;
        //         if self.compiler.func.arity > 255 {
        //             self.error_at_current("Cannot have more than 255 parameters.");
        //         }

        //         let mut mutable = true;
        //         if self.match_token(TokenType::Const) {
        //             mutable = false;
        //         }
        //         let id = self.parse_variable("Expect parameter name.", mutable);
        //         self.consume(TokenType::Colon, "Expect ':' after parameter name.");

        //         let p_type = self.parse_complex_type(!mutable);
        //         param_types.push(p_type);

        //         self.define_variable(id as u32, p_type);

        //         if !self.match_token(TokenType::Comma) {
        //             break;
        //         }
        //     }
        // }
        self.function_compiler.func.arity = function_expr.params.len();
        for (_, param) in function_expr.params.iter().enumerate() {
            let _mutable = true;

            let p_type = param.1;

            self.parse_variable(param.0.clone(), _mutable);
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = p_type;

            self.define_variable(0, p_type);
        }

        let return_type = function_expr.return_type;

        self.function_compiler.func.return_type = return_type.into();
        self.function_compiler.func.return_size = return_type.num_words() as usize;

        let mut param_types = function_expr
            .params
            .iter()
            .map(|(_, t)| *t)
            .collect::<Vec<_>>();
        param_types.push(return_type);

        let func_name = function_expr.name.clone();

        let ft = ValueTypeK::Closure(param_types.clone().as_slice().into()).intern();
        self.function_compiler.locals.insert(
            0,
            Local {
                name: func_name,
                depth: 0,
                mutable: false,
                assigned: false,
                captured: false,
                local_type: ft,
            },
        );
        // self.compiler.local_count += ft.num_words() as usize;

        for d in &function_expr.body {
            self.visit(d.clone());
        }

        self.emit_return();
        *param_types.last_mut().unwrap() = self.function_compiler.func.return_type;
        let c = std::mem::take(&mut self.function_compiler);
        let name = c.func.name.clone();
        // if self.had_error.get() {
        c.func.chunk.dissassemble(&name, Level::DEBUG);
        // }

        let (rfunc, maybe_enclosing, upvalues) = c.recover_values();
        let Some(enclosing) = maybe_enclosing else {
            error!(self, "Cannot return from top-level code.");
            return ValueTypeK::Err.intern();
        };
        let func = Rc::new(rfunc);

        self.function_compiler = enclosing;
        self.function_segment.push(Runnable::Function(func.clone()));
        self.num_functions += 1;
        let f_id = self.num_functions - 1;

        self.function_compiler
            .func
            .chunk
            .write_pool_opcode(Opcode::Closure, f_id as u32, 0);

        for i in 0..func.upvalue_count {
            self.function_compiler
                .func
                .chunk
                .write(upvalues[i].is_local as u8, 0);
            self.function_compiler.func.chunk.write(upvalues[i].index as u8, 0);
        }
        ft
    }

    #[instrument(level = "trace", skip_all)]
    pub fn cast(&mut self, value: Expression, cast_type: ValueType) -> ValueType {
        // cast(value, type)
        let line = 0;

        let t = self.visit_expression(value, false);

        match (t.decay(self.custom_structs.clone().into()), cast_type) {
            (x, y) if x == y => x,
            (ValueTypeK::Integer, ValueTypeK::Float) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastIntToFloat.into(), line);
                ValueTypeK::Float.intern()
            }
            (ValueTypeK::Float, ValueTypeK::Integer) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastFloatToInt.into(), line);
                ValueTypeK::Integer.intern()
            }
            (ValueTypeK::Integer, ValueTypeK::Bool) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastIntToBool.into(), line);
                ValueTypeK::Bool.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::Integer) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastBoolToInt.into(), line);
                ValueTypeK::Integer.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::Float) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastBoolToFloat.into(), line);
                ValueTypeK::Float.intern()
            }
            (ValueTypeK::Integer, ValueTypeK::String) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastIntToString.into(), line);
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Float, ValueTypeK::String) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastFloatToString.into(), line);
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::String) => {
                self.function_compiler
                    .func
                    .chunk
                    .write(Opcode::CastBoolToString.into(), line);
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

    pub fn struct_literal(
        &mut self,
        t: CustomStruct,
        fields: HashMap<String, Expression>,
    ) -> ValueType {
        let temp= t.fields.borrow();
        let mut orig_fields = temp.iter().collect::<Vec<_>>();
        orig_fields.sort_by(|a, b| a.1.offset.cmp(&b.1.offset));
        for (name, entry) in orig_fields.iter() {
            let Some(exp) = fields.get(*name) else {
                for _ in 0..entry.value.num_words() {
                    self.function_compiler.func.chunk.write(Opcode::Nil.into(), 0);
                }
                continue;
            };
            let ft = self.visit_expression(exp.clone(), false);
            if ft != entry.value {
                error!(self, format!("Field {} has type {:?}, expected {:?}", name, ft, entry.value).as_str());
                return ValueTypeK::Err.intern();
            }
        };
        drop(temp);
        ValueTypeK::Struct(t).intern()
    }

    fn emit_return(&mut self) {
        self.function_compiler.func.chunk.write(Opcode::Nil.into(), 0);
        self.function_compiler
            .func
            .chunk
            .write_pool_opcode(Opcode::Return.into(), 1, 0);
    }

    fn resolve_upvalue(&mut self, compiler_level: u32, token: &Token) -> Option<usize> {
        if let None = Self::compiler_at(compiler_level + 1, &mut self.function_compiler) {
            return None;
        };

        if let (Some(local), _) = self.resolve_local(compiler_level + 1, token) {
            let Some(compiler) = Self::compiler_at(compiler_level + 1, &mut self.function_compiler) else {
                return None;
            };

            compiler
                .locals
                .get_mut(&local)
                .expect("Local not found")
                .captured = true;
            let t = compiler
                .locals
                .get(&local)
                .expect("Local not found")
                .local_type;
            return Some(self.add_upvalue(local, true, compiler_level, t));
        };
        if let Some(upvalue) = self.resolve_upvalue(compiler_level + 1, token) {
            let Some(compiler) = Self::compiler_at(compiler_level + 1, &mut self.function_compiler) else {
                return None;
            };
            let t = compiler.upvalues[upvalue].upvalue_type;
            return Some(self.add_upvalue(upvalue, false, compiler_level, t));
        };

        None
    }

    fn compiler_at(level: u32, compiler: &mut FunctionCompiler) -> Option<&mut FunctionCompiler> {
        if level == 0 {
            return Some(compiler);
        } else {
            return compiler
                .enclosing
                .as_mut()
                .and_then(|e| Self::compiler_at(level - 1, e));
        }
    }

    fn add_upvalue(
        &mut self,
        idx: usize,
        is_local: bool,
        compiler_level: u32,
        upvalue_type: ValueType,
    ) -> usize {
        let compiler = Self::compiler_at(compiler_level, &mut self.function_compiler).unwrap();
        let upvalue_count = compiler.func.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &compiler.upvalues[i];
            if upvalue.index == idx && upvalue.is_local == is_local {
                return i;
            }
        }

        if upvalue_count == u8::MAX as usize {
            error!(self, "Trying to capture too many variables in closure");
            return 0;
        }
        compiler.upvalues[upvalue_count].is_local = is_local;
        compiler.upvalues[upvalue_count].index = idx;
        compiler.upvalues[upvalue_count].upvalue_type = upvalue_type;
        compiler.func.upvalue_count += 1;

        return compiler.func.upvalue_count - 1;
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
