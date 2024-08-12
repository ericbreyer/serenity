use std::{collections::HashMap, sync::atomic::AtomicUsize};

use tracing::instrument;

use crate::{
    lexer::{Token, TokenType},
    prelude::*,
    rvalue,
    value::{pointer::Pointer, Value},
};

use super::{
    iasm::{IASMCommandType, IASMLabel, IASMParam, IASMRegister},
    EmitRegWalker, FunctionCompiler, Local,
};

use crate::error;

impl EmitRegWalker {
    #[instrument(level = "trace", skip_all)]
    pub fn literal(&mut self, v: Value, line: usize) -> (IASMRegister, usize) {
        let t = match v {
            Value::Bool(false) => ValueType::Bool.intern(),
            Value::Bool(true) => ValueType::Bool.intern(),
            Value::Integer(_i) => ValueType::Integer.intern(),
            Value::Float(_f) => ValueType::Float.intern(),
            Value::Char(_c) => ValueType::Char.intern(),
            Value::UInteger(_) => ValueType::UInteger.intern(),
            Value::Pointer(_) => {
                error!(
                    self,
                    format!("[line {}] Cannot have pointer or closure literals", line).as_str()
                );
                ValueType::Err.intern()
            }
        };

        (self.write_constant(v, t, line), 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn string(&mut self, s: SharedString, line: usize) -> (IASMRegister, usize) {
        let mut value = s.to_string();
        value = value.trim_matches('"').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\a", "\x07");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        value.push('\0');

        let global_id = self.global_types.values().map(|i| i.0.num_words()).sum();

        static ID: AtomicUsize = AtomicUsize::new(0);

        self.gloabls.insert(
            format!(
                "String_literal_{}",
                ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            )
            .into(),
            global_id,
        );
        self.global_types.insert(
            global_id,
            (
                ValueType::Array(ValueType::Char.intern(), value.len()).intern(),
                false,
            ),
        );

        let reg = self.write_constant(
            Value::Pointer(Pointer::Static(global_id)),
            ValueType::Pointer(ValueType::Char.intern(), false).intern(),
            line,
        );

        for (i, c) in value.chars().enumerate() {
            let r = self.write_constant(Value::Char(c as u8), ValueType::Char.intern(), line);
            let place = self.write_constant(
                Value::Pointer(Pointer::Static(global_id + i)),
                ValueType::Pointer(ValueType::Char.intern(), false).intern(),
                line,
            );
            self.write_store(place, r, ValueType::Char.intern(), line);
        }

        (reg, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn unary(&mut self, token_t: TokenType, e: Expression, line: usize) -> (IASMRegister, usize) {
        let operator_type = token_t;
        // Compile the operand.
        let from_loc = rvalue!(self, self.visit_expression(e).0, line);
        let t = from_loc.get_type();
        let to_loc = IASMRegister::new(t);
        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus => match t.as_ref() {
                ValueType::Integer => self.write_statement(
                    IASMCommandType::NegateInt,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(from_loc)],
                    line,
                ),
                ValueType::UInteger => {
                    self.write_statement(
                        IASMCommandType::NegateUint,
                        vec![IASMParam::Register(to_loc), IASMParam::Register(from_loc)],
                        line,
                    );
                }
                ValueType::Float => self.write_statement(
                    IASMCommandType::NegateFloat,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(from_loc)],
                    line,
                ),
                _ => {
                    error!(
                        self,
                        format!("[line {}] Operand must be a number.", line).as_str()
                    );
                }
            },
            TokenType::Bang => match t.as_ref() {
                ValueType::Bool => self.write_statement(
                    IASMCommandType::Not,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(from_loc)],
                    line,
                ),
                _ => {
                    error!(
                        self,
                        format!("[line {}] Operand must be a boolean.", line).as_str()
                    );
                }
            },
            _ => unreachable!(),
        }
        (to_loc, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn deref(&mut self, e: Expression, line: usize) -> (IASMRegister, usize) {
        // Compile the operand.
        let from_loc = rvalue!(self, self.visit_expression(e.clone()).0, line);
        let raw_t = from_loc.get_type();

        let (pointee_type, ic) = match raw_t.decay(self.custom_structs.clone().into()).as_ref() {
            &ValueType::Pointer(pointee_type, ic) => (pointee_type, ic),
            _ => {
                error!(
                    self,
                    format!("[line {line}] Expected pointer type, got {:?}", *raw_t).as_str()
                );
                return (IASMRegister::nil_reg(), 0);
            }
        };

        (if !pointee_type.is_array() {
            // If the pointee type is not an aggregate type, return an lvalue
            self.map_reg_type(from_loc, ValueType::LValue(pointee_type, ic).intern(), line)
        } else {
            // If the pointee type is an aggregate type, return a pointer to the pointee type
            from_loc
        }, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn addr_of(&mut self, e: Expression, line: usize) -> (IASMRegister, usize) {
        // Compile the operand.
        let reg = self.visit_expression(e).0;
        let t = reg.get_type();
        (match t.as_ref() {
            &ValueType::LValue(pt, ic) => {
                self.map_reg_type(reg, ValueType::Pointer(pt, ic).intern(), line)
            }
            _ if t.is_aggregate() => {
                self.map_reg_type(reg, ValueType::Pointer(t, true).intern(), line)
            }
            _ => {
                error!(
                    self,
                    format!("[line {line}] Expected lvalue, got {:?}", *t).as_str()
                );
                self.map_reg_type(reg, ValueType::Err.intern(), line)
            }
        }, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn index(&mut self, e: Expression, i: Expression, line: usize) -> (IASMRegister, usize) {
        // Compile the operand.
        let reg_loc = rvalue!(self, self.visit_expression(e).0, line);
        let raw_t = reg_loc.get_type();
        let pointer_type = raw_t.decay(self.custom_structs.clone().into());
        let &ValueType::Pointer(pointee_type, ic) = pointer_type.as_ref() else {
            error!(
                self,
                format!("[line {line}] Expected pointer type, got {:?}", *raw_t).as_str()
            );
            return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
        };

        let idx_loc = rvalue!(self, self.visit_expression(i).0, line);
        let i_type = idx_loc.get_type();
        if i_type != ValueType::Integer.intern() {
            error!(
                self,
                format!("[line {line}] Index must be an integer.").as_str()
            );
        }

        let to_loc = IASMRegister::new(pointee_type);
        self.write_statement(
            IASMCommandType::PointerAdd,
            vec![
                IASMParam::Register(to_loc),
                IASMParam::Register(reg_loc),
                IASMParam::Register(idx_loc),
                IASMParam::Immediate(Value::UInteger(pointee_type.num_words() as u64)),
            ],
            line,
        );

        (if !pointee_type.is_array() {
            // If the pointee type is not an array type, return an lvalue
            self.map_reg_type(to_loc, ValueType::LValue(pointee_type, ic).intern(), line)
        } else {
            // If the pointee type is an array type, return a pointer to the pointee type
            to_loc
        }, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn binary(
        &mut self,
        l: Expression,
        operator: TokenType,
        r: Expression,
        line: usize,
    ) -> (IASMRegister, usize) {
        let operator_type = operator;
        let _line = 0;

        // Compile the left operand.
        let left_loc = rvalue!(self, self.visit_expression(l).0, line);
        let rlt = left_loc.get_type();
        let lt = rlt.decay(Some(self.custom_structs.clone()));

        // Compile the right operand.
        let right_loc = rvalue!(self, self.visit_expression(r).0, line);
        let rt = right_loc.get_type();
        // Emit the operator instruction.
        let (command, t) = match operator_type {
            TokenType::Plus => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::AddInt, ValueType::Integer.intern())
                }

                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::AddUint, ValueType::UInteger.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::AddFloat, ValueType::Float.intern())
                }
                (ValueType::Pointer(_, _), ValueType::Integer) => (IASMCommandType::PointerAdd, lt),
                (ValueType::Pointer(_, _), ValueType::UInteger) => {
                    (IASMCommandType::PointerAdd, lt)
                }
                _ => {
                    error!(
              self,
              format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer got {lt:?} and {rt:?}"
              ).as_str()
            );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::Minus => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::SubInt, ValueType::Integer.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::SubUint, ValueType::UInteger.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::SubFloat, ValueType::Float.intern())
                }
                (ValueType::Pointer(_, _), ValueType::Integer) => (IASMCommandType::PointerSub, lt),
                (ValueType::Pointer(_, _), ValueType::UInteger) => {
                    (IASMCommandType::PointerSub, lt)
                }
                _ => {
                    error!(
                        self,
                        format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer"
              )
                        .as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::Star => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::MulInt, ValueType::Integer.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::MulUint, ValueType::UInteger.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::MulFloat, ValueType::Float.intern())
                }
                _ => {
                    error!(
                        self,
                        format!("[line {line}] Operands must be two numbers").as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::Slash => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::DivInt, ValueType::Integer.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::DivUint, ValueType::UInteger.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::DivFloat, ValueType::Float.intern())
                }
                _ => {
                    error!(
                        self,
                        format!("[line {line}] Operands must be two numbers").as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::EqualEqual => {
                if rt != lt {
                    self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always false, please cast to the same type"
            )
          );
                }

                (IASMCommandType::Equal, ValueType::Bool.intern())
            }
            TokenType::BangEqual => {
                if rt != lt {
                    self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always true, please cast to the same type"
            )
          );
                }

                (IASMCommandType::NotEqual, ValueType::Bool.intern())
            }
            TokenType::Greater => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::GreaterInt, ValueType::Bool.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::GreaterFloat, ValueType::Bool.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::GreaterUint, ValueType::Bool.intern())
                }
                _ => {
                    error!(
                        self,
                        format!("[line {line}] Operands must be two numbers").as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::GreaterEqual => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::GreaterEqualInt, ValueType::Bool.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::GreaterEqualFloat, ValueType::Bool.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::GreaterEqualUint, ValueType::Bool.intern())
                }
                _ => {
                    error!(
                        self,
                        format!("[line {line}] Operands must be two numbers").as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::Less => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::LessInt, ValueType::Bool.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::LessFloat, ValueType::Bool.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::LessUint, ValueType::Bool.intern())
                }
                _ => {
                    error!(
                        self,
                        format!("[line {line}] Operands must be two numbers").as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            TokenType::LessEqual => match (lt.as_ref(), rt.as_ref()) {
                (ValueType::Integer, ValueType::Integer) => {
                    (IASMCommandType::LessEqualInt, ValueType::Bool.intern())
                }
                (ValueType::Float, ValueType::Float) => {
                    (IASMCommandType::LessEqualFloat, ValueType::Bool.intern())
                }
                (ValueType::UInteger, ValueType::UInteger) => {
                    (IASMCommandType::LessEqualUint, ValueType::Bool.intern())
                }
                _ => {
                    error!(
                        self,
                        format!("[line {line}] Operands must be two numbers").as_str()
                    );
                    (IASMCommandType::Nop, ValueType::Err.intern())
                }
            },
            _ => unreachable!(),
        };
        let to_loc = IASMRegister::new(t);

        self.write_statement(
            command,
            vec![
                IASMParam::Register(to_loc),
                IASMParam::Register(left_loc),
                IASMParam::Register(right_loc),
            ],
            line,
        );

        (to_loc, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn ternary(
        &mut self,
        c: Expression,
        t: Expression,
        f: Expression,
        line: usize,
    ) -> (IASMRegister, usize) {
        let cond_loc = rvalue!(self, self.visit_expression(c).0, line);
        let c_type = cond_loc.get_type();
        if c_type != ValueType::Bool.intern() {
            error!(
                self,
                format!("[line {line}] Condition must be a boolean.").as_str()
            );
        }

        let else_jump = IASMLabel::new_mangled("else");
        let end_jump = IASMLabel::new_mangled("end");

        self.write_statement(
            IASMCommandType::JumpIfFalse,
            vec![
                IASMParam::Register(cond_loc),
                IASMParam::Label(else_jump.clone()),
            ],
            line,
        );

        // compile the then branch
        let than_loc = rvalue!(self, self.visit_expression(t).0, line);
        let t_type = than_loc.get_type();
        let to_loc = IASMRegister::new(t_type);
        self.write_statement(
            IASMCommandType::Move,
            vec![IASMParam::Register(to_loc), IASMParam::Register(than_loc)],
            line,
        );
        self.write_statement(
            IASMCommandType::Jump,
            vec![IASMParam::Label(end_jump.clone())],
            line,
        );
        self.write_label(&else_jump);
        // compile the else branch
        let else_loc = rvalue!(self, self.visit_expression(f).0, line);
        let f_type = else_loc.get_type();
        if t_type != f_type {
            error!(
                self,
                format!("[line {line}] Branches must have the same type.").as_str()
            );
        }

        self.write_statement(
            IASMCommandType::Move,
            vec![IASMParam::Register(to_loc), IASMParam::Register(else_loc)],
            line,
        );

        self.write_label(&end_jump);

        (to_loc, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn variable(&mut self, tok: Token, line: usize) -> (IASMRegister, usize) {
        let (addr, t, a): (IASMRegister, UValueType, bool);
        let (arg, assignable) = self.resolve_local(0, &tok);
        if arg.is_none() {
            let Some(i) = self.gloabls.get(&tok.lexeme).cloned() else {
                error!(
                    self,
                    format!("[line {line}] Undefined variable '{}'.", tok.lexeme).as_str()
                );
                return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
            };
            t = self.global_types.get(&i).unwrap().0;
            a = self.global_types.get(&i).unwrap().1;
            addr = self.write_constant(Value::Pointer(Pointer::Static(i)), t, line);
        } else {
            let arg = arg.unwrap();

            t = self.function_compiler.locals.get(&arg).unwrap().local_type;
            a = assignable;
            addr = self.write_constant(Value::Pointer(Pointer::Local(arg)), t, line);
        }

        (if t.is_array() {
             self.map_reg_type(addr, t, line)
        } else {
             self.map_reg_type(addr, ValueType::LValue(t, a).intern(), line)
        },0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn assign(&mut self, l: Expression, r: Expression, line: usize) -> (IASMRegister, usize) {

        let f_loc = rvalue!(self, self.visit_expression(r).0, line);
        let t2 = f_loc.get_type();
        let s_loc = self.visit_expression(l.clone()).0;
        let t = s_loc.get_type();
        let decayed = t.decay(self.custom_structs.clone().into());
        let ValueType::LValue(s, assignable) = decayed.as_ref() else {
            error!(
                self,
                format!("[line {line}] Can only assign to a pointer type, got {t:?}").as_str()
            );
            return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), 0), 0);
        };

        if !assignable {
            error!(
                self,
                format!("[line {line}] Can't assign to const variable {l:?}").as_str()
            );
        }

        if t2 != *s {
            error!(
                self,
                format!("[line {line}] Cannot assign {t2:?} to variable of type {s:?}").as_str()
            );
        }

        self.write_store(s_loc, f_loc, t2, line);

        (f_loc, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn logical(
        &mut self,
        l: Expression,
        operator: TokenType,
        r: Expression,
        line: usize,
    ) -> (IASMRegister, usize) {
        let operator_type = operator;

        let end_jump = IASMLabel::new_mangled("end");
        let l_loc = rvalue!(self, self.visit_expression(l).0, line);
        let t = l_loc.get_type();
        if t != ValueType::Bool.intern() {
            error!(
                self,
                format!("[line {line}] Operand must be a boolean.").as_str()
            );
        }
        let to_loc = IASMRegister::new(t);
        self.write_statement(
            IASMCommandType::Move,
            vec![IASMParam::Register(to_loc), IASMParam::Register(l_loc)],
            line,
        );

        match operator_type {
            TokenType::Or => {
                self.write_statement(
                    IASMCommandType::JumpIfTrue,
                    vec![
                        IASMParam::Register(l_loc),
                        IASMParam::Label(end_jump.clone()),
                    ],
                    line,
                );
            }
            TokenType::And => {
                self.write_statement(
                    IASMCommandType::JumpIfFalse,
                    vec![
                        IASMParam::Register(l_loc),
                        IASMParam::Label(end_jump.clone()),
                    ],
                    line,
                );
            }
            _ => unreachable!(),
        }

        let r_loc = rvalue!(self, self.visit_expression(r).0, line);
        let t2 = r_loc.get_type();
        if t2 != ValueType::Bool.intern() {
            error!(
                self,
                format!("[line {line}] Operand must be a boolean.").as_str()
            );
        }

        self.write_statement(
            IASMCommandType::Move,
            vec![IASMParam::Register(to_loc), IASMParam::Register(r_loc)],
            line,
        );

        self.write_label(&end_jump);

        (to_loc, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn call(&mut self, callee: Expression, args: Vec<Expression>, line: usize) -> (IASMRegister, usize) {
        let mut callee_loc = rvalue!(self, self.visit_expression(callee.clone()).0, line);
        let mut t = callee_loc.get_type();
        if let ValueType::Pointer(_, _) = t.as_ref() {
            callee_loc = rvalue!(
                self,
                self.visit_expression(Expression::Deref(DerefExpression{ operand: Box::new(callee), line_no:  line})).0,
                line
            );
            t = callee_loc.get_type();
        }

        let mut ts = Vec::new();
        let mut arg_locs = Vec::new();
        for arg in args.clone() {
            let ndarg = rvalue!(self, self.visit_expression(arg).0, line);
            let t = ndarg.get_type();
            ts.push(t);
            arg_locs.push(ndarg);
        }
        let t = t.decay(self.custom_structs.clone().into());

        if let ValueType::Closure(f, upvals) = t.as_ref() {
            if ts
                .iter()
                .map(|x| x.decay(None))
                .collect::<Vec<UValueType>>()
                != f[0..f.len() - 1]
            {
                error!(
                    self,
                    format!(
            "[line {line}] Argument types do not match function signature. Expected {:?}, got {:?}",
            f[0..f.len() - 1].to_vec(),
            ts
          )
                    .as_str()
                );
            }
            let return_loc: IASMRegister;

            if f.last().unwrap().is_aggregate() {
                return_loc = self.write_constant(
                    Value::Pointer(Pointer::Local(self.function_compiler.local_count)),
                    *f.last().unwrap(),
                    line,
                );
                self.make_room_on_stack_for(*f.last().unwrap(), line);
            } else {
                return_loc = IASMRegister::new(*f.last().unwrap());
            }

            let mut call_args = Vec::new();
            call_args.push(IASMParam::Register(return_loc));
            call_args.push(IASMParam::Register(callee_loc));
            call_args.push(IASMParam::Immediate(Value::UInteger(*upvals as u64)));

            self.write_statement(IASMCommandType::PreCall, call_args, line);

            let sp = 3;
            // push params to stack
            for (i, t) in ts.iter().enumerate() {
                let new_val_loc = self.write_constant(
                    Value::Pointer(Pointer::Local(sp + i)),
                    ValueType::Pointer(*t, true).intern(),
                    line,
                );
                let new_val = arg_locs[i];
                for _ in 0..t.num_words() {
                    self.write_statement(
                        IASMCommandType::Push,
                        vec![IASMParam::Register(IASMRegister::nil_reg())],
                        line,
                    );
                    self.function_compiler.push_count += 1;
                }
                self.write_store(new_val_loc, new_val, *t, line);
            }

            // push upvals to stack
            for i in 0..*upvals {
                let from_loc = IASMRegister::new(
                    ValueType::Pointer(ValueType::UInteger.intern(), false).intern(),
                );
                self.write_statement(
                    IASMCommandType::PointerAdd,
                    vec![
                        IASMParam::Register(from_loc),
                        IASMParam::Register(callee_loc),
                        IASMParam::Immediate(Value::UInteger(i as u64)),
                    ],
                    line,
                );
                let from = IASMRegister::new(ValueType::UInteger.intern());
                self.write_load(from, from_loc, ValueType::UInteger.intern(), line);
                self.write_statement(IASMCommandType::Push, vec![IASMParam::Register(from)], line);
                self.function_compiler.push_count += 1;
            }

            self.write_statement(IASMCommandType::Call, vec![], line);
            return (return_loc, f.last().unwrap().num_words());
        } else {
            error!(
                self,
                format!("[line {line}] Can only call function types, got {t:?}").as_str()
            );
            return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn dot(&mut self, e: Expression, tok: Token, line: usize) -> (IASMRegister, usize) {
        let _line = 0;
        let s_loc = rvalue!(self, self.visit_expression(e.clone()).0, line);
        let t = s_loc.get_type();

        let ValueType::Struct(s) = t.as_ref() else {
            error!(
                self,
                format!("[line {line}] Can only access fields of struct types, got {t:?} in {e:?}")
                    .as_str()
            );
            return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
        };

        let m_name: SharedString = format!("{}_{}", t.unique_string(), tok.lexeme).into();
        if s.methods.borrow().contains(&m_name) {
            let method_id = self.gloabls.get(&m_name).unwrap();
            let method_type = self.global_types.get(method_id).unwrap().0;
            let ValueType::Closure(_f, _) = method_type.as_ref() else {
                error!(
                    self,
                    format!("[line {line}] Expected method type, got {method_type:?}").as_str()
                );
                return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
            };
            let r = self.call(
                Expression::Variable(
                    Token {
                        token_type: TokenType::Identifier,
                        lexeme: m_name.clone(),
                        line: line,
                    },
                    line,
                ),
                vec![Expression::Ref(Box::new(e.clone()), line)],
                line,
            );
            return r;
        }

        let f = s.fields.borrow();
        let Some(field) = f.get(&tok.lexeme) else {
            error!(
                self,
                format!(
                    "[line {line}] Field '{}' not found have fields {:?}",
                    tok.lexeme, s.fields
                )
                .as_str()
            );
            return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
        };

        let new_loc = IASMRegister::new(field.value);
        self.write_statement(
            IASMCommandType::PointerAdd,
            vec![
                IASMParam::Register(new_loc),
                IASMParam::Register(s_loc),
                IASMParam::Immediate(Value::UInteger(field.offset as u64)),
            ],
            line,
        );

        (if field.value.is_array() {
             new_loc
        } else {
            self.map_reg_type(new_loc, ValueType::LValue(field.value, true).intern(), line)
        }, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn function(&mut self, function_expr: FunctionExpression, line: usize) -> (IASMRegister, usize) {
        let mut capture_size = 0;
        let mut captures: Vec<(SharedString, UValueType)> = Vec::new();
        for cap in &function_expr.captures {
            let cap_loc = rvalue!(
                self,
                rvalue!(
                    self,
                    self.variable(
                        Token {
                            lexeme: cap.clone(),
                            token_type: TokenType::Identifier,
                            line
                        },
                        line
                    ).0,
                    line
                ),
                line
            );
            let t = cap_loc.get_type();

            self.make_room_on_stack_for(t, line);

            let to_loc = self.write_constant(
                Value::Pointer(Pointer::Local(self.function_compiler.local_count + capture_size)),
                ValueType::Pointer(t, false).intern(),
                line,
            );
            self.write_store(to_loc, cap_loc, t, line);

            capture_size += t.num_words();
            captures.push((cap.clone(), t));
            
            // self.function_compiler.local_count += t.num_words();
            

        }
        let stack_place = self.function_compiler.local_count;

        let c = FunctionCompiler::new(
            Some(std::mem::take(&mut self.function_compiler)),
            &function_expr.name.clone(),
        );
        let func_label = self.text.start_function(&function_expr.name);
        self.function_compiler = Box::new(c);
        self.function_compiler.scope_depth += 1;

        let return_type = function_expr.return_type;
        self.function_compiler.return_type = return_type;

        let mut param_types = function_expr
            .params
            .iter()
            .map(|(_, t, _)| *t)
            // .map(|t| if t.is_aggregate() { ValueType::Pointer(t, false).intern() } else { t })
            .collect::<Vec<_>>();
        param_types.push(return_type);

        let func_name = function_expr.name.clone();

        let temp = param_types.clone();
        let ft = temp.as_slice();
        self.function_compiler.locals.insert(
            0,
            Local {
                name: func_name,
                depth: 0,
                mutable: false,
                assigned: false,
                captured: false,
                local_type: ValueType::Pointer(
                    ValueType::Closure(ft.into(), capture_size).intern(),
                    false,
                )
                .intern(),
            },
        );
        // self.function_compiler.local_count += ;

        for param in &function_expr.params {
            let mutable = param.2;

            let p_type = param.1;

            self.parse_variable(param.0.clone(), mutable);
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = p_type;
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .mutable = mutable;
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .assigned = true;
            if p_type.is_aggregate() {
                self.function_compiler.local_count += p_type.num_words();
                self.function_compiler.push_count += p_type.num_words();
            }
            self.define_variable(0, p_type);
        }

        for cap in captures {
            let t = cap.1;
            self.function_compiler.locals.insert(
                self.function_compiler.local_count,
                Local {
                    name: cap.0,
                    depth: 0,
                    mutable: false,
                    assigned: false,
                    captured: true,
                    local_type: t,
                },
            );
            self.function_compiler.local_count += t.num_words();
        }

        let mut does_return = false;
        for d in &function_expr.body {
            let r = self.visit(d.clone());
            does_return = does_return || r;
        }

        if !does_return && return_type == ValueType::Nil.intern() {
            does_return = self.visit_statement(Statement::Return(None, line));
        }

        if !does_return {
            error!(
                self,
                format!(
                    "[line {line}] Function \"{}\" must return a value in all code paths",
                    function_expr.name
                )
                .as_str()
            );
        }

        *param_types.last_mut().unwrap() = self.function_compiler.return_type;
        let c = std::mem::take(&mut self.function_compiler);
        let _locals = c.locals.clone();

        let maybe_enclosing = c.recover_enclosing();
        let Some(enclosing) = maybe_enclosing else {
            error!(self, "[line {line}] Cannot return from top-level code.");
            return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
        };

        self.function_compiler = enclosing;
        self.text.end_function();

        // let c_loc = self.write_constant(Value::Pointer(Pointer::Function(func_label.into())),ValueType::Pointer(ValueType::Nil.intern(), false).intern(), line);
        self.write_statement(
            IASMCommandType::Push,
            vec![IASMParam::Label(func_label)],
            line,
        );
        self.function_compiler.push_count += 1;
        // self.function_compiler.local_count += 1;

        let typ = ValueType::Closure(param_types.clone().as_slice().into(), capture_size).intern();
        let r_loc = self.write_constant(Value::Pointer(Pointer::Local(stack_place)), typ, line);

        (r_loc, typ.num_words())
    }

    #[instrument(level = "trace", skip_all)]
    pub fn cast(
        &mut self,
        value: Expression,
        mcast_type: Option<UValueType>,
        line: usize,
    ) -> (IASMRegister, usize) {
        let loc = rvalue!(self, self.visit_expression(value).0, line);
        let t = loc.get_type();

        let Some(cast_type) = mcast_type else {
            if let ValueType::Pointer(ms, b) = t.decay(self.custom_structs.clone().into()).as_ref()
            {
                if let ValueType::Struct(s) = ms.decay(self.custom_structs.clone().into()).as_ref()
                {
                    if s.embed.is_some() {
                        return (self.map_reg_type(
                            loc,
                            ValueType::Pointer(
                                self.custom_structs
                                    .get(s.embed.as_ref().unwrap())
                                    .map(|s| ValueType::Struct(s.clone()).intern())
                                    .unwrap_or(ValueType::Err.intern()),
                                *b,
                            )
                            .intern(),
                            line,
                        ), 0);
                    }
                }
            }
            return (loc, 0);
        };
        let to_loc = IASMRegister::new(cast_type);
        (match (
            t.decay(self.custom_structs.clone().into()).as_ref(),
            cast_type.as_ref(),
        ) {
            (x, y) if x == y => self.map_reg_type(loc, y.intern(), line),
            (ValueType::Integer, ValueType::Float) => {
                self.write_statement(
                    IASMCommandType::CastIntToFloat,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Float, ValueType::Integer) => {
                self.write_statement(
                    IASMCommandType::CastFloatToInt,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Integer, ValueType::Bool) => {
                self.write_statement(
                    IASMCommandType::CastIntToBool,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Bool, ValueType::Integer) => {
                self.write_statement(
                    IASMCommandType::CastBoolToInt,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Bool, ValueType::Float) => {
                self.write_statement(
                    IASMCommandType::CastBoolToFloat,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::UInteger, ValueType::Integer) => {
                self.write_statement(
                    IASMCommandType::CastUintToInt,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::UInteger, ValueType::Float) => {
                self.write_statement(
                    IASMCommandType::CastUintToFloat,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Integer, ValueType::UInteger) => {
                self.write_statement(
                    IASMCommandType::CastIntToUint,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::UInteger, ValueType::Bool) => {
                self.write_statement(
                    IASMCommandType::CastUintToBool,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Bool, ValueType::UInteger) => {
                self.write_statement(
                    IASMCommandType::CastBoolToUint,
                    vec![IASMParam::Register(to_loc), IASMParam::Register(loc)],
                    line,
                );
                to_loc
            }
            (ValueType::Array(x, _), ValueType::Pointer(y, _)) if x == y => {
                self.map_reg_type(loc, ValueType::Pointer(*x, false).intern(), line)
            }
            (ValueType::Pointer(_, _), ValueType::Pointer(_, _)) => {
                self.map_reg_type(loc, cast_type, line)
            }
            (ValueType::Integer, ValueType::Pointer(_, _)) => {
                let c = self.write_constant(
                    Value::UInteger(1 << 62),
                    ValueType::UInteger.intern(),
                    line,
                );
                self.write_statement(
                    IASMCommandType::AddUint,
                    vec![
                        IASMParam::Register(to_loc),
                        IASMParam::Register(loc),
                        IASMParam::Register(c),
                    ],
                    line,
                );
                self.map_reg_type(to_loc, cast_type, line)
            }
            _ => {
                error!(
                    self,
                    &format!("[line {line}] Cannot cast {t:?} to {cast_type:?}")
                );
                self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line)
            }
        }, 0)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn struct_literal(
        &mut self,
        t: CustomStruct,
        fields: HashMap<SharedString, Expression>,
        line: usize,
    ) -> (IASMRegister, usize) {
        let f = t.fields.borrow();
        let mut orig_fields = f.iter().collect::<Vec<_>>();
        orig_fields.sort_by(|a, b| a.1.offset.cmp(&b.1.offset));

        let my_stack_pos = self.function_compiler.local_count;
        for (name, entry) in &orig_fields {
            let Some(exp) = fields.get(*name) else {
                panic!("Field {} has no value, defaulting to 0", name);
                // self.make_room_on_stack_for(ft, line);
                continue;
            };
            let expr_res = self.visit_expression(exp.clone());
            let exp_loc = rvalue!(self, expr_res.0, line);
            let stack_increased_by = expr_res.1;
            let ft = exp_loc.get_type();

            if !ft.is_aggregate() || stack_increased_by == 0{
                self.make_room_on_stack_for(ft, line);
                let place = self.write_constant(
                    Value::Pointer(Pointer::Local(self.function_compiler.local_count)),
                    ValueType::Pointer(ft, true).intern(),
                    line,
                );

                self.write_store(place, exp_loc, ft, line);
            }

            if ft != entry.value {
                error!(
                    self,
                    format!(
                        "[line {line}] Field {} has type {:?}, expected {:?}",
                        name, ft, entry.value
                    )
                    .as_str()
                );
                return (self.map_reg_type(IASMRegister::nil_reg(), ValueType::Err.intern(), line), 0);
            }
            self.function_compiler.local_count += ft.num_words();
        }

        self.function_compiler.local_count = my_stack_pos;
        (self.write_constant(
            Value::Pointer(Pointer::Local(my_stack_pos)),
            ValueType::Struct(t.clone()).intern(),
            line,
        ), ValueType::Struct(t.clone()).intern().num_words())
    }

    fn compiler_at(level: u32, compiler: &mut FunctionCompiler) -> Option<&mut FunctionCompiler> {
        if level == 0 {
            Some(compiler)
        } else {
            return compiler
                .enclosing
                .as_mut()
                .and_then(|e| Self::compiler_at(level - 1, e));
        }
    }

    fn resolve_local(&mut self, compiler_level: u32, token: &Token) -> (Option<usize>, bool) {
        let Some(compiler) = Self::compiler_at(compiler_level, &mut self.function_compiler) else {
            return (None, false);
        };
        if compiler.locals.is_empty() {
            return (None, true);
        }
        for (k, local) in &compiler.locals {
            if token.lexeme == local.name {
                if local.depth == -1 {
                    error!(self, "Cannot read local variable in its own initializer.");
                    return (None, true);
                }
                return (Some(*k), local.mutable || !local.assigned);
            }
        }
        return (None, false);
    }
}
