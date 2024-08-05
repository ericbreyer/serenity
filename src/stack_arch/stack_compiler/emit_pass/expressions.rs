use std::{ collections::HashMap, rc::Rc };

use tracing::{ instrument, warn, Level };

use crate::{
  chunk::Opcode,
  common::{ declared_ast::{ Expression, FunctionExpression }, runnable::Runnable },
  lexer::{ Token, TokenType },
  typing::{ CustomStruct, UValueType, ValueType },
  value::{ pointer::Pointer, Value },
};

use super::{ EmitWalker, FunctionCompiler };

use crate::error;

impl EmitWalker {
  #[instrument(level = "trace", skip_all)]
  pub fn literal(&mut self, v: Value, _line: usize) -> UValueType {
    let line = 0;
    match v {
      Value::Bool(false) => {
        self.function_compiler.func.chunk.write(Opcode::False.into(), line);
        ValueType::Bool.intern()
      }
      Value::Nil => {
        self.function_compiler.func.chunk.write(Opcode::Nil.into(), line);
        ValueType::Nil.intern()
      }
      Value::Bool(true) => {
        self.function_compiler.func.chunk.write(Opcode::True.into(), line);
        ValueType::Bool.intern()
      }
      Value::Integer(i) => {
        self.function_compiler.func.chunk.write_constant(Value::Integer(i), line);
        ValueType::Integer.intern()
      }
      Value::UInteger(i) => {
        self.function_compiler.func.chunk.write_constant(Value::UInteger(i), line);
        ValueType::UInteger.intern()
      }
      Value::Float(f) => {
        self.function_compiler.func.chunk.write_constant(Value::Float(f), line);
        ValueType::Float.intern()
      }
      Value::Char(c) => {
        self.function_compiler.func.chunk.write_constant(Value::Char(c), line);
        ValueType::Char.intern()
      }
      _ => {
        error!(self, format!("Unknown literal type: {v:?} at line {line}").as_str());
        ValueType::Err.intern()
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn SharedString(&mut self, s: SharedString, _line: usize) -> UValueType {
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
    self.static_data_segment.extend(Value::Integer(value.len() as i64).to_words());
    for c in value.chars() {
      self.static_data_segment.extend(Value::Char(c as u8).to_words());
    }
    self.function_compiler.func.chunk.write_constant(Value::Pointer(Pointer::Static(start)), line);
    return ValueType::Pointer(ValueType::Char.intern(), false).intern()
  }

  #[instrument(level = "trace", skip_all)]
  pub fn unary(&mut self, token_t: TokenType, e: Expression, _line: usize) -> UValueType {
    let line = 0;
    let operator_type = token_t;
    // Compile the operand.
    let t = self.visit_expression(e, false);

    // Emit the operator instruction.
    match operator_type {
      TokenType::Minus =>
        match t.as_ref() {
          ValueType::Integer => {
            self.function_compiler.func.chunk.write(Opcode::NegateInt.into(), line);
          }
          ValueType::Float => {
            self.function_compiler.func.chunk.write(Opcode::NegateFloat.into(), line);
          }
          _ => {
            error!(self, format!("[line {line}] Operand must be a number, got {t:?}").as_str());
          }
        }
      TokenType::Bang => self.function_compiler.func.chunk.write(Opcode::Not.into(), line),
      _ => unreachable!(),
    }
    t
  }

  #[instrument(level = "trace", skip_all)]
  pub fn deref(&mut self, e: Expression, at: bool, line: usize) -> UValueType {
    // Compile the operand.
    let raw_t = self.visit_expression(e, false);
    let (t, _) = match raw_t.decay(self.custom_structs.clone().into()).as_ref() {
      &ValueType::Pointer(pointee_type, is_const) => (pointee_type, is_const),
      _ => {
        error!(self, format!("[line {line}] Expected pointer type, got {raw_t:?}").as_str());
        (ValueType::Err.intern(), false)
      }
    };

    if !at {
      self.function_compiler.func.chunk.write(Opcode::DerefGet.into(), line);
      self.function_compiler.func.chunk.write(t.num_words() as u8, line);
      t
    } else {
      raw_t
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn addr_of(&mut self, e: Expression, line: usize) -> UValueType {
    // Compile the operand.
    let t = self.visit_expression(e, true);

    match t.as_ref() {
      ValueType::Pointer(_, _) => t,
      _ => {
        error!(self, format!("[line {line}] Expected pointer type, got {t:?}").as_str());
        ValueType::Err.intern()
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn index(&mut self, e: Expression, i: Expression, at: bool, _line: usize) -> UValueType {
    let line = 0;

    // Compile the operand.
    let raw_t = self.visit_expression(e, false);
    let pointer_type = raw_t.decay(self.custom_structs.clone().into());
    let &ValueType::Pointer(pointee_type, _) = pointer_type.as_ref() else {
      error!(self, format!("[line {line}] Expected pointer type, got {raw_t:?}").as_str());
      return ValueType::Err.intern();
    };

    if raw_t == ValueType::SharedString(0).intern() {
      self.function_compiler.func.chunk.write_constant(Value::Integer(1), line);
      self.function_compiler.func.chunk.write(Opcode::PointerAdd as u8, line);
      self.function_compiler.func.chunk.write(1, line);
    }

    let i_type = self.visit_expression(i, false);
    if i_type != ValueType::Integer.intern() {
      error!(self, format!("[line {line}] Index must be an integer, got {i_type:?}").as_str());
    }

    // self.function_compiler.func.chunk.write_constant(
    //     Value::Integer(pointee_type.num_words() as i64),
    //     line
    // );
    // self.function_compiler.func.chunk.write(Opcode::MultiplyInt as u8, line);
    self.function_compiler.func.chunk.write(Opcode::PointerAdd as u8, line);
    self.function_compiler.func.chunk.write(pointee_type.num_words() as u8, line);

    if !at {
      self.function_compiler.func.chunk.write(Opcode::DerefGet.into(), line);
      self.function_compiler.func.chunk.write(pointee_type.num_words() as u8, line);
      pointee_type
    } else {
      raw_t
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn binary(
    &mut self,
    l: Expression,
    operator: TokenType,
    r: Expression,
    _line: usize
  ) -> UValueType {
    let operator_type = operator;
    let line = 0;

    // Compile the left operand.
    let lt = self.visit_expression(l, false).decay(self.custom_structs.clone().into());

    // Compile the right operand.
    let rt = self.visit_expression(r, false);

    // Emit the operator instruction.
    match operator_type {
      TokenType::Plus =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::AddInt.into(), line);
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::AddUint.into(), line);
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::AddFloat.into(), line);
            return ValueType::Float.intern();
          }
          (ValueType::Pointer(pointee_t, _), ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::PointerAdd.into(), line);
            self.function_compiler.func.chunk.write(pointee_t.num_words() as u8, line);
            return lt;
          }
          (ValueType::Pointer(pointee_t, _), ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::PointerAdd.into(), line);
            self.function_compiler.func.chunk.write(pointee_t.num_words() as u8, line);
            return lt;
          }
          _ => {
            error!(
              self,
              format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer."
              ).as_str()
            );
            return ValueType::Err.intern();
          }
        }
      TokenType::Minus =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::SubtractInt.into(), line);
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::SubtractUint.into(), line);
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::SubtractFloat.into(), line);
            return ValueType::Float.intern();
          }
          (ValueType::Pointer(pointee_t, _), ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::PointerSubtract.into(), line);
            self.function_compiler.func.chunk.write(pointee_t.num_words() as u8, line);

            return lt;
          }
          (ValueType::Pointer(pointee_t, _), ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::PointerSubtract.into(), line);
            self.function_compiler.func.chunk.write(pointee_t.num_words() as u8, line);

            return lt;
          }
          _ => {
            error!(
              self,
              format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer."
              ).as_str()
            );
            return ValueType::Err.intern();
          }
        }
      TokenType::Star =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::MultiplyInt.into(), line);
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::MultiplyUint.into(), line);
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::MultiplyFloat.into(), line);
            return ValueType::Float.intern();
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers.").as_str());
            return ValueType::Err.intern();
          }
        }
      TokenType::Slash =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::DivideInt.into(), line);
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::DivideUint.into(), line);
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::DivideFloat.into(), line);
            return ValueType::Float.intern();
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers.").as_str());
            return ValueType::Err.intern();
          }
        }
      TokenType::EqualEqual => {
        if rt != lt {
          self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always false, please cast to the same type"
            )
          );
          self.function_compiler.func.chunk.write_pop(rt, line);
          self.function_compiler.func.chunk.write_pop(lt, line);
          self.function_compiler.func.chunk.write(Opcode::False.into(), line);
          return ValueType::Bool.intern();
        }
        self.function_compiler.func.chunk.write(Opcode::Equal.into(), line);
        return ValueType::Bool.intern();
      }
      TokenType::BangEqual => {
        if rt != lt {
          self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always true, please cast to the same type"
            )
          );
          self.function_compiler.func.chunk.write_pop(rt, line);
          self.function_compiler.func.chunk.write_pop(lt, line);
          self.function_compiler.func.chunk.write(Opcode::True.into(), line);
          return ValueType::Bool.intern();
        }
        self.function_compiler.func.chunk.write(Opcode::Equal.into(), line);
        self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
        return ValueType::Bool.intern();
      }
      TokenType::Greater => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::GreaterInt.into(), line);
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::GreaterUint.into(), line);
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::GreaterFloat.into(), line);
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers.").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      TokenType::GreaterEqual => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::LessInt.into(), line);
            self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::LessUint.into(), line);
            self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::LessFloat.into(), line);
            self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers.").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      TokenType::Less => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::LessInt.into(), line);
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::LessUint.into(), line);
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::LessFloat.into(), line);
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers.").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      TokenType::LessEqual => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            self.function_compiler.func.chunk.write(Opcode::GreaterInt.into(), line);
            self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            self.function_compiler.func.chunk.write(Opcode::GreaterUint.into(), line);
            self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
          }
          (ValueType::Float, ValueType::Float) => {
            self.function_compiler.func.chunk.write(Opcode::GreaterFloat.into(), line);
            self.function_compiler.func.chunk.write(Opcode::Not.into(), line);
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers.").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      _ => unreachable!(),
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn ternary(
    &mut self,
    c: Expression,
    t: Expression,
    f: Expression,
    _line: usize
  ) -> UValueType {
    let line = 0;
    let c_type = self.visit_expression(c, false);
    if c_type != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Condition must be a boolean, got {c_type:?}").as_str());
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
    let _f_type = self.visit_expression(f, false);
    // patch the jump to the end
    self.patch_jump(jump_to_end);

    t_type
  }

  #[instrument(level = "trace", skip_all)]
  pub fn variable(&mut self, tok: SharedString, assignment_target: bool, _line: usize) -> UValueType {
    let line = 0;

    let (arg, level) = self.resolve_local(0, &tok);
    if let Some(i) = arg {
      let Some((local_compiler, frame_offset)) = Self::compiler_at(
        level,
        &mut self.function_compiler
      ) else {
        error!(self, format!("[line {line}] Undefined variable '{}'.", tok).as_str());
        return ValueType::Err.intern();
      };

      if assignment_target {
        // if !assignable {
        //     error!(self, "Can't assign to const variable");
        // }
        local_compiler.func.chunk.write_local_ptr(i - frame_offset, line);

        return ValueType::Pointer(
          self.function_compiler.locals[0][&i].local_type,
          false
        ).intern();
      } else {
        
        local_compiler.func.chunk.write_pool_opcode(
          Opcode::GetLocal,
          (i as i32) - (frame_offset as i32),
          line
        );
        let t = local_compiler.locals[0].get(&i).expect("local").local_type;
        local_compiler.func.chunk.write(t.num_words() as u8, line);
        return t;
      }
    } else if let Some(i) = self.gloabls.get(&tok) {
      if assignment_target {
        self.function_compiler.func.chunk.write_constant(Value::Pointer(Pointer::Static(*i)), line);
        return ValueType::Pointer(
          *self.global_types.get(i).unwrap(),
          false
        ).intern();
      } else {
        self.function_compiler.func.chunk.write_pool_opcode(Opcode::GetGlobal, *i as i32, line);
        let t = self.global_types.get(i).unwrap();
        self.function_compiler.func.chunk.write(t.num_words() as u8, line);
        return *t;
      }
    } else {
      error!(self, format!("[line {line}] Undefined variable '{}'.", tok).as_str());
      ValueType::Err.intern()
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn assign(&mut self, l: Expression, r: Expression, _line: usize) -> UValueType {
    let line = 0;
    let _t = self.visit_expression(l, true);

    let t2 = self.visit_expression(r, false);

    self.function_compiler.func.chunk.write_pool_opcode(
      Opcode::DerefAssign,
      t2.num_words() as i32,
      line
    );
    t2
  }

  #[instrument(level = "trace", skip_all)]
  pub fn logical(
    &mut self,
    l: Expression,
    operator: TokenType,
    r: Expression,
    _line: usize
  ) -> UValueType {
    let line = 0;
    let operator_type = operator;
    let t = self.visit_expression(l, false);
    if t != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
    }
    if operator_type == TokenType::Or {
      let jump_to_end = self.emit_jump(Opcode::JumpIfTrue.into());
      self.function_compiler.func.chunk.write_pop(t, line);
      let t2 = self.visit_expression(r, false);
      if t2 != ValueType::Bool.intern() {
        error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
      }
      self.patch_jump(jump_to_end);
      return ValueType::Bool.intern();
    } else {
      let jump_to_else = self.emit_jump(Opcode::JumpIfFalse.into());
      self.function_compiler.func.chunk.write_pop(t, line);
      let t2 = self.visit_expression(r, false);
      if t2 != ValueType::Bool.intern() {
        error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
      }
      self.patch_jump(jump_to_else);
      return ValueType::Bool.intern();
    }
  }

  

  #[instrument(level = "trace", skip_all)]
  pub fn call(&mut self, callee: Expression, args: Vec<Expression>, _line: usize) -> UValueType {
    let line = 0;
    let t = self.visit_expression(callee, false);

    let mut ts = Vec::new();
    for arg in args.clone() {
      ts.push(self.visit_expression(arg, false));
    }

    if let ValueType::Closure(f, upvals) = t.as_ref() {
      self.function_compiler.func.chunk.write_pool_opcode(
        Opcode::Call,
        ts.iter().fold(0, |a, b| a + (b.num_words() as i32)),
        line
      );
      self.function_compiler.func.chunk.write(*upvals as u8, line);

      let return_type = f.last().unwrap();
      return *return_type;
    } else {
      error!(
        self,
        format!("[line {line}] Can only call functions and closures, got {t:?}").as_str()
      );
      return ValueType::Err.intern();
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn dot(&mut self, e: Expression, tok: Token, at: bool, line: usize) -> UValueType {
    let t = self.visit_expression(e, true);

    let decayed = t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(maybe_s, _) = decayed.as_ref() else {
      error!(
        self,
        format!("[line {line}] Can only access fields of struct types, got {t:?}").as_str()
      );
      return ValueType::Err.intern();
    };

    let ValueType::Struct(s) = maybe_s.as_ref() else {
      error!(
        self,
        format!("[line {line}] Can only access fields of struct types, got {t:?}").as_str()
      );
      return ValueType::Err.intern();
    };

    let l = s.fields.read();
    let temp = l.as_ref().unwrap();
    let Some(field) = temp.get(&tok.lexeme) else {
      error!(self, format!("[line {line}] Struct {} has no field {}", s.name, tok.lexeme).as_str());
      return ValueType::Err.intern();
    };

    if !at {
      self.function_compiler.func.chunk.write_constant(Value::Integer(field.offset as i64), line);
      self.function_compiler.func.chunk.write(Opcode::PointerAdd.into(), line);
      self.function_compiler.func.chunk.write(1, line);
      self.function_compiler.func.chunk.write(Opcode::DerefGet.into(), line);
      self.function_compiler.func.chunk.write(field.value.num_words() as u8, line);
      return field.value;
    } else {
      self.function_compiler.func.chunk.write_constant(Value::Integer(field.offset as i64), line);
      self.function_compiler.func.chunk.write(Opcode::PointerAdd.into(), line);
      self.function_compiler.func.chunk.write(1, line);
      return ValueType::Pointer(field.value, false).intern();
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn function(&mut self, function_expr: FunctionExpression, _line: usize) -> UValueType {
    let mut capture_size = 0;
    let mut captures = Vec::new();
    for cap in &function_expr.captures {
      let t = self.variable(
        cap.clone(),
        false,
        0
      );
      captures.push((cap.clone(), t));
      capture_size += t.num_words();
    }

    let c = FunctionCompiler::new(
      super::FunctionType::Function,
      Some(std::mem::take(&mut self.function_compiler)),
      &function_expr.name.clone(),
      function_expr.locals.clone(),

    );
    self.function_compiler = Box::new(c);
    self.function_compiler.scope_depth += 1;
    self.function_compiler.func.arity = function_expr.params.len();

    let return_type = function_expr.return_type;

    self.function_compiler.func.return_type = return_type;
    self.function_compiler.func.return_size = return_type.num_words();

    for (_name, t) in captures {
      self.function_compiler.local_count += t.num_words();
    }
    for (_name, t, _) in &function_expr.params {
      self.function_compiler.local_count += t.num_words();
    }

    let mut param_types = function_expr.params
      .iter()
      .map(|(_, t, _)| *t)
      .collect::<Vec<_>>();
    param_types.push(return_type);

    let _func_name = function_expr.name.clone();

    let ft = ValueType::Closure(param_types.clone().as_slice().into(), capture_size).intern();

    // self.begin_scope(function_expr.locals.clone());

    for d in &function_expr.body {
      self.visit(d.clone());
    }

    if self.function_compiler.func.return_type == ValueType::Nil.intern() {
      self.emit_return(return_type);
    }
    // self.end_scope();
    *param_types.last_mut().unwrap() = self.function_compiler.func.return_type;
    let c = std::mem::take(&mut self.function_compiler);
    let name = c.func.name.clone();
    // if self.had_error.get() {
    c.func.chunk.disassemble(&name, Level::DEBUG);
    // }

    let (rfunc, maybe_enclosing) = c.recover_values();
    let Some(enclosing) = maybe_enclosing else {
      error!(self, format!("Function {} has no enclosing function", name).as_str());
      return ValueType::Err.intern();
    };
    let func = Rc::new(rfunc);

    self.function_compiler = enclosing;
    self.function_segment.push(Runnable::StackVMFunction(func.clone()));
    self.num_functions += 1;
    let f_id = self.num_functions - 1;
    self.function_compiler.func.chunk.write_constant(Value::Pointer(Pointer::Function(f_id)), 0);

    // self.function_compiler.func.chunk.write_pool_opcode(Opcode::Closure, f_id as i32, 0);
    // self.function_compiler.func.chunk.write(capture_size as u8, 0);

    ft
  }

  #[instrument(level = "trace", skip_all)]
  pub fn cast(
    &mut self,
    value: Expression,
    mcast_type: Option<UValueType>,
    _from_type: UValueType,
    _line: usize
  ) -> UValueType {
    let line = 0;

    let t = self.visit_expression(value, false);
    // let t = from_type;

    let cast_type = match mcast_type {
      Some(t) => t,
      None => {
        if let ValueType::Pointer(ms, b) = t.decay(self.custom_structs.clone().into()).as_ref() {
          if let ValueType::Struct(s) = ms.decay(self.custom_structs.clone().into()).as_ref() {
            if s.embed.is_some() {
              return ValueType::Pointer(self.custom_structs
                .get(s.embed.as_ref().unwrap())
                .map(|s| ValueType::Struct(s.clone()).intern())
                .unwrap_or(ValueType::Err.intern()), *b).intern()}
          }
        }
        t
      }
    };

    match (t.decay(self.custom_structs.clone().into()).as_ref(), cast_type.as_ref()) {
      (x, y) if x.intern() == y.intern() => x.intern(),
      (ValueType::Integer, ValueType::Float) => {
        self.function_compiler.func.chunk.write(Opcode::CastIntToFloat.into(), line);
        ValueType::Float.intern()
      }
      (ValueType::Float, ValueType::Integer) => {
        self.function_compiler.func.chunk.write(Opcode::CastFloatToInt.into(), line);
        ValueType::Integer.intern()
      }
      (ValueType::Integer, ValueType::Bool) => {
        self.function_compiler.func.chunk.write(Opcode::CastIntToBool.into(), line);
        ValueType::Bool.intern()
      }
      (ValueType::Bool, ValueType::Integer) => {
        self.function_compiler.func.chunk.write(Opcode::CastBoolToInt.into(), line);
        ValueType::Integer.intern()
      }
      (ValueType::Bool, ValueType::Float) => {
        self.function_compiler.func.chunk.write(Opcode::CastBoolToFloat.into(), line);
        ValueType::Float.intern()
      }
      (ValueType::Integer, ValueType::SharedString(0)) => {
        self.function_compiler.func.chunk.write(Opcode::CastIntToSharedString.into(), line);
        ValueType::SharedString(0).intern()
      }
      (ValueType::Float, ValueType::SharedString(0)) => {
        self.function_compiler.func.chunk.write(Opcode::CastFloatToSharedString.into(), line);
        ValueType::SharedString(0).intern()
      }
      (ValueType::Bool, ValueType::SharedString(0)) => {
        self.function_compiler.func.chunk.write(Opcode::CastBoolToSharedString.into(), line);
        ValueType::SharedString(0).intern()
      }
      (ValueType::Array(x, _), ValueType::Pointer(y, _)) if x == y => {
        ValueType::Pointer(*x, false).intern()
      }
      (ValueType::UInteger, ValueType::Integer) => {
        self.function_compiler.func.chunk.write(Opcode::CastUintToInt.into(), line);
        ValueType::Integer.intern()
      }
      (ValueType::UInteger, ValueType::Float) => {
        self.function_compiler.func.chunk.write(Opcode::CastUintToFloat.into(), line);
        ValueType::Float.intern()
      }
      (ValueType::Integer, ValueType::UInteger) => {
        self.function_compiler.func.chunk.write(Opcode::CastIntToUint.into(), line);
        ValueType::UInteger.intern()
      }
      (ValueType::UInteger, ValueType::Bool) => {
        self.function_compiler.func.chunk.write(Opcode::CastUintToBool.into(), line);
        ValueType::Bool.intern()
      }
      (ValueType::Bool, ValueType::UInteger) => {
        self.function_compiler.func.chunk.write(Opcode::CastBoolToUint.into(), line);
        ValueType::UInteger.intern()
      }

      (ValueType::Pointer(_, _), ValueType::Pointer(_, _)) => cast_type,

      _ => {
        error!(self, &format!("[line {line}] Can't cast {t:?} to {cast_type:?}").as_str());
        ValueType::Err.intern()
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn array_literal(&mut self, elements: Vec<Expression>, line: usize) -> UValueType {
    let _line = 0;
    let mut element_types = Vec::new();
    for e in elements {
      let t = self.visit_expression(e, false);
      element_types.push(t);
    }
    let array_type = element_types[0];
    for t in &element_types {
      if *t != array_type {
        error!(
          self,
          format!(
            "[line {line}] All elements of an array must be of the same type, got {t:?} and {array_type:?}"
          ).as_str()
        );
      }
    }

    ValueType::Array(array_type, element_types.len()).intern()
  }

  pub fn struct_literal(
    &mut self,
    t: CustomStruct,
    fields: HashMap<SharedString, Expression>,
    line: usize
  ) -> UValueType {
    let l = t.fields.read();
    let temp = l.as_ref().unwrap();
    let mut orig_fields = temp.iter().collect::<Vec<_>>();
    orig_fields.sort_by(|a, b| a.1.offset.cmp(&b.1.offset));
    for (name, entry) in &orig_fields {
      let Some(exp) = fields.get(*name) else {
        for _ in 0..entry.value.num_words() {
          self.function_compiler.func.chunk.write(Opcode::Nil.into(), 0);
        }
        continue;
      };
      let ft = self.visit_expression(exp.clone(), false);
      if ft != entry.value {
        error!(
          self,
          format!(
            "[line {line}] Field {} has type {:?}, expected {:?}",
            name,
            ft,
            entry.value
          ).as_str()
        );
        return ValueType::Err.intern();
      }
    }
    drop(l);
    ValueType::Struct(t).intern()
  }

  fn emit_return(&mut self, t: UValueType) {
    for _ in 0..t.num_words() {
      self.function_compiler.func.chunk.write(Opcode::Nil.into(), 0);
    }
    self.function_compiler.func.chunk.write_pool_opcode(Opcode::Return, 1, 0);
  }

  fn compiler_at(
    level: u32,
    compiler: &mut FunctionCompiler
  ) -> Option<(&mut FunctionCompiler, usize)> {
    if level == 0 {
      Some((compiler, 0))
    } else {
      return compiler.enclosing.as_mut().and_then(|e| {
        let locals = e.local_count;
        Self::compiler_at(level - 1, e).map(|(c, i)| { (c, i + locals) })
      });
    }
  }

  pub fn resolve_local(&mut self, compiler_level: u32, token: &str) -> (Option<usize>, u32) {
    let Some((compiler, _)) = Self::compiler_at(compiler_level, &mut self.function_compiler) else {
      return (None, compiler_level);
    };
    if compiler.locals.is_empty() {
      return (None, compiler_level);
    }
    for (k, local) in &compiler.locals[0] {
      if token == local.name {
        if local.depth == -1 {
          error!(self, "Cannot read local variable in its own initializer.");
          return (None, compiler_level);
        }
        return (Some(*k), compiler_level);
      }
    }
    self.resolve_local(compiler_level + 1, token)
  }
}
