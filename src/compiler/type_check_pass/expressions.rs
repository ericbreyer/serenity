use std::{ cell::Cell, collections::HashMap };

use tracing::{ debug, instrument };

use crate::{
  common::ast::{ Expression, FunctionExpression },
  lexer::{ Token, TokenType },
  typing::{ CustomStruct, UValueType, ValueType },
  value::Value,
};

use super::{ FunctionCompiler, Local, TypeCheckWalker, Upvalue };

use crate::error;

impl TypeCheckWalker {
  #[instrument(level = "trace", skip_all)]
  pub fn literal(&mut self, v: Value, line: usize) -> UValueType {
    match v {
      Value::Bool(false) => { ValueType::Bool.intern() }
      Value::Nil => { ValueType::Nil.intern() }
      Value::Bool(true) => { ValueType::Bool.intern() }
      Value::Integer(_i) => { ValueType::Integer.intern() }
      Value::Float(_f) => { ValueType::Float.intern() }
      Value::Char(_c) => { ValueType::Char.intern() }
      Value::UInteger(_) => { ValueType::UInteger.intern() }
      Value::Pointer(_) | Value::Closure(_) => {
        error!(self, format!("[line {}] Cannot have pointer or closure literals", line).as_str());
        ValueType::Err.intern()
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn string(&mut self, _s: String, _line: usize) -> UValueType {
    return ValueType::String.intern();
  }

  #[instrument(level = "trace", skip_all)]
  pub fn unary(&mut self, token_t: TokenType, e: Expression, line:usize) -> UValueType {
    let operator_type = token_t;
    // Compile the operand.
    let t = self.visit_expression(e, false);

    // Emit the operator instruction.
    match operator_type {
      TokenType::Minus =>
        match t.as_ref() {
          ValueType::Integer | ValueType::Float => {}
          _ => {
            error!(self, format!("[line {}] Operand must be a number.", line).as_str());
          }
        }
      TokenType::Bang =>
        match t.as_ref() {
          ValueType::Bool => {}
          _ => {
            error!(self, format!("[line {}] Operand must be a boolean.", line).as_str());
          }
        }
      _ => unreachable!(),
    }
    t
  }

  #[instrument(level = "trace", skip_all)]
  pub fn deref(&mut self, e: Expression, at: bool, line: usize) -> UValueType {
    // Compile the operand.
    let raw_t = self.visit_expression(e, false);
    let (t, _is_const) = match raw_t.decay(self.custom_structs.clone().into()).as_ref() {
      &ValueType::Pointer(pointee_type, is_const) => (pointee_type, is_const),
      _ => {
        error!(self, format!("[line {line}] Expected pointer type, got {:?}", *raw_t).as_str());
        (ValueType::Err.intern(), false)
      }
    };

    if !at {
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
        error!(self, format!("[line {line}] Expected pointer type, got {:?}", *t).as_str());
        ValueType::Err.intern()
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn index(&mut self, e: Expression, i: Expression, at: bool, line: usize) -> UValueType {
    let _line = 0;

    // Compile the operand.
    let raw_t = self.visit_expression(e, false);
    let pointer_type = raw_t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(pointee_type, _) = pointer_type.as_ref() else {
      error!(self, format!("[line {line}] Expected pointer type, got {:?}", *raw_t).as_str());
      return ValueType::Err.intern();
    };

    let i_type = self.visit_expression(i, false);
    if i_type != ValueType::Integer.intern() {
      error!(self, format!("[line {line}] Index must be an integer.").as_str());
    }

    if !at {
      *pointee_type
    } else {
      raw_t
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn binary(&mut self, l: Expression, operator: TokenType, r: Expression, line: usize) -> UValueType {
    let operator_type = operator;
    let _line = 0;

    // Compile the left operand.
    let lt = self.visit_expression(l, false).decay(Some(self.custom_structs.clone()));

    // Compile the right operand.
    let rt = self.visit_expression(r, false);

    // Emit the operator instruction.
    match operator_type {
      TokenType::Plus =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            return ValueType::Float.intern();
          }
          (ValueType::Pointer(_, _), ValueType::Integer) => {
            return lt;
          }
          (ValueType::Pointer(_, _), ValueType::UInteger) => {
            return lt;
          }
          _ => {
            error!(
              self,
              format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer got {lt:?} and {rt:?}"
              ).as_str()
            );
            return ValueType::Err.intern();
          }
        }
      TokenType::Minus =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            return ValueType::Float.intern();
          }
          (ValueType::Pointer(_, _), ValueType::Integer) => {
            return lt;
          }
          (ValueType::Pointer(_, _), ValueType::UInteger) => {
            return lt;
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers or a pointer and an integer").as_str());
            return ValueType::Err.intern();
          }
        }
      TokenType::Star =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            return ValueType::Float.intern();
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            return ValueType::Err.intern();
          }
        }
      TokenType::Slash =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {
            return ValueType::Integer.intern();
          }
          (ValueType::UInteger, ValueType::UInteger) => {
            return ValueType::UInteger.intern();
          }
          (ValueType::Float, ValueType::Float) => {
            return ValueType::Float.intern();
          }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
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

          return ValueType::Bool.intern();
        }

        return ValueType::Bool.intern();
      }
      TokenType::BangEqual => {
        if rt != lt {
          self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always true, please cast to the same type"
            )
          );

          return ValueType::Bool.intern();
        }

        return ValueType::Bool.intern();
      }
      TokenType::Greater => {
        match (lt.as_ref(), rt.as_ref()) {
          | (ValueType::Integer, ValueType::Integer)
          | (ValueType::Float, ValueType::Float)
          | (ValueType::UInteger, ValueType::UInteger) => {}
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      TokenType::GreaterEqual => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {}
          (ValueType::Float, ValueType::Float) => {}
          (ValueType::UInteger, ValueType::UInteger) => {}
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      TokenType::Less => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {}
          (ValueType::Float, ValueType::Float) => {}
          (ValueType::UInteger, ValueType::UInteger) => {}
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      TokenType::LessEqual => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => {}
          (ValueType::Float, ValueType::Float) => {}
          (ValueType::UInteger, ValueType::UInteger) => {}
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            return ValueType::Err.intern();
          }
        }
        return ValueType::Bool.intern();
      }
      _ => unreachable!(),
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn ternary(&mut self, c: Expression, t: Expression, f: Expression, line: usize) -> UValueType {
    let c_type = self.visit_expression(c, false);
    if c_type != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Condition must be a boolean.").as_str());
    }

    // compile the then branch
    let t_type = self.visit_expression(t, false);

    // compile the else branch
    let f_type = self.visit_expression(f, false);

    if t_type != f_type {
      error!(self, format!("[line {line}] Branches must have the same type.").as_str());
    }
    t_type
  }

  #[instrument(level = "trace", skip_all)]
  pub fn variable(&mut self, tok: Token, assignment_target: bool, line: usize) -> UValueType {

    debug!(?tok, ?assignment_target, "variable");

    let (arg, assignable) = self.resolve_local(0, &tok);
    if let Some(i) = arg {
      if assignment_target {
        // if !assignable {
        //     error!(self, "Can't assign to const variable");
        // }

        return ValueType::Pointer(
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

        return ValueType::Pointer(
          self.function_compiler.locals[&i].local_type,
          assignable
        ).intern();
      } else {
        let t = self.function_compiler.locals.get(&i).expect("local").local_type;

        return t;
      }
    } else if let Some(i) = self.gloabls.get(&tok.lexeme) {
      if assignment_target {
        return ValueType::Pointer(
          self.global_types.get(i).unwrap().0,
          self.global_types.get(i).unwrap().1
        ).intern();
      } else {
        let t = self.global_types.get(i).unwrap().0;

        return t;
      }
    } else {
      error!(self, format!("[line {line}] Undefined variable '{}'.", tok.lexeme).as_str());
      ValueType::Err.intern()
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn assign(&mut self, l: Expression, r: Expression, line: usize) -> UValueType {
    let t = self.visit_expression(l, true);

    let t2 = self.visit_expression(r, false);

    let decayed = t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(s, assignable) = decayed.as_ref() else {
      error!(self, format!("[line {line}] Can only assign to a pointer type, got {t:?}").as_str());
      return ValueType::Err.intern();
    };

    if !assignable {
      error!(self, format!("[line {line}] Can't assign to const variable").as_str());
    }

    if t2 != *s {
      error!(self, format!("[line {line}] Cannot assign {t2:?} to variable of type {s:?}").as_str());
    }

    t2
  }

  #[instrument(level = "trace", skip_all)]
  pub fn logical(&mut self, l: Expression, operator: TokenType, r: Expression, line: usize) -> UValueType {
    let _line = 0;
    let operator_type = operator;
    let t = self.visit_expression(l, false);
    if t != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
    }
    if operator_type == TokenType::Or {
      let t2 = self.visit_expression(r, false);
      if t2 != ValueType::Bool.intern() {
        error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
      }
      return ValueType::Bool.intern();
    } else {
      let t2 = self.visit_expression(r, false);
      if t2 != ValueType::Bool.intern() {
        error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
      }
      return ValueType::Bool.intern();
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn call(&mut self, callee: Expression, args: Vec<Expression>, line: usize) -> UValueType {
    let _line = 0;
    let t = self.visit_expression(callee, false);

    let mut ts = Vec::new();
    for arg in args.clone() {
      ts.push(self.visit_expression(arg, false));
    }

    if let ValueType::Closure(f) = t.as_ref() {
      if
        ts
          .iter()
          .map(|x| x.decay(None))
          .collect::<Vec<UValueType>>() != f[0..f.len() - 1]
      {
        error!(
          self,
          format!(
            "[line {line}] Argument types do not match function signature. Expected {:?}, got {:?}",
            f[0..f.len() - 1].to_vec(),
            ts
          ).as_str()
        );
      }

      let return_type = f.last().unwrap();
      return *return_type;
    } else {
      error!(self, format!("[line {line}] Can only call function types, got {t:?}").as_str());
      return ValueType::Err.intern();
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn dot(&mut self, e: Expression, tok: Token, at: bool, line: usize) -> UValueType {
    let _line = 0;
    let t = self.visit_expression(e, true);

    let decayed = t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(maybe_s, _) = decayed.as_ref() else {
      error!(self, format!("[line {line}] Can only access fields of struct types, got {t:?}").as_str());
      return ValueType::Err.intern();
    };

    let ValueType::Struct(s) = maybe_s.as_ref() else {
      error!(self, format!("[line {line}] Can only access fields of struct types, got {t:?}").as_str());
      return ValueType::Err.intern();
    };

    let l = s.fields.read();
    let temp = l.as_ref().unwrap();
    let Some(field) = temp.get(&tok.lexeme) else {
      error!(self, format!("[line {line}] Field '{}' not found have fields {:?}", tok.lexeme, s.fields).as_str());
      return ValueType::Err.intern();
    };

    if !at {
      return field.value;
    } else {
      return ValueType::Pointer(field.value, true).intern();
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn function(&mut self, function_expr: FunctionExpression, line: usize) -> UValueType {
    let c = FunctionCompiler::new(
      Some(std::mem::take(&mut self.function_compiler)),
      &function_expr.name.clone()
    );
    self.function_compiler = Box::new(c);
    self.function_compiler.scope_depth += 1;

    for param in &function_expr.params {
      let mutable = param.2;

      let p_type = param.1;

      self.parse_variable(param.0.clone(), mutable);
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = p_type;
      self.function_compiler.locals.get_mut(&last_local).unwrap().mutable = mutable;
      self.function_compiler.locals.get_mut(&last_local).unwrap().assigned = true;

      self.define_variable(0, p_type);
    }

    let return_type = function_expr.return_type;
    self.function_compiler.return_type = return_type;

    let mut param_types = function_expr.params
      .iter()
      .map(|(_, t, _)| *t)
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
      local_type: ValueType::Closure(ft.into()).intern(),
    });
    // self.compiler.local_count += ft.num_words() as usize;
    let mut does_return = false;
    for d in &function_expr.body {
      let (_, r) = self.visit(d.clone());
      does_return = does_return || r;
    }

    if !does_return && self.function_compiler.return_type != ValueType::Nil.intern() {
      error!(
        self,
        format!(
          "[line {line}] Function \"{}\" must return a value in all code paths",
          function_expr.name
        ).as_str()
      );
    }

    *param_types.last_mut().unwrap() = self.function_compiler.return_type;
    let c = std::mem::take(&mut self.function_compiler);

    let (maybe_enclosing, _upvalues) = c.recover_values();
    let Some(enclosing) = maybe_enclosing else {
      error!(self, "[line {line}] Cannot return from top-level code.");
      return ValueType::Err.intern();
    };

    self.function_compiler = enclosing;

    // if any upvalues are non-const we can not escape

    return ValueType::Closure(ft.into()).intern();
  }

  #[instrument(level = "trace", skip_all)]
  pub fn cast(
    &mut self,
    value: Expression,
    cast_type: UValueType,
    from_type: &Cell<UValueType>,
    line: usize
  ) -> UValueType {

    let t = self.visit_expression(value, false);
    from_type.set(t);

    match (t.decay(self.custom_structs.clone().into()).as_ref(), cast_type.as_ref()) {
      (x, y) if x == y => x.intern(),
      (ValueType::Integer, ValueType::Float) => { ValueType::Float.intern() }
      (ValueType::Float, ValueType::Integer) => { ValueType::Integer.intern() }
      (ValueType::Integer, ValueType::Bool) => { ValueType::Bool.intern() }
      (ValueType::Bool, ValueType::Integer) => { ValueType::Integer.intern() }
      (ValueType::Bool, ValueType::Float) => { ValueType::Float.intern() }
      (ValueType::Integer, ValueType::String) => { ValueType::String.intern() }
      (ValueType::Float, ValueType::String) => { ValueType::String.intern() }
      (ValueType::Bool, ValueType::String) => { ValueType::String.intern() }
      (ValueType::UInteger, ValueType::Integer) => { ValueType::Integer.intern() }
      (ValueType::UInteger, ValueType::Float) => { ValueType::Float.intern() }
      (ValueType::Integer, ValueType::UInteger) => { ValueType::UInteger.intern() }
      (ValueType::UInteger, ValueType::Bool) => { ValueType::Bool.intern() }
      (ValueType::UInteger, ValueType::String) => { ValueType::String.intern() }
      (ValueType::Bool, ValueType::UInteger) => { ValueType::UInteger.intern() }
      (ValueType::Array(x, _), ValueType::Pointer(y, _)) if x == y => {
        ValueType::Pointer(*x, false).intern()
      }
      (ValueType::Pointer(_, _), ValueType::Pointer(_, _)) => cast_type,
      _ => {
        error!(self, &format!("[line {line}] Cannot cast {t:?} to {cast_type:?}"));
        ValueType::Err.intern()
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn array_literal(&mut self, elements: Vec<Expression>, line:usize) -> UValueType {
    let _line = 0;
    let mut element_types = Vec::new();
    for e in elements {
      let t = self.visit_expression(e, false);
      element_types.push(t);
    }
    let array_type = element_types[0];
    for t in &element_types {
      if *t != array_type {
        error!(self, format!("[line {line}] All elements of an array must be of the same type.").as_str());
      }
    }

    ValueType::Array(array_type, element_types.len()).intern()
  }

  #[instrument(level = "trace", skip_all)]
  pub fn struct_literal(
    &mut self,
    t: CustomStruct,
    fields: HashMap<String, Expression>,
    line: usize
  ) -> UValueType {
    let l = t.fields.read();
    let temp = l.as_ref().unwrap();
    let mut orig_fields = temp.iter().collect::<Vec<_>>();
    orig_fields.sort_by(|a, b| a.1.offset.cmp(&b.1.offset));
    for (name, entry) in &orig_fields {
      let Some(exp) = fields.get(*name) else {
        for _ in 0..entry.value.num_words() {
        }
        continue;
      };
      let ft = self.visit_expression(exp.clone(), false);
      if ft != entry.value {
        error!(
          self,
          format!("[line {line}] Field {} has type {:?}, expected {:?}", name, ft, entry.value).as_str()
        );
        return ValueType::Err.intern();
      }
    }
    drop(l);
    ValueType::Struct(t).intern()
  }

  fn resolve_upvalue(&mut self, compiler_level: u32, token: &Token) -> Option<usize> {
    Self::compiler_at(compiler_level + 1, &mut self.function_compiler)?;

    if let (Some(local), _) = self.resolve_local(compiler_level + 1, token) {
      let Some(compiler) = Self::compiler_at(compiler_level + 1, &mut self.function_compiler) else {
        return None;
      };

      compiler.locals.get_mut(&local).expect("Local not found").captured = true;
      let t = compiler.locals.get(&local).expect("Local not found").local_type;
      let n = compiler.locals.get(&local).expect("Local not found").name.clone();
      let Some((_upval_place, local_place)) = Some(
        self.add_upvalue(local, compiler_level, t, n)
      ) else {
        return None;
      };
      return Some(local_place);
    }
    if let Some(upvalue) = self.resolve_upvalue(compiler_level + 1, token) {
      let Some(compiler) = Self::compiler_at(compiler_level + 1, &mut self.function_compiler) else {
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
      Some(compiler)
    } else {
      return compiler.enclosing.as_mut().and_then(|e| Self::compiler_at(level - 1, e));
    }
  }

  fn add_upvalue(
    &mut self,
    up_idx: usize,
    compiler_level: u32,
    upvalue_type: UValueType,
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
      return (0, 0);
    }

    if compiler.locals.contains_key(&compiler.local_count) {
      compiler.locals.insert(
        compiler.local_count + upvalue_type.num_words(),
        compiler.locals.get(&compiler.local_count).unwrap().clone()
      );
    }

    compiler.locals.insert(compiler.local_count, Local {
      name: name.clone(),
      depth: compiler.scope_depth,
      mutable: true,
      assigned: true,
      captured: true,
      local_type: upvalue_type,
    });

    compiler.upvalues.push(Upvalue {
      local_idx: up_idx,
      name: name.clone(),
    });
    compiler.local_count += upvalue_type.num_words();

    compiler.upvalue_count += upvalue_type.num_words();

    (
      compiler.upvalue_count - upvalue_type.num_words(),
      compiler.local_count - upvalue_type.num_words(),
    )
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
    (None, true)
  }
}
