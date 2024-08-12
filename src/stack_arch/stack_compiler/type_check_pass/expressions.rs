use std::collections::HashMap;

use tracing::{ debug, instrument };

use crate::{
  common::{ parsed_ast::{ Expression, FunctionExpression }, declared_ast },
  lexer::{ Token, TokenType },
  typing::{ CustomStruct, UValueType, ValueType },
  value::Value,
};

use super::{ FunctionCompiler, Local, TypeCheckWalker };

use crate::error;

impl TypeCheckWalker {
  #[instrument(level = "trace", skip_all)]
  pub fn literal(&mut self, v: Value, line: usize) -> (declared_ast::Expression, UValueType) {
    let t = match v {
      Value::Bool(false) => { ValueType::Bool.intern() }
      Value::Nil => { ValueType::Nil.intern() }
      Value::Bool(true) => { ValueType::Bool.intern() }
      Value::Integer(_i) => { ValueType::Integer.intern() }
      Value::Float(_f) => { ValueType::Float.intern() }
      Value::Char(_c) => { ValueType::Char.intern() }
      Value::UInteger(_) => { ValueType::UInteger.intern() }
      Value::Pointer(_) => {
        error!(self, format!("[line {}] Cannot have pointer or closure literals", line).as_str());
        ValueType::Err.intern()
      }

    };
    (declared_ast:Expression::Literal(LiteralExpression{ value: v, line), line_no:  t})
  }

  #[instrument(level = "trace", skip_all)]
  pub fn SharedString(&mut self, s: SharedString, line: usize) -> (declared_ast::Expression, UValueType) {
    (declared_ast::Expression::StringLiteral(s, line), ValueType::Pointer(ValueType::Char.intern(), false).intern())
  }

  #[instrument(level = "trace", skip_all)]
  pub fn unary(
    &mut self,
    token_t: TokenType,
    e: Expression,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let operator_type = token_t;
    // Compile the operand.
    let (nde, t) = self.visit_expression(e, false);

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
    (declared_ast::Expression::Unary(operator_type, Box::new(nde), line), t)
  }

  #[instrument(level = "trace", skip_all)]
  pub fn deref(
    &mut self,
    e: Expression,
    at: bool,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    // Compile the operand.
    let (nde, raw_t) = self.visit_expression(e.clone(), false);
    let (t, _is_const) = match raw_t.decay(self.custom_structs.clone().into()).as_ref() {
      &ValueType::Pointer(pointee_type, is_const) => (pointee_type, is_const),
      _ => {
        error!(self, format!("[line {line}] Expected pointer type, got {:?}", *raw_t).as_str());
        (ValueType::Err.intern(), false)
      }
    };


    if !at {
      (declared_ast::Expression::Deref(DerefExpression{ operand: Box::new(nde), line_no:  line}), t)
    } else {
      (declared_ast::Expression::Deref(DerefExpression{ operand: Box::new(nde), line_no:  line}), raw_t)
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn addr_of(&mut self, e: Expression, line: usize) -> (declared_ast::Expression, UValueType) {
    // Compile the operand.
    let (nde, t) = self.visit_expression(e, true);
    match t.as_ref() {
      ValueType::Pointer(_, _) => (declared_ast::Expression::Ref(Box::new(nde), line), t),
      _ => {
        error!(self, format!("[line {line}] Expected pointer type, got {:?}", *t).as_str());
        (nde, ValueType::Err.intern())
      }
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn index(
    &mut self,
    e: Expression,
    i: Expression,
    at: bool,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let _line = 0;

    // Compile the operand.
    let (nde, raw_t) = self.visit_expression(e, false);
    let pointer_type = raw_t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(pointee_type, _) = pointer_type.as_ref() else {
      error!(self, format!("[line {line}] Expected pointer type, got {:?}", *raw_t).as_str());
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    };

    let (ndi, i_type) = self.visit_expression(i, false);
    if i_type != ValueType::Integer.intern() {
      error!(self, format!("[line {line}] Index must be an integer.").as_str());
    }

    if !at {
      (declared_ast:Expression::Index(IndexExpression{ array: Box::new(nde), Box::new(ndi), index:  line), line_no:  *pointee_type})
    } else {
      (declared_ast:Expression::Index(IndexExpression{ array: Box::new(nde), Box::new(ndi), index:  line), line_no:  raw_t})
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn binary(
    &mut self,
    l: Expression,
    operator: TokenType,
    r: Expression,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let operator_type = operator;
    let _line = 0;

    // Compile the left operand.
    let (ndl, rlt) = self.visit_expression(l, false);
    let lt = rlt.decay(Some(self.custom_structs.clone()));

    // Compile the right operand.
    let (ndr, rt) = self.visit_expression(r, false);

    // Emit the operator instruction.
    let t = match operator_type {
      TokenType::Plus =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => { ValueType::Integer.intern() }
          (ValueType::UInteger, ValueType::UInteger) => { ValueType::UInteger.intern() }
          (ValueType::Float, ValueType::Float) => { ValueType::Float.intern() }
          (ValueType::Pointer(_, _), ValueType::Integer) => { lt }
          (ValueType::Pointer(_, _), ValueType::UInteger) => { lt }
          _ => {
            error!(
              self,
              format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer got {lt:?} and {rt:?}"
              ).as_str()
            );
            ValueType::Err.intern()
          }
        }
      TokenType::Minus =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => { ValueType::Integer.intern() }
          (ValueType::UInteger, ValueType::UInteger) => { ValueType::UInteger.intern() }
          (ValueType::Float, ValueType::Float) => { ValueType::Float.intern() }
          (ValueType::Pointer(_, _), ValueType::Integer) => { lt }
          (ValueType::Pointer(_, _), ValueType::UInteger) => { lt }
          _ => {
            error!(
              self,
              format!(
                "[line {line}] Operands must be two numbers or a pointer and an integer"
              ).as_str()
            );
            ValueType::Err.intern()
          }
        }
      TokenType::Star =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => { ValueType::Integer.intern() }
          (ValueType::UInteger, ValueType::UInteger) => { ValueType::UInteger.intern() }
          (ValueType::Float, ValueType::Float) => { ValueType::Float.intern() }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            ValueType::Err.intern()
          }
        }
      TokenType::Slash =>
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => { ValueType::Integer.intern() }
          (ValueType::UInteger, ValueType::UInteger) => { ValueType::UInteger.intern() }
          (ValueType::Float, ValueType::Float) => { ValueType::Float.intern() }
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            ValueType::Err.intern()
          }
        }
      TokenType::EqualEqual => {
        if rt != lt {
          self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always false, please cast to the same type"
            )
          );

          ValueType::Bool.intern();
        }

        ValueType::Bool.intern()
      }
      TokenType::BangEqual => {
        if rt != lt {
          self.warn(
            &format!(
              "[line {line}] Comparison of different types ({lt:?} and {rt:?}) is always true, please cast to the same type"
            )
          );

          ValueType::Bool.intern();
        }

        ValueType::Bool.intern()
      }
      TokenType::Greater => {
        match (lt.as_ref(), rt.as_ref()) {
          | (ValueType::Integer, ValueType::Integer)
          | (ValueType::Float, ValueType::Float)
          | (ValueType::UInteger, ValueType::UInteger) => ValueType::Bool.intern(),
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            ValueType::Err.intern()
          }
        }
      }
      TokenType::GreaterEqual => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => ValueType::Bool.intern(),
          (ValueType::Float, ValueType::Float) => ValueType::Bool.intern(),
          (ValueType::UInteger, ValueType::UInteger) => ValueType::Bool.intern(),
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            ValueType::Err.intern()
          }
        }
      }
      TokenType::Less => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => ValueType::Bool.intern(),
          (ValueType::Float, ValueType::Float) => ValueType::Bool.intern(),
          (ValueType::UInteger, ValueType::UInteger) => ValueType::Bool.intern(),
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            ValueType::Err.intern()
          }
        }
      }
      TokenType::LessEqual => {
        match (lt.as_ref(), rt.as_ref()) {
          (ValueType::Integer, ValueType::Integer) => ValueType::Bool.intern(),
          (ValueType::Float, ValueType::Float) => ValueType::Bool.intern(),
          (ValueType::UInteger, ValueType::UInteger) => ValueType::Bool.intern(),
          _ => {
            error!(self, format!("[line {line}] Operands must be two numbers").as_str());
            ValueType::Err.intern()
          }
        }
      }
      _ => unreachable!(),
    };

    (declared_ast:Expression::Binary(BinaryExpression{ left: Box::new(ndl), operator_type, operator:  Box::new(ndr), right:  line), line_no:  t})
  }

  #[instrument(level = "trace", skip_all)]
  pub fn ternary(
    &mut self,
    c: Expression,
    t: Expression,
    f: Expression,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let (ndc, c_type) = self.visit_expression(c, false);
    if c_type != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Condition must be a boolean.").as_str());
    }

    // compile the then branch
    let (ndt, t_type) = self.visit_expression(t, false);

    // compile the else branch
    let (ndf, f_type) = self.visit_expression(f, false);

    if t_type != f_type {
      error!(self, format!("[line {line}] Branches must have the same type.").as_str());
    }
    (declared_ast:Expression::Ternary(TernaryExpression{ condition: Box::new(ndc), Box::new(ndt), then_branch:  Box::new(ndf), else_branch:  line), line_no:  t_type})
  }

  #[instrument(level = "trace", skip_all)]
  pub fn variable(
    &mut self,
    tok: Token,
    assignment_target: bool,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    debug!(?tok, ?assignment_target, "variable");

    let ndv = declared_ast::Expression::Variable(tok.clone(), line);

    let (arg, assignable, compiler_level) = self.resolve_local(0, &tok);
    if let Some(i) = arg {
      let local_compiler = Self::compiler_at(compiler_level, &mut self.function_compiler).unwrap();
      if assignment_target {
        return (ndv, ValueType::Pointer(local_compiler.locals[&i].local_type, assignable).intern());
      } else {
        let t = local_compiler.locals.get(&i).expect("local").local_type;

        return (ndv, t);
      }
    } else if let Some(i) = self.gloabls.get(&tok.lexeme) {
      if assignment_target {
        return (
          ndv,
          ValueType::Pointer(
            self.global_types.get(i).unwrap().0,
            self.global_types.get(i).unwrap().1
          ).intern(),
        );
      } else {
        let t = self.global_types.get(i).unwrap().0;

        return (ndv, t);
      }
    } else {
      error!(self, format!("[line {line}] Undefined variable '{}'.", tok.lexeme).as_str());
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn assign(
    &mut self,
    l: Expression,
    r: Expression,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let (ndl, t) = self.visit_expression(l, true);

    let (ndr, t2) = self.visit_expression(r, false);

    let decayed = t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(s, assignable) = decayed.as_ref() else {
      error!(self, format!("[line {line}] Can only assign to a pointer type, got {t:?}").as_str());
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    };

    if !assignable {
      error!(self, format!("[line {line}] Can't assign to const variable").as_str());
    }

    if t2 != *s {
      error!(
        self,
        format!("[line {line}] Cannot assign {t2:?} to variable of type {s:?}").as_str()
      );
    }

    (declared_ast:Expression::Assign(AssignExpression{ variable: Box::new(ndl), Box::new(ndr), value:  line), line_no:  t2})
  }

  #[instrument(level = "trace", skip_all)]
  pub fn logical(
    &mut self,
    l: Expression,
    operator: TokenType,
    r: Expression,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let _line = 0;
    let operator_type = operator;
    let (ndl, t) = self.visit_expression(l, false);
    if t != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
    }
    let (ndr, t2) = self.visit_expression(r, false);
    if t2 != ValueType::Bool.intern() {
      error!(self, format!("[line {line}] Operand must be a boolean.").as_str());
    }
    return (
      declared_ast:Expression::Logical(LogicalExpression{ left: Box::new(ndl), operator:  operator_type, right:  Box::new(ndr), line_no:  line}),
      ValueType::Bool.intern(),
    );
  }

  #[instrument(level = "trace", skip_all)]
  pub fn call(&mut self, callee: Expression, args: Vec<Expression>, line: usize) -> (declared_ast::Expression, UValueType) {
    let _line = 0;
    let (ndcallee, t) = self.visit_expression(callee, false);

    let mut ts = Vec::new();
    let mut ndargs = Vec::new();
    for arg in args.clone() {
      let (ndarg, t) = self.visit_expression(arg, false);
      ts.push(t);
      ndargs.push(ndarg);
    }

    if let ValueType::Closure(f, _) = t.as_ref() {
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
      return (
        declared_ast:Expression::Call(CallExpression{ callee: Box::new(ndcallee), arguments:  ndargs, line_no:  line}),
        *return_type,
      );
    } else {
      error!(self, format!("[line {line}] Can only call function types, got {t:?}").as_str());
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn dot(&mut self, e: Expression, tok: Token, at: bool, line: usize) -> (declared_ast::Expression, UValueType) {
    let _line = 0;
    let (nde, t) = self.visit_expression(e, true);

    let decayed = t.decay(self.custom_structs.clone().into());
    let ValueType::Pointer(maybe_s, _) = decayed.as_ref() else {
      error!(
        self,
        format!("[line {line}] Can only access fields of struct types, got {t:?}").as_str()
      );
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    };

    let ValueType::Struct(s) = maybe_s.as_ref() else {
      error!(
        self,
        format!("[line {line}] Can only access fields of struct types, got {t:?}").as_str()
      );
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    };

    let m_name = format!("{}_{}",maybe_s.unique_string(),tok.lexeme);
    if s.methods.read().unwrap().contains(&m_name) {
      let ndc = declared_ast::Expression::Call(
        Box::new(declared_ast::Expression::Variable(Token{token_type:TokenType::Identifier,lexeme: m_name.clone(),line:line},line)),
        vec![declared_ast::Expression::Ref(Box::new(nde), line)],
        line,
      );
      let method_id = self.gloabls.get(&m_name).unwrap();
      let method_type = self.global_types.get(method_id).unwrap().0;
      let ValueType::Closure(f, _) = method_type.as_ref() else {
        error!(
          self,
          format!(
            "[line {line}] Expected method type, got {method_type:?}"
          ).as_str()
        );
        return (declared_ast::Expression::Empty, ValueType::Err.intern());
      };
      return (ndc, *f.last().unwrap());
    }

    let l = s.fields.read();
    let temp = l.as_ref().unwrap();
    let Some(field) = temp.get(&tok.lexeme) else {
      error!(
        self,
        format!(
          "[line {line}] Field '{}' not found have fields {:?}",
          tok.lexeme,
          s.fields
        ).as_str()
      );
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    };

    let ndd = declared_ast:Expression::Dot(DotExpression{ object: Box::new(nde), field:  tok, line_no:  line});
    if !at {
      return (ndd, field.value);
    } else {
      return (ndd, ValueType::Pointer(field.value, true).intern());
    }
  }

  #[instrument(level = "trace", skip_all)]
  pub fn function(&mut self, function_expr: FunctionExpression, line: usize) -> (declared_ast::Expression, UValueType) {
    let mut capture_size = 0;
    let mut captures: Vec<(SharedString, UValueType)> = Vec::new();
    for cap in &function_expr.captures {
      let (_ndc, t) = self.variable(
        Token { lexeme: cap.clone(), token_type: TokenType::Identifier, line },
        false,
        line
      );
      capture_size += t.num_words();
      captures.push((cap.clone(), t));
    }

    let c = FunctionCompiler::new(
      Some(std::mem::take(&mut self.function_compiler)),
      &function_expr.name.clone()
    );
    self.function_compiler = Box::new(c);
    self.function_compiler.scope_depth += 1;

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
      local_type: ValueType::Closure(ft.into(), capture_size).intern(),
    });
    self.function_compiler.local_count += capture_size;

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

    for cap in captures {
      let t = cap.1;
      self.function_compiler.locals.insert(self.function_compiler.local_count, Local {
        name: cap.0,
        depth: 0,
        mutable: false,
        assigned: false,
        captured: true,
        local_type: t,
      });
      self.function_compiler.local_count += t.num_words();
    }

    let mut does_return = false;
    let mut ndbody = Vec::new();
    for d in &function_expr.body {
      let (b, r) = self.visit(d.clone());
      does_return = does_return || r;
      ndbody.push(b);
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
    let locals = c.locals.clone();

    let maybe_enclosing = c.recover_enclosing();
    let Some(enclosing) = maybe_enclosing else {
      error!(self, "[line {line}] Cannot return from top-level code.");
      return (declared_ast::Expression::Empty, ValueType::Err.intern());
    };

    self.function_compiler = enclosing;

    // if any upvalues are non-const we can not escape
    let ndfe = declared_ast::FunctionExpression {
      name: function_expr.name.clone(),
      params: function_expr.params.clone(),
      captures: function_expr.captures.clone(),
      return_type: function_expr.return_type,
      body: ndbody,
      locals: locals.iter().map(|(i, l)| (*i, declared_ast::Local{ name: l.name.clone(), depth: l.depth, local_type: l.local_type })).collect(),
    };
    let nde = declared_ast::Expression::Function(ndfe, line);

    return (nde, ValueType::Closure(ft.into(), capture_size).intern());
  }

  #[instrument(level = "trace", skip_all)]
  pub fn cast(
    &mut self,
    value: Expression,
    mcast_type: Option<UValueType>,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let (ndvalue, t) = self.visit_expression(value, false);
    let from_type = t;

    let ndcexpr = declared_ast::Expression::Cast(Box::new(ndvalue), mcast_type, from_type, line);

    let Some(cast_type) = mcast_type else {
      if let ValueType::Pointer(ms, b) = t.decay(self.custom_structs.clone().into()).as_ref() {
        if let ValueType::Struct(s) = ms.decay(self.custom_structs.clone().into()).as_ref() {
          if s.embed.is_some() {
            return (ndcexpr, ValueType::Pointer(
              self.custom_structs
                .get(s.embed.as_ref().unwrap())
                .map(|s| ValueType::Struct(s.clone()).intern())
                .unwrap_or(ValueType::Err.intern()),
              *b
            ).intern());
          }
        }
      }
      return (ndcexpr, t);
    };

   let newt = match (t.decay(self.custom_structs.clone().into()).as_ref(), cast_type.as_ref()) {
      (x, y) if x == y => x.intern(),
      (ValueType::Integer, ValueType::Float) => { ValueType::Float.intern() }
      (ValueType::Float, ValueType::Integer) => { ValueType::Integer.intern() }
      (ValueType::Integer, ValueType::Bool) => { ValueType::Bool.intern() }
      (ValueType::Bool, ValueType::Integer) => { ValueType::Integer.intern() }
      (ValueType::Bool, ValueType::Float) => { ValueType::Float.intern() }
      (ValueType::UInteger, ValueType::Integer) => { ValueType::Integer.intern() }
      (ValueType::UInteger, ValueType::Float) => { ValueType::Float.intern() }
      (ValueType::Integer, ValueType::UInteger) => { ValueType::UInteger.intern() }
      (ValueType::UInteger, ValueType::Bool) => { ValueType::Bool.intern() }
      (ValueType::Bool, ValueType::UInteger) => { ValueType::UInteger.intern() }
      (ValueType::Array(x, _), ValueType::Pointer(y, _)) if x == y => {
        ValueType::Pointer(*x, false).intern()
      }
      (ValueType::Pointer(_, _), ValueType::Pointer(_, _)) => cast_type,
      _ => {
        error!(self, &format!("[line {line}] Cannot cast {t:?} to {cast_type:?}"));
        ValueType::Err.intern()
      }
    };
    (ndcexpr, newt)
  }

  #[instrument(level = "trace", skip_all)]
  pub fn struct_literal(
    &mut self,
    t: CustomStruct,
    fields: HashMap<SharedString, Expression>,
    line: usize
  ) -> (declared_ast::Expression, UValueType) {
    let l = t.fields.read();
    let temp = l.as_ref().unwrap();
    let mut orig_fields = temp.iter().collect::<Vec<_>>();
    orig_fields.sort_by(|a, b| a.1.offset.cmp(&b.1.offset));

    let mut ndfields = HashMap::new();
    for (name, entry) in &orig_fields {
      let Some(exp) = fields.get(*name) else {
        for _ in 0..entry.value.num_words() {
        }
        continue;
      };
      let (ndexp, ft) = self.visit_expression(exp.clone(), false);
      ndfields.insert(name.to_string(), ndexp);
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
        return (declared_ast::Expression::Empty, ValueType::Err.intern());
      }
    }
    drop(l);
    (declared_ast::Expression::StructInitializer(t.clone(), ndfields, line), ValueType::Struct(t).intern())
  }

  fn compiler_at(level: u32, compiler: &mut FunctionCompiler) -> Option<&mut FunctionCompiler> {
    if level == 0 {
      Some(compiler)
    } else {
      return compiler.enclosing.as_mut().and_then(|e| Self::compiler_at(level - 1, e));
    }
  }

  fn resolve_local(&mut self, compiler_level: u32, token: &Token) -> (Option<usize>, bool, u32) {
    let Some(compiler) = Self::compiler_at(compiler_level, &mut self.function_compiler) else {
      return (None, false, compiler_level);
    };
    if compiler.locals.is_empty() {
      return (None, true, compiler_level);
    }
    for (k, local) in &compiler.locals {
      if token.lexeme == local.name {
        if local.depth == -1 {
          error!(self, "Cannot read local variable in its own initializer.");
          return (None, true, compiler_level);
        }
        return (Some(*k), local.mutable || !local.assigned, compiler_level);
      }
    }
    return (None, false, compiler_level);
    // self.resolve_local(compiler_level + 1, token)
  }
}
