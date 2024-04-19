mod declarations;
mod expressions;
mod statements;


use tracing::{ debug, instrument, warn };

use crate::{
  error,
  typing::{ CustomStruct, UValueType, ValueType },
  common::{
    ParseResult,
    ast::{ ASTNode, Declaration, Expression, Statement },
  },
};

use std::{cell::Cell, collections::HashMap, fmt::Debug, rc::Rc, sync::atomic::AtomicU64};

pub struct TypeCheckWalker {
  had_error: Cell<bool>,
  panic_mode: Cell<bool>,
  function_compiler: Box<FunctionCompiler>,
  gloabls: HashMap<String, usize>,
  global_types: HashMap<usize, (UValueType, bool)>,
  custom_structs: Rc<HashMap<String, CustomStruct>>,
}

impl Debug for TypeCheckWalker {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "TypeChecker")
  }
}

impl TypeCheckWalker {
  fn new(
    compiler: Box<FunctionCompiler>,
    native_functions: &HashMap<String, (usize, UValueType)>,
    custom_structs: HashMap<String, CustomStruct>
  ) -> TypeCheckWalker {
    let mut c = TypeCheckWalker {
      had_error: false.into(),
      panic_mode: false.into(),
      function_compiler: compiler,
      gloabls: HashMap::new(),
      global_types: HashMap::new(),
      custom_structs: custom_structs.into(),
    };
    for (i, (name, (_id, t))) in native_functions.iter().enumerate() {
      c.gloabls.insert(name.clone(), i);
      c.global_types.insert(i, (*t, false));
    }
    c
  }

  #[instrument(skip_all, level = "trace")]
  fn visit(&mut self, node: ASTNode) -> (ASTNode, bool) {
    let mut has_return = false;
    match node.clone() {
      ASTNode::Module(_, nodes) => {
        for n in &nodes[0..nodes.len()] {
          match n {
            ASTNode::Declaration(_) => (),
            ASTNode::Module(_, _) => (),
            ASTNode::CallMain(_) => (),
            _ => {
              error!(self, "Only declarations are allowed at the top level.");
              return (ASTNode::Err, false);
            }
          }

          self.visit(n.clone());
        }
      }
      ASTNode::Statement(s) => {
        has_return = self.visit_statement(s);
      }
      ASTNode::Expression(e) => {
        self.visit_expression(e, false);
      }
      ASTNode::Declaration(d) => {
        self.visit_declaration(d);
      }
      ASTNode::CallMain(c) => {
        self.visit(c.as_ref().clone());
      }
      _ => (),
    }
    (node, has_return)
  }

  #[instrument(skip_all, level = "trace")]
  fn visit_statement(&mut self, node: Statement) -> bool {
    static LOOP_NEST: AtomicU64 = AtomicU64::new(0);

    match node {
      Statement::Print(e, line) => { self.print(*e, line) }
      Statement::Block(nodes, _line) => {
        self.begin_scope();
        let mut has_return = false;
        for n in nodes {
          let (n_prime, r) = self.visit(n);
          has_return = has_return || r;
          // if nprime was a break, return, or continue, we should break out of the loop
          if
            matches!(
              n_prime,
              ASTNode::Statement(
                Statement::Break(_) | Statement::Return(_, _) | Statement::Continue(_)
              )
            )
          {
            break;
          }
        }
        self.end_scope();

        has_return
      }
      Statement::If(e, s, o, line) => {
        self.if_statement(
          *e,
          *s,
          o.map(|x| *x), line
        )
      }
      Statement::While(e, s, line) => {
        LOOP_NEST.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let r = self.while_statement(*e, *s, line);
        LOOP_NEST.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        r
      }
      Statement::For(i, c, u, s, line) => {
        LOOP_NEST.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let r = self.for_statement(
          i.map(|x| *x),
          c.map(|x| *x),
          u.map(|x| *x),
          *s, line
        );
        LOOP_NEST.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        r
      }
      Statement::Break(line) => {
        if LOOP_NEST.load(std::sync::atomic::Ordering::SeqCst) == 0 {
          error!(self, &format!("[line {line}] Cannot break outside of loop."));
        }
        false
      }
      Statement::Continue(line) => {
        if LOOP_NEST.load(std::sync::atomic::Ordering::SeqCst) == 0 {
          error!(self, &format!("[line {line}] Cannot continue outside of loop."));
        }
        false
      }
      Statement::Return(s, line) => {
        let mut t = ValueType::Nil.intern();
        if let Some(e) = s {
          t = self.visit_expression(*e, false);
        }
        if t != self.function_compiler.return_type {
          error!(
            self,
            format!(
              "[line {line}] Expected return type {:?}, got {:?}",
              self.function_compiler.return_type,
              t
            ).as_str()
          );
        }

        true
      }
      Statement::Expression(e, _line) => {
        self.visit_expression(*e, false);
        false
      }
    }
  }

  #[instrument(skip_all, level = "trace")]
  fn visit_expression(&mut self, node: Expression, assignment_target: bool) -> UValueType {
    // if assignment_target {
    //     match node {
    //         Expression::Deref(_) => (),
    //         Expression::Index(_, _) => (),
    //         Expression::Variable(_) => (),
    //         Expression::Dot(_, _) => (),
    //         _ => {
    //             error!(self,format!("Invalid in assignment target expression {:?}.", ASTNode::Expression(node, 0)).as_str());
    //             return ValueType::Err.intern();
    //         }
    //     }
    // }

    match node {
      Expression::Literal(v, line) => self.literal(v, line),
      Expression::StringLiteral(s, line) => self.string(s, line),
      Expression::Unary(t, e, line) => self.unary(t, *e, line),
      Expression::Deref(e, line) => self.deref(*e, assignment_target, line),
      Expression::Ref(e, line) => self.addr_of(*e, line),
      Expression::Index(e, i, line) => self.index(*e, *i, assignment_target, line),
      Expression::Binary(l, t, r, line) => self.binary(*l, t, *r, line),
      Expression::Ternary(c, t, f, line) => self.ternary(*c, *t, *f, line),
      Expression::Variable(t, line) => self.variable(t, assignment_target, line),
      Expression::Assign(l, r, line) => self.assign(*l, *r, line),
      Expression::Logical(l, t, r, line) => self.logical(*l, t, *r, line),
      Expression::Call(c, a, line) => self.call(*c, a, line),
      Expression::Dot(e, t, line) => self.dot(*e, t, assignment_target, line),
      Expression::Function(f, line) => self.function(f, line),
      Expression::Cast(e, t, ref from_t, line) => self.cast(*e, t, from_t, line),
      Expression::ArrayLiteral(a, line) => self.array_literal(a, line),
      Expression::StructInitializer(t, v, line) => self.struct_literal(t, v, line),
      _ => ValueType::Nil.intern(),
    }
  }

  #[instrument(skip_all, level = "trace")]
  fn visit_declaration(&mut self, node: Declaration) {
    match node {
      Declaration::Var(v, line) => self.var_declaration(v, line),
      Declaration::Function(f, line) => self.fun_declaration(f, line),
      Declaration::Struct(s, line) => self.struct_declaration(s, line),
      Declaration::Array(a, line) => self.array_declaration(a, line),
    }
  }

  fn begin_scope(&mut self) {
    self.function_compiler.scope_depth += 1;
  }

  fn end_scope(&mut self) {
    let _line = 0;
    self.function_compiler.scope_depth -= 1;
    while !self.function_compiler.locals.is_empty() {
      let last_local = self.last_local();
      if
        self.function_compiler.locals.get(&last_local).unwrap().depth <=
        self.function_compiler.scope_depth
      {
        break;
      }

      if
        self.function_compiler.locals.get(&last_local).unwrap().captured &&
        self.function_compiler.locals.get(&last_local).unwrap().mutable
      {
        error!(self, "Cannot capture mutable variables.");
      }

      // if its an array, we need to pop the array off the stack
      if
        let ValueType::Array(t, n) = self.function_compiler.locals
          .get(&last_local)
          .unwrap()
          .local_type.as_ref()
      {
        let words = t.num_words();
        for _ in 0..*n {
          self.function_compiler.local_count -= words;
        }
      }
      self.function_compiler.local_count -= self.function_compiler.locals
        .get(&last_local)
        .unwrap()
        .local_type.num_words();

      self.function_compiler.locals.remove(&last_local);
    }
  }

  fn define_variable(&mut self, _global_id: u32, local_type: UValueType) {
    if self.function_compiler.scope_depth > 0 {
      self.mark_initialized();
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = local_type;
      self.function_compiler.local_count += local_type.num_words();
    }
    // if local_type == ValueTypeK::AnyFunction.intern() {
    //     self.compiler
    //         .func
    //         .chunk
    //         .write(Opcode::CopyClosure.into(), 0);
    // }
  }

  fn last_local(&mut self) -> usize {
    self.function_compiler.locals
      .iter()
      .map(|l| *l.0)
      .max()
      .unwrap()
  }

  fn mark_initialized(&mut self) {
    if self.function_compiler.scope_depth == 0 {
      return;
    }
    let last_local = self.function_compiler.locals
      .iter()
      .map(|l| *l.0)
      .max()
      .unwrap();
    self.function_compiler.locals.get_mut(&last_local).unwrap().depth =
      self.function_compiler.scope_depth;
  }

  fn error(&self, message: &str, from: &str, line: u32) {
    self.error_at(true, message, from, line);
  }

  fn error_at(&self, _prev: bool, message: &str, from: &str, line: u32) {
    if self.panic_mode.get() {
      return;
    }
    self.panic_mode.set(true);
    tracing::error!("[compiler] error at {}:{} - {}", from, line, message);

    self.had_error.set(true);
  }

  fn warn(&self, message: &str) {
    self.warn_at(true, message);
  }

  fn warn_at(&self, _prev: bool, message: &str) {
    if self.panic_mode.get() {
      return;
    }
    // self.panic_mode.set(true);
    warn!("{}", message);
  }
}

#[derive(Debug, Clone)]
struct Local {
  name: String,
  depth: i32,
  mutable: bool,
  assigned: bool,
  captured: bool,
  local_type: UValueType,
}

struct FunctionCompiler {
  upvalues: Vec<Upvalue>,
  return_type: UValueType,

  locals: HashMap<usize, Local>,
  local_count: usize,
  upvalue_count: usize,
  scope_depth: i32,

  enclosing: Option<Box<FunctionCompiler>>,
}
struct Upvalue {
  pub name: String,
  pub local_idx: usize,
}

impl Default for FunctionCompiler {
  fn default() -> Self {
    FunctionCompiler {
      locals: HashMap::new(),
      return_type: ValueType::Nil.intern(),
      local_count: 0,
      upvalue_count: 0,
      scope_depth: 0,
      enclosing: None,
      upvalues: Vec::new(),
    }
  }
}

impl FunctionCompiler {
  fn new(current: Option<FunctionCompiler>, _func_name: &str) -> FunctionCompiler {
    let mut c = FunctionCompiler {
      locals: HashMap::new(),
      return_type: ValueType::Nil.intern(),
      local_count: 0,
      upvalue_count: 0,
      scope_depth: 0,
      enclosing: current.map(Box::new),
      upvalues: Vec::new(),
    };
    c.locals.insert(c.local_count, Local {
      name: "this".into(),
      depth: 0,
      mutable: false,
      assigned: false,
      captured: false,
      local_type: ValueType::Nil.intern(),
    });

    c.local_count += 3;
    c
  }

  fn recover_values(self) -> (Option<Box<FunctionCompiler>>, Vec<Upvalue>) {
    (self.enclosing, self.upvalues)
  }
}

impl TypeCheckWalker {
  #[instrument(skip_all, level = "info")]
  pub fn type_check(
    parsed: ParseResult,
    native_functions: &HashMap<String, (usize, UValueType)>
  ) -> ParseResult {
    if parsed.had_errors {
      tracing::error!("Parsing failed.");
      return parsed;
    }
    debug!("ast {:?}", parsed.ast);

    let mut comiler = TypeCheckWalker::new(
      Box::new(FunctionCompiler::new(None, "script")),
      native_functions,
      parsed.custom_structs.clone()
    );

    let (ast, _) = comiler.visit(parsed.ast);

    let had_err = comiler.had_error.get();

    if had_err {
      tracing::error!("Type checking failed.");
      return ParseResult {
        had_errors: true,
        ast,
        custom_structs: parsed.custom_structs,
      };
    } else {
      tracing::info!("Type checking succeeded.");
      return ParseResult {
        had_errors: false,
        ast,
        custom_structs: parsed.custom_structs,
      };
    }
  }
}
