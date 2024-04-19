mod declarations;
mod expressions;
mod statements;

use tracing::{ debug, instrument, warn, Level };

use crate::{
  error,
  chunk::Opcode,
  value::{ pointer::Pointer, Value, Word },
  typing::{ CustomStruct, UValueType, ValueType },
  common::{
    CompileResult,
    ParseResult,
    ast::{ ASTNode, Declaration, Expression, Statement },
    runnable::{ Function, Runnable },
  },
};

use std::{cell::Cell, collections::HashMap, fmt::Debug, rc::Rc};

pub struct EmitWalker {
  had_error: Cell<bool>,
  panic_mode: Cell<bool>,
  function_compiler: Box<FunctionCompiler>,
  static_data_segment: Vec<Word>,
  function_segment: Vec<Runnable>,
  num_functions: usize,
  gloabls: HashMap<String, usize>,
  global_types: HashMap<usize, (UValueType, bool, bool)>,
  custom_structs: Rc<HashMap<String, CustomStruct>>,
}

impl Debug for EmitWalker {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Compiler")
  }
}

impl EmitWalker {
  fn new(
    compiler: Box<FunctionCompiler>,
    native_functions: &HashMap<String, (usize, UValueType)>,
    custom_structs: HashMap<String, CustomStruct>
  ) -> EmitWalker {
    let mut c = EmitWalker {
      had_error: false.into(),
      panic_mode: false.into(),
      function_compiler: compiler,
      static_data_segment: Vec::new(),
      gloabls: HashMap::new(),
      global_types: HashMap::new(),
      function_segment: Vec::new(),
      num_functions: 0,
      custom_structs: custom_structs.into(),
    };
    for (name, (id, t)) in native_functions {
      c.static_data_segment.extend(
        Value::Closure(crate::value::Closure {
          func: Pointer::Function(*id),
          upvalue_count: 0,
          upvalue_segment: Rc::new(Vec::new()),
        }).to_words()
      );

      c.gloabls.insert(name.clone(), c.static_data_segment.len() - 3);
      c.global_types.insert(c.static_data_segment.len() - 3, (*t, false, true));
      c.num_functions += 1;
    }
    c
  }

  #[instrument(skip_all, level = "trace")]
  fn visit(&mut self, node: ASTNode) {
    match node {
      ASTNode::Module(_, nodes) => {
        for n in nodes {
          self.visit(n);
        }
      }
      ASTNode::Statement(s) => self.visit_statement(s),
      ASTNode::Expression(e) => {
        self.visit_expression(e, false);
      }
      ASTNode::Declaration(d) => {
        self.visit_declaration(d);
      }
      ASTNode::CallMain(c) => {
        self.visit(*c);
      }
      _ => (),
    }
  }

  #[instrument(skip_all, level = "trace")]
  fn visit_statement(&mut self, node: Statement) {
    match node {
      Statement::Print(e, line) => {
        self.print(*e, line);
      }
      Statement::Block(nodes, _line) => {
        self.begin_scope();
        for n in nodes {
          self.visit(n);
        }
        self.end_scope();
      }
      Statement::If(e, s, o, line) => {
        self.if_statement(
          *e,
          *s,
          o.map(|x| *x),
          line
        );
      }
      Statement::While(e, s, line) => {
        self.while_statement(*e, *s, line);
      }
      Statement::For(i, c, u, s, line) => {
        self.for_statement(
          i.map(|x| *x),
          c.map(|x| *x),
          u.map(|x| *x),
          *s,
          line
        );
      }
      Statement::Break(_line) => {
        self.function_compiler.func.chunk.write(Opcode::Break.into(), 0);
        self.function_compiler.func.chunk.write(0xff, 0);
        self.function_compiler.func.chunk.write(0xff, 0);
      }
      Statement::Continue(_line) => {
        self.function_compiler.func.chunk.write(Opcode::Continue.into(), 0);
        self.function_compiler.func.chunk.write(0xff, 0);
        self.function_compiler.func.chunk.write(0xff, 0);
      }
      Statement::Return(s, _line) => {
        let mut t = ValueType::Nil.intern();
        if let Some(e) = s {
          t = self.visit_expression(*e, false);
        }
        self.function_compiler.func.chunk.write(Opcode::Return.into(), 0);
        self.function_compiler.func.chunk.write(t.num_words() as u8, 0);
      }
      Statement::Expression(e, _line) => {
        let t = self.visit_expression(*e, false);
        self.function_compiler.func.chunk.write_pop(t, 0);
      }
    }
  }

  #[instrument(skip_all, level = "trace")]
  fn visit_expression(&mut self, node: Expression, assignment_target: bool) -> UValueType {
    if assignment_target {
      match node {
        Expression::Deref(_, _) => (),
        Expression::Index(_, _, _) => (),
        Expression::Variable(_, _) => (),
        Expression::Dot(_, _, _) => (),
        _ => {
          error!(
            self,
            &format!("[line {}] Invalid in assignment target expression.", node.line_no())
          );
          return ValueType::Err.intern();
        }
      }
    }
    let t = match node {
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
      Expression::Cast(e, t, _, line) => self.cast(*e, t, line),
      Expression::ArrayLiteral(a, line) => self.array_literal(a, line),
      Expression::StructInitializer(t, v, line) => self.struct_literal(t, v, line),
      _ => ValueType::Nil.intern(),
    };
    // // if t2 is a function, we need to copy the function
    if let ValueType::Closure(_) = t.as_ref() {
      self.function_compiler.func.chunk.write(Opcode::CopyClosure.into(), 0);
    }
    t
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
    let line = 0;
    self.function_compiler.scope_depth -= 1;
    while !self.function_compiler.locals.is_empty() {
      let last_local = self.last_local();
      if
        self.function_compiler.locals.get(&last_local).unwrap().depth <=
        self.function_compiler.scope_depth
      {
        break;
      }

      self.function_compiler.func.chunk.write_pop(
        self.function_compiler.locals.get(&last_local).unwrap().local_type,
        line
      );

      // if its an array, we need to pop the array off the stack
      if
        let ValueType::Array(t, n) = self.function_compiler.locals
          .get(&last_local)
          .unwrap()
          .local_type.as_ref()
      {
        let words = t.num_words();
        for _ in 0..*n {
          self.function_compiler.func.chunk.write_pop(*t, line);
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

  fn emit_jump(&mut self, instruction: u8) -> usize {
    let line = 0;
    self.function_compiler.func.chunk.write(instruction, line);
    self.function_compiler.func.chunk.write(0xff, line);
    self.function_compiler.func.chunk.write(0xff, line);
    self.function_compiler.func.chunk.code.len() - 2
  }

  fn patch_jump(&mut self, offset: usize) {
    let jump = self.function_compiler.func.chunk.code.len() - offset - 2;
    if jump > (std::u16::MAX as usize) {
      error!(self, "Too much code to jump over.");
    }

    self.function_compiler.func.chunk.code[offset].0 = ((jump >> 8) & 0xff) as u8;
    self.function_compiler.func.chunk.code[offset + 1].0 = (jump & 0xff) as u8;
  }

  fn define_variable(&mut self, global_id: u32, local_type: UValueType) {
    if self.function_compiler.scope_depth > 0 {
      self.mark_initialized();
      let last_local = self.last_local();
      self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = local_type;
      self.function_compiler.local_count += local_type.num_words();
      return;
    }

    // for _ in 0..local_type.num_words() {
    //     self.static_data_segment.push(Value::Nil.to_word());
    // }

    self.function_compiler.func.chunk.define_global(global_id, 0, local_type);
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

#[derive(Clone, PartialEq)]
struct Local {
  name: String,
  depth: i32,
  mutable: bool,
  assigned: bool,
  captured: bool,
  local_type: UValueType,
}

impl Debug for Local {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?} {:?}", self.name, self.local_type)
  }
}

#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
  Function,
  Script,
}

struct FunctionCompiler {
  func: Box<Function>,
  upvalues: Vec<Upvalue>,

  locals: HashMap<usize, Local>,
  local_count: usize,
  scope_depth: i32,

  enclosing: Option<Box<FunctionCompiler>>,
}
struct Upvalue {
  pub upvalue_type: UValueType,
  pub name: String,
  pub idx: usize,
  pub is_local: bool,
}

impl Default for FunctionCompiler {
  fn default() -> Self {
    FunctionCompiler {
      locals: HashMap::new(),
      local_count: 0,
      scope_depth: 0,
      func: Box::new(Function::new(0, Rc::from("script"))),
      enclosing: None,
      upvalues: Vec::new(),
    }
  }
}

impl FunctionCompiler {
  fn new(
    function_type: FunctionType,
    current: Option<FunctionCompiler>,
    func_name: &str
  ) -> FunctionCompiler {
    let mut c = FunctionCompiler {
      locals: HashMap::new(),
      local_count: 0,
      scope_depth: 0,
      func: Box::new(
        Function::new(0, if function_type == FunctionType::Script {
          Rc::from("script")
        } else {
          Rc::from(func_name)
        })
      ),
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

  fn recover_values(mut self) -> (Function, Option<Box<FunctionCompiler>>, Vec<Upvalue>) {
    (std::mem::take(&mut self.func), self.enclosing, self.upvalues)
  }
}

impl EmitWalker {
  #[instrument(skip_all, level = "info")]
  pub fn emit(
    parsed: ParseResult,
    native_functions: &HashMap<String, (usize, UValueType)>
  ) -> CompileResult {
    if parsed.had_errors {
      tracing::error!("Parsing failed.");
      return CompileResult {
        function: None,
        static_data_segment: Vec::new(),
        function_segment: Vec::new(),
      };
    }
    debug!("ast {:?}", parsed.ast);

    let compiler: FunctionCompiler;
    let _end_line: usize;
    let had_err: bool;
    let static_data_segment: Vec<Word>;
    let function_segment: Vec<Runnable>;
    {
      let mut comiler = EmitWalker::new(
        Box::new(FunctionCompiler::new(FunctionType::Function, None, "script")),
        native_functions,
        parsed.custom_structs
      );

      comiler.visit(parsed.ast);

      had_err = comiler.had_error.get();
      compiler = *comiler.function_compiler;
      static_data_segment = comiler.static_data_segment;
      function_segment = comiler.function_segment;
    }
    let (mut func, _, _) = compiler.recover_values();
    let chunk = &mut func.chunk;

    chunk.disassemble(if func.name != "".into() { &func.name } else { "script" }, Level::DEBUG);

    chunk.write(Opcode::Nil.into(), 0);
    chunk.write_pool_opcode(Opcode::Return, 1, 0);

    if had_err {
      tracing::error!("Compilation failed.");
      return CompileResult {
        function: None,
        static_data_segment: Vec::new(),
        function_segment: Vec::new(),
      };
    } else {
      tracing::info!("Compilation succeeded.");
      return CompileResult {
        function: Some(func),
        static_data_segment,
        function_segment,
      };
    }
  }
}
