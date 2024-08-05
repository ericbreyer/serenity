mod declarations;
mod expressions;
mod statements;

use tracing::{ debug, instrument, warn, Level };

use crate::{
  chunk::Opcode,
  common::{
    declared_ast::{ Declaration, Expression, Local, NDASTNode, Statement },
    runnable::{ StackVMFunction, Runnable },
    CompileResult,
    TypeCheckResult,
  },
  error,
  typing::{ CustomStruct, UValueType, ValueType },
  value::{ pointer::Pointer, Value, Word },
};

use std::{ cell::Cell, collections::{HashMap, VecDeque}, fmt::Debug, rc::Rc };

pub struct EmitWalker {
  had_error: Cell<bool>,
  panic_mode: Cell<bool>,
  function_compiler: Box<FunctionCompiler>,
  static_data_segment: Vec<Word>,
  function_segment: Vec<Runnable>,
  num_functions: usize,
  gloabls: HashMap<SharedString, usize>,
  global_types: HashMap<usize, UValueType>,
  custom_structs: Rc<HashMap<SharedString, CustomStruct>>,
}

impl Debug for EmitWalker {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Compiler")
  }
}

impl EmitWalker {
  fn new(
    compiler: Box<FunctionCompiler>,
    native_functions: &HashMap<SharedString, (usize, UValueType)>,
    custom_structs: HashMap<SharedString, CustomStruct>,
    globals: HashMap<SharedString, usize>,
    global_types: HashMap<usize, UValueType>
  ) -> EmitWalker {
    let mut c = EmitWalker {
      had_error: false.into(),
      panic_mode: false.into(),
      function_compiler: compiler,
      static_data_segment: Vec::new(),
      gloabls: globals,
      global_types: global_types,
      function_segment: Vec::new(),
      num_functions: 0,
      custom_structs: custom_structs.into(),
    };

    for (_s, i) in &c.gloabls {
      let t = c.global_types.get(i).unwrap();
        for _ in 0..t.num_words() {
          c.static_data_segment.push(Value::Nil.to_word());
        }
    }

    for (name, (id, _t)) in native_functions {
      let glob_id = c.gloabls.get(name).unwrap();
      c.static_data_segment[*glob_id] = Pointer::Function(*id).to_word();

      // c.gloabls.insert(name.clone(), c.static_data_segment.len() - 1);
      // c.global_types.insert(c.static_data_segment.len() - 1, *t);
      c.num_functions += 1;
    }
    c
  }

  #[instrument(skip_all, level = "trace")]
  fn visit(&mut self, node: NDASTNode) {
    match node {
      NDASTNode::Module(_, nodes) => {
        for n in nodes {
          self.visit(n);
        }
      }
      NDASTNode::Statement(s) => self.visit_statement(s),
      NDASTNode::Expression(e) => {
        self.visit_expression(e, false);
      }
      NDASTNode::Declaration(d) => {
        self.visit_declaration(d);
      }
      NDASTNode::Group(nodes) => {
        for n in nodes {
          self.visit(n);
        }
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
      Statement::Block(nodes, locals, _line) => {
        self.begin_scope(locals);
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
      Statement::For(l, i, c, u, s, line) => {
        self.for_statement(
          l,
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
      Expression::StringLiteral(s, line) => self.SharedString(s, line),
      Expression::Unary(t, e, line) => self.unary(t, *e, line),
      Expression::Deref(e, line) => self.deref(*e, assignment_target, line),
      Expression::Ref(e, line) => self.addr_of(*e, line),
      Expression::Index(e, i, line) => self.index(*e, *i, assignment_target, line),
      Expression::Binary(l, t, r, line) => self.binary(*l, t, *r, line),
      Expression::Ternary(c, t, f, line) => self.ternary(*c, *t, *f, line),
      Expression::Variable(t, line) => self.variable(t.lexeme, assignment_target, line),
      Expression::Assign(l, r, line) => self.assign(*l, *r, line),
      Expression::Logical(l, t, r, line) => self.logical(*l, t, *r, line),
      Expression::Call(c, a, line) => self.call(*c, a, line),
      Expression::Dot(e, t, line) => self.dot(*e, t, assignment_target, line),
      Expression::Function(f, line) => self.function(f, line),
      Expression::Cast(e, t, from_t, line) => self.cast(*e, t, from_t, line),
      Expression::StructInitializer(t, v, line) => self.struct_literal(t, v, line),
      _ => ValueType::Nil.intern(),
    };

    t
  }

  #[instrument(skip_all, level = "trace")]
  fn visit_declaration(&mut self, node: Declaration) {
    match node {
      Declaration::Var(v, line) => self.var_declaration(v, line),
      Declaration::Function(f, line) => self.fun_declaration(f, line),
      Declaration::Array(a, line) => self.array_declaration(a, line),
      Declaration::Empty => {}
    }
  }

  fn begin_scope(&mut self, locals: HashMap<usize, Local>) {
    self.function_compiler.scope_depth += 1;
    self.function_compiler.locals.push_front(locals);
  }

  fn end_scope(&mut self) {
    let line = 0;
    self.function_compiler.scope_depth -= 1;
    loop {
      let last_local = self.last_local();
      if
        self.function_compiler.locals[0].get(&last_local).unwrap().depth <=
        self.function_compiler.scope_depth
      {
        break;
      }

      self.function_compiler.func.chunk.write_pop(
        self.function_compiler.locals[0].get(&last_local).unwrap().local_type,
        line
      );

      // if its an array, we need to pop the array off the stack
      if
        let ValueType::Array(t, n) = self.function_compiler.locals[0]
          .get(&last_local)
          .unwrap()
          .local_type.as_ref()
      {
        let _words = t.num_words();
        for _ in 0..*n {
          self.function_compiler.func.chunk.write_pop(*t, line);
        }
      }

      self.function_compiler.locals[0].remove(&last_local);
    }
    self.function_compiler.locals.pop_front();
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

  fn last_local(&mut self) -> usize {
    self.function_compiler.locals[0]
      .iter()
      .map(|l| *l.0)
      .max()
      .unwrap()
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



#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
  Function,
  Script,
}

struct FunctionCompiler {
  func: Box<StackVMFunction>,

  locals: VecDeque<HashMap<usize, Local>>,
  local_count: usize,
  scope_depth: i32,

  enclosing: Option<Box<FunctionCompiler>>,
}
impl Default for FunctionCompiler {
  fn default() -> Self {
    FunctionCompiler {
      locals: VecDeque::new(),
      local_count: 0,
      scope_depth: 0,
      func: Box::new(StackVMFunction::new(0, Rc::from("script"))),
      enclosing: None,
    }
  }
}

impl FunctionCompiler {
  fn new(
    function_type: FunctionType,
    current: Option<FunctionCompiler>,
    func_name: &str,
    locals: HashMap<usize, Local>,
  ) -> FunctionCompiler {
    FunctionCompiler {
      locals: vec![locals].into(),
      local_count: 0,
      scope_depth: 0,
      func: Box::new(
        StackVMFunction::new(0, if function_type == FunctionType::Script {
          Rc::from("script")
        } else {
          Rc::from(func_name)
        })
      ),
      enclosing: current.map(Box::new),
    }
  }

  fn recover_values(mut self) -> (StackVMFunction, Option<Box<FunctionCompiler>>) {
    (std::mem::take(&mut self.func), self.enclosing)
  }
}

impl EmitWalker {
  #[instrument(skip_all, level = "info")]
  pub fn emit(
    parsed: TypeCheckResult,
    native_functions: &HashMap<SharedString, (usize, UValueType)>
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
        Box::new(FunctionCompiler::new(FunctionType::Function, None, "script", HashMap::new())),
        native_functions,
        parsed.custom_structs,
        parsed.global_vars,
        parsed.global_types
      );

      comiler.visit(parsed.ast);

      had_err = comiler.had_error.get();
      compiler = *comiler.function_compiler;
      static_data_segment = comiler.static_data_segment;
      function_segment = comiler.function_segment;
    }
    let (mut func, _) = compiler.recover_values();
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
