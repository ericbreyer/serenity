mod declarations;
mod expressions;
mod statements;

use tracing::{ debug, instrument, warn, Level };

use crate::common::ast::{ ASTNode, Declaration, Expression, Statement };
use crate::common::ParseResult;
use crate::common::runnable::Function;
use crate::error;

use crate::typing::{ CustomStruct, UValueType };
use crate::typing::ValueType;
use crate::value::Value::{ self };

use std::cell::Cell;

use std::collections::HashMap;

use std::fmt::Debug;
use std::rc::Rc;

use crate::chunk::Opcode;

pub struct OptimizationWalker {
    had_error: Cell<bool>,
    panic_mode: Cell<bool>,
    function_compiler: Box<FunctionCompiler>,
    _globals: HashMap<String, Option<Value>>,
}

impl Debug for OptimizationWalker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compiler")
    }
}

impl OptimizationWalker {
    fn new(
        compiler: Box<FunctionCompiler>,
        _native_functions: &HashMap<String, (usize, UValueType)>,
        _custom_structs: HashMap<String, CustomStruct>
    ) -> OptimizationWalker {
        
        OptimizationWalker {
            had_error: false.into(),
            panic_mode: false.into(),
            function_compiler: compiler,
            _globals: HashMap::new(),
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit(&mut self, node: &mut ASTNode) {
        match node {
            ASTNode::Module(nodes) => {
                for n in nodes {
                    self.visit(n);
                }
            }
            ASTNode::Statement(s, _) => self.visit_statement(s),
            ASTNode::Expression(e, l) => {
                self.visit_expression(e.clone(), false);
                *node = ASTNode::Expression(e.clone(), *l);
            }
            ASTNode::Declaration(d, _) => {
                self.visit_declaration(d);
            }
            _ => (),
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_statement(&mut self, node: &mut Statement) {
        match node {
            Statement::Print(e) => {
                self.print(e);
            }
            Statement::Block(nodes) => {
                self.begin_scope();
                for n in nodes {
                    self.visit(n);
                }
                self.end_scope();
            }
            Statement::If(e, s, o) => {
                self.if_statement(
                    *e.clone(),
                    *s.clone(),
                    o.clone().map(|x| *x)
                );
            }
            Statement::While(e, s) => {
                self.while_statement(*e.clone(), *s.clone());
            }
            Statement::For(i, c, u, s) => {
                self.for_statement(
                    i.clone().map(|x| *x),
                    c.clone().map(|x| *x),
                    u.clone().map(|x| *x),
                    *s.clone()
                );
            }
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Return(s) => {
                if let Some(e) = s {
                    let sgg = self.visit_expression(*e.clone(), false);
                    *node = Statement::Return(Some(Box::new(sgg)));
                }
            }
            Statement::Expression(e) => {
                let e = self.visit_expression(*e.clone(), false);
                *node = Statement::Expression(Box::new(e));
            }
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_expression(&mut self, node: Expression, assignment_target: bool) -> Expression {
        if assignment_target {
            match node {
                Expression::Deref(_) => (),
                Expression::Index(_, _) => (),
                Expression::Variable(_) => (),
                Expression::Dot(_, _) => (),
                _ => {
                    error!(self, "Invalid in assignment target expression.");
                    return Expression::Empty;
                }
            }
        }
        

        match node {
            Expression::Literal(v) => self.literal(v),
            Expression::StringLiteral(s) => self.string(s),
            Expression::Unary(t, e) => self.unary(t, *e),
            Expression::Deref(e) => self.deref(*e, assignment_target),
            Expression::Ref(e) => self.addr_of(*e),
            Expression::Index(e, i) => self.index(*e, *i, assignment_target),
            Expression::Binary(l, t, r) => self.binary(*l, t, *r),
            Expression::Ternary(c, t, f) => self.ternary(*c, *t, *f),
            Expression::Variable(t) => self.variable(t, assignment_target),
            Expression::Assign(l, r) => self.assign(*l, *r),
            Expression::Logical(l, t, r) => self.logical(*l, t, *r),
            Expression::Call(c, a) => self.call(*c, a),
            Expression::Dot(e, t) => self.dot(*e, t, assignment_target),
            Expression::Function(f) => self.function(f),
            Expression::Cast(e, t, o) => self.cast(*e, t, o),
            Expression::ArrayLiteral(a) => self.array_literal(a),
            Expression::StructInitializer(t, v) => self.struct_literal(t, v),
            _ => Expression::Empty,
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_declaration(&mut self, node: &mut Declaration) {
        match node {
            Declaration::Var(v) => self.var_declaration(v),
            Declaration::Function(f) => self.fun_declaration(f),
            Declaration::Struct(s) => self.struct_declaration(s),
            Declaration::Array(a) => self.array_declaration(a),
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
                    .unwrap().local_type.as_ref()
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

struct Local {
    name: String,
    depth: i32,
    local_type: UValueType,
}

#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
    Function,
    Script,
}

struct FunctionCompiler {
    func: Box<Function>,

    locals: HashMap<usize, Local>,
    local_count: usize,
    scope_depth: i32,

    enclosing: Option<Box<FunctionCompiler>>,
}

impl Default for FunctionCompiler {
    fn default() -> Self {
        FunctionCompiler {
            locals: HashMap::new(),
            local_count: 0,
            scope_depth: 0,
            func: Box::new(Function::new(0, Rc::from("script"))),
            enclosing: None,
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
        };

        c.local_count += 3;
        c
    }

    fn recover_values(mut self) -> (Function, Option<Box<FunctionCompiler>>) {
        (std::mem::take(&mut self.func), self.enclosing)
    }
}

impl OptimizationWalker {
    #[instrument(skip_all, level = "info")]
    pub fn optimize(
        mut parsed: ParseResult,
        native_functions: &HashMap<String, (usize, UValueType)>
    ) -> ParseResult {
        if parsed.had_errors {
            tracing::error!("Parsing failed.");
            return parsed;
        }
        debug!("ast {:?}", parsed.ast);

        let compiler: FunctionCompiler;
        let _end_line: usize;
        let had_err: bool;
        {
            let mut comiler = OptimizationWalker::new(
                Box::new(FunctionCompiler::new(FunctionType::Function, None, "script")),
                native_functions,
                parsed.custom_structs.clone()
            );

            comiler.visit(&mut parsed.ast);

            had_err = comiler.had_error.get();
            compiler = *comiler.function_compiler;
        }
        let (mut func, _) = compiler.recover_values();
        let chunk = &mut func.chunk;

        chunk.disassemble(
            if func.name != "".into() {
                &func.name
            } else {
                "script"
            },
            Level::DEBUG
        );

        chunk.write(Opcode::Nil.into(), 0);
        chunk.write_pool_opcode(Opcode::Return, 1, 0);

        if had_err {
            tracing::error!("Optimization failed.");
        } else {
            tracing::info!("Optimization succeeded.");
        }
        parsed
    }
}
