mod declarations;
mod expressions;
mod statements;

use tracing::{ debug, instrument, warn, Level };

use crate::common::ast::{ ASTNode, Declaration, Expression, Statement };
use crate::common::{ CompileResult, ParseResult };
use crate::common::runnable::{ Function, Runnable };
use crate::value::pointer::Pointer;
use crate::typing::{ CustomStruct, ValueType };
use crate::typing::ValueTypeK;
use crate::value::Value::{ self };
use crate::value::Word;

use std::array;
use std::cell::Cell;

use std::collections::HashMap;

use std::fmt::Debug;
use std::rc::Rc;

use crate::chunk::Opcode;

#[macro_export]
macro_rules! error {
    ($self:ident, $message:expr) => {
        $self.error($message, file!(), line!());
    };
}

pub struct Compiler {
    had_error: Cell<bool>,
    panic_mode: Cell<bool>,
    function_compiler: Box<FunctionCompiler>,
    static_data_segment: Vec<Word>,
    function_segment: Vec<Runnable>,
    num_functions: usize,
    gloabls: HashMap<String, usize>,
    global_types: HashMap<usize, (ValueType, bool, bool)>,
    custom_structs: Rc<HashMap<String, CustomStruct>>,
}

impl Debug for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compiler")
    }
}

impl Compiler {
    fn new(
        compiler: Box<FunctionCompiler>,
        native_functions: &HashMap<String, (usize, ValueType)>,
        custom_structs: HashMap<String, CustomStruct>
    ) -> Compiler {
        let mut c = Compiler {
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
        for (name, (id, t)) in native_functions.iter() {
            c.static_data_segment.extend(Value::Pointer(Pointer::Function(*id)).to_words());
            c.static_data_segment.push((0).into());
            c.static_data_segment.push(
                (Box::into_raw(Box::new(Vec::<Upvalue>::new())) as u64).into()
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
            ASTNode::Module(nodes) => {
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
            _ => (),
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_statement(&mut self, node: Statement) {
        match node {
            Statement::Print(e) => {
                self.print(*e);
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
                    *e,
                    *s,
                    o.map(|x| *x)
                );
            }
            Statement::While(e, s) => {
                self.while_statement(*e, *s);
            }
            Statement::For(i, c, u, s) => {
                self.for_statement(
                    i.map(|x| *x),
                    c.map(|x| *x),
                    u.map(|x| *x),
                    *s
                );
            }
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Return(s) => {
                let mut t = ValueTypeK::Nil.intern();
                if let Some(e) = s {
                    t = self.visit_expression(*e, false);
                }
                if t != self.function_compiler.func.return_type {
                    error!(self, "Return type does not match function return type.");
                }
                self.function_compiler.func.chunk.write(Opcode::Return.into(), 0);
                self.function_compiler.func.chunk.write(t.num_words() as u8, 0);
            }
            Statement::Expression(e) => {
                let t = self.visit_expression(*e, false);
                self.function_compiler.func.chunk.write_pop(t, 0);
            }
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_expression(&mut self, node: Expression, assignment_target: bool) -> ValueType {
        if assignment_target {
            match node {
                Expression::Deref(_) => (),
                Expression::Index(_, _) => (),
                Expression::Variable(_) => (),
                Expression::Dot(_, _) => (),
                _ => {
                    error!(self, "Invalid in assignment target expression.");
                    return ValueTypeK::Err.intern();
                }
            }
        }
        let t = match node {
            Expression::Literal(v) => self.literal(v),
            Expression::StringLiteral(s) => self.string(s),
            Expression::Grouping(e) => self.visit_expression(*e, false),
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
            Expression::Cast(e, t) => self.cast(*e, t),
            Expression::ArrayLiteral(a) => self.array_literal(a),
            Expression::StructInitializer(t, v) => self.struct_literal(t, v),
            Expression::Empty => ValueTypeK::Nil.intern(),
            Expression::Nil => ValueTypeK::Nil.intern(),
            _ => ValueTypeK::Err.intern(),
        };
        // if t2 is a function, we need to copy the function
        if let ValueTypeK::Closure(_) = t {
            self.function_compiler.func.chunk.write(Opcode::CopyClosure.into(), 0);
        }
        t
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_declaration(&mut self, node: Declaration) {
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
        while self.function_compiler.locals.len() > 0 {
            let last_local = self.last_local();
            if
                self.function_compiler.locals.get(&last_local).unwrap().depth <=
                self.function_compiler.scope_depth
            {
                break;
            }

            if self.function_compiler.locals.get(&last_local).unwrap().captured {
                if self.function_compiler.locals.get(&last_local).unwrap().mutable {
                    error!(self, "Cannot capture mutable variables.");
                }
                self.function_compiler.func.chunk.write_pool_opcode(
                    Opcode::CloseUpvalue.into(),
                    self.function_compiler.locals
                        .get(&last_local)
                        .unwrap()
                        .local_type.num_words() as u32,
                    line
                );
            } else {
                self.function_compiler.func.chunk.write_pop(
                    self.function_compiler.locals.get(&last_local).unwrap().local_type,
                    line
                );
            }

            // if its an array, we need to pop the array off the stack
            if
                let ValueTypeK::Array(t, n) = self.function_compiler.locals
                    .get(&last_local)
                    .unwrap().local_type
            {
                let words = t.num_words();
                for _ in 0..*n {
                    self.function_compiler.func.chunk.write_pop(t, line);
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

    fn define_variable(&mut self, global_id: u32, local_type: ValueType) {
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
        warn!("[compiler] warning {}", message);
    }
}

struct Local {
    name: String,
    depth: i32,
    mutable: bool,
    assigned: bool,
    captured: bool,
    local_type: ValueType,
}

#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
    Function,
    Script,
}

struct FunctionCompiler {
    func: Box<Function>,
    upvalues: [Upvalue; u8::MAX as usize],

    locals: HashMap<usize, Local>,
    local_count: usize,
    scope_depth: i32,

    enclosing: Option<Box<FunctionCompiler>>,
}
struct Upvalue {
    pub index: usize,
    pub is_local: bool,
    pub upvalue_type: ValueType,
}

impl Default for FunctionCompiler {
    fn default() -> Self {
        FunctionCompiler {
            locals: HashMap::new(),
            local_count: 0,
            scope_depth: 0,
            func: Box::new(Function::new(0, Rc::from("script"))),
            enclosing: None,
            upvalues: array::from_fn(|_| Upvalue {
                index: 0,
                is_local: false,
                upvalue_type: ValueTypeK::Undef.intern(),
            }),
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
            upvalues: array::from_fn(|_| Upvalue {
                index: 0,
                is_local: false,
                upvalue_type: ValueTypeK::Undef.intern(),
            }),
        };
        c.locals.insert(c.local_count, Local {
            name: "this".into(),
            depth: 0,
            mutable: false,
            assigned: false,
            captured: false,
            local_type: ValueTypeK::Nil.intern(),
        });

        c.local_count += 3;
        c
    }

    fn recover_values(
        mut self
    ) -> (Function, Option<Box<FunctionCompiler>>, [Upvalue; u8::MAX as usize]) {
        (std::mem::take(&mut self.func), self.enclosing, self.upvalues)
    }
}

impl Compiler {
    #[instrument(skip_all, level = "trace")]
    pub fn compile(
        parsed: ParseResult,
        native_functions: &HashMap<String, (usize, ValueType)>
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
            let mut comiler = Compiler::new(
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

        chunk.dissassemble(
            if func.name != "".into() {
                &func.name
            } else {
                "script"
            },
            Level::DEBUG
        );

        chunk.write(Opcode::Nil.into(), 0);
        chunk.write_pool_opcode(Opcode::Return.into(), 1, 0);

        if had_err {
            return CompileResult {
                function: None,
                static_data_segment: Vec::new(),
                function_segment: Vec::new(),
            };
        } else {
            return CompileResult {
                function: Some(func),
                static_data_segment: static_data_segment,
                function_segment: function_segment,
            };
        }
    }
}
