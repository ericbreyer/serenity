mod declarations;
mod expressions;
mod statements;

use super::iasm::{self, IASMProg};
use tracing::{debug, instrument, warn};
use iasm::IASMProgSplit;

use crate::{
    prelude::*,
    error,
    value::Value,
};

use std::{cell::Cell, collections::HashMap, fmt::Debug, rc::Rc, sync::atomic::AtomicU64};

use self::iasm::{IASMCommandType, IASMLabel, IASMParam, IASMRegister};

/// A macro to log an error message with the file and line number.
#[macro_export]
macro_rules! error {
    ($self:ident, $message:expr) => {
        $self.error($message, file!(), line!());
    };
}

pub struct EmitRegWalker {
    had_error: Cell<Option<SharedString>>,
    panic_mode: Cell<bool>,
    function_compiler: Box<FunctionCompiler>,
    gloabls: HashMap<SharedString, usize>,
    global_types: HashMap<usize, (UValueType, bool)>,
    custom_structs: Rc<HashMap<SharedString, CustomStruct>>,
    text: IASMProgSplit,
}

impl Debug for EmitRegWalker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeChecker")
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! rvalue {
    ($self:expr, $func:expr, $line:expr) => {{
        let reg = $func;
        $self.rvalue(reg, $line)
    }};
}

impl EmitRegWalker {
    fn new(
        compiler: Box<FunctionCompiler>,
        custom_structs: HashMap<SharedString, CustomStruct>,
    ) -> EmitRegWalker {
        let c = EmitRegWalker {
            had_error: None.into(),
            panic_mode: false.into(),
            function_compiler: compiler,
            gloabls: HashMap::new(),
            global_types: HashMap::new(),
            custom_structs: custom_structs.into(),
            text: IASMProgSplit::new(),
        };
        c
    }

    #[instrument(skip_all, level = "trace")]
    fn visit(&mut self, node: ASTNode) -> bool {
        match node.clone() {
            ASTNode::Statement(s) => self.visit_statement(s),
            ASTNode::Expression(e) => {
                self.visit_expression(e);
                false
            }
            ASTNode::Declaration(d) => {
                self.visit_declaration(d);
                false
            }
            ASTNode::CallMain(c) => {
                self.visit(c.as_ref().clone());
                self.write_statement(IASMCommandType::Halt, vec![], 0);
                true
            }
            _ => false,
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_statement(&mut self, node: Statement) -> bool {
        static LOOP_NEST: AtomicU64 = AtomicU64::new(0);

        match node {
            Statement::Print(e, line) => self.print(*e, line),
            Statement::Block(nodes, _line) => {
                self.begin_scope();
                let mut has_return = false;
                for n in nodes {
                    let r = self.visit(n.clone());
                    has_return = has_return || r;

                    // if nprime was a break, return, or continue, we should break out of the loop
                    if matches!(
                        n,
                        ASTNode::Statement(
                            Statement::Break(_) | Statement::Return(_, _) | Statement::Continue(_)
                        )
                    ) {
                        break;
                    }
                }

                self.end_scope();
                has_return
            }
            Statement::If(e, s, o, line) => self.if_statement(*e, *s, o.map(|x| *x), line),
            Statement::While(e, s, line) => {
                LOOP_NEST.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                let r = self.while_statement(*e, *s, line);
                self.text.fill_break_continue();
                LOOP_NEST.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
                r
            }
            Statement::For(i, c, u, s, line) => {
                LOOP_NEST.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                let r = self.for_statement(i.map(|x| *x), c.map(|x| *x), u.map(|x| *x), *s, line);
                self.text.fill_break_continue();
                LOOP_NEST.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
                r
            }
            Statement::Break(line) => {
                if LOOP_NEST.load(std::sync::atomic::Ordering::SeqCst) == 0 {
                    error!(
                        self,
                        &format!("[line {line}] Cannot break outside of loop.")
                    );
                }
                self.write_comment("break".into());
                false
            }
            Statement::Continue(line) => {
                if LOOP_NEST.load(std::sync::atomic::Ordering::SeqCst) == 0 {
                    error!(
                        self,
                        &format!("[line {line}] Cannot continue outside of loop.")
                    );
                }
                self.write_comment("continue".into());
                false
            }
            Statement::Return(s, line) => {
                let loc = rvalue!(
                    self,
                    s.map(|x| self.visit_expression(*x).0)
                        .unwrap_or_else(|| IASMRegister::nil_reg()),
                    line
                );
                let t = loc.get_type();
                if t != self.function_compiler.return_type {
                    error!(
                        self,
                        &format!(
                            "[line {line}] Expected return type {:?}, got {:?}",
                            self.function_compiler.return_type, t
                        )
                    );
                }
                if t.is_aggregate() {
                    self.write_statement(
                        IASMCommandType::Return,
                        vec![
                            IASMParam::Register(loc),
                            IASMParam::Immediate(Value::UInteger(t.num_words() as u64)),
                        ],
                        line,
                    );
                } else {
                    self.write_statement(
                        IASMCommandType::Return,
                        vec![
                            IASMParam::Register(loc),
                            IASMParam::Immediate(Value::UInteger(0)),
                        ],
                        line,
                    );
                }

                true
            }
            Statement::Expression(e, _line) => {
                let _loc = self.visit_expression(*e);

                false
            }
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_expression(&mut self, node: Expression) -> (IASMRegister, usize) {
        match node {
           Expression::Literal(LiteralExpression{ value: v, line) => self.literal(v, line_no:  line}),
            Expression::StringLiteral(s, line) => self.string(s, line),
            Expression::Unary(t, e, line) => self.unary(t, *e, line),
            Expression::Deref(DerefExpression{ operand: e, line_no:  line}) => self.deref(*e, line),
            Expression::Ref(e, line) => self.addr_of(*e, line),
           Expression::Index(IndexExpression{ array: e, i, line) => self.index(*e, index:  *i, line_no:  line}),
           Expression::Binary(BinaryExpression{ left: l, t, r, line) => self.binary(*l, operator:  t, right:  *r, line_no:  line}),
           Expression::Ternary(TernaryExpression{ condition: c, t, f, line) => self.ternary(*c, then_branch:  *t, else_branch:  *f, line_no:  line}),
            Expression::Variable(t, line) => self.variable(t, line),
           Expression::Assign(AssignExpression{ variable: l, r, line) => self.assign(*l, value:  *r, line_no:  line}),
           Expression::Logical(LogicalExpression{ left: l, t, r, line) => self.logical(*l, operator:  t, right:  *r, line_no:  line}),
           Expression::Call(CallExpression{ callee: c, a, line) => self.call(*c, arguments:  a, line_no:  line}),
           Expression::Dot(DotExpression{ object: e, t, line) => self.dot(*e, field:  t, line_no:  line}),
            Expression::Function(f, line) => self.function(f, line),
            Expression::Cast(e, t, line) => self.cast(*e, t, line),
            Expression::StructInitializer(t, v, line) => self.struct_literal(t, v, line),
            _ => (IASMRegister::nil_reg(), 0),
        }
    }

    #[instrument(skip_all, level = "trace")]
    fn visit_declaration(&mut self, node: Declaration) {
        println!("push count: {}, local count: {}", self.function_compiler.push_count, self.function_compiler.local_count);
        let r = match node.clone() {
            Declaration::Var(v, line) => self.var_declaration(v, line),
            Declaration::Function(f, line) => self.fun_declaration(f, line),
            Declaration::Array(a, line) => self.array_declaration(a, line),
        };

        println!("{:?}\npush count: {}, local count: {}", ASTNode::Declaration(node), self.function_compiler.push_count, self.function_compiler.local_count);
        r
    }

    fn begin_scope(&mut self) {
        self.function_compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let _line = 0;
        self.function_compiler.scope_depth -= 1;
        while !self.function_compiler.locals.is_empty() {
            let last_local = self.last_local();
            if self
                .function_compiler
                .locals
                .get(&last_local)
                .unwrap()
                .depth
                <= self.function_compiler.scope_depth
            {
                break;
            }

            if self
                .function_compiler
                .locals
                .get(&last_local)
                .unwrap()
                .captured
                && self
                    .function_compiler
                    .locals
                    .get(&last_local)
                    .unwrap()
                    .mutable
            {
                error!(self, "Cannot capture mutable variables.");
            }

            // if its an array, we need to pop the array off the stack
            if let ValueType::Array(t, n) = self
                .function_compiler
                .locals
                .get(&last_local)
                .unwrap()
                .local_type
                .as_ref()
            {
                let words = t.num_words();
                for _ in 0..*n {
                    self.function_compiler.local_count -= words;
                }
            }
            self.function_compiler.local_count -= self
                .function_compiler
                .locals
                .get(&last_local)
                .unwrap()
                .local_type
                .num_words();
            self.pop_off_stack(
                self.function_compiler.locals.get(&last_local).unwrap().local_type,
                _line,
            );


            self.function_compiler.locals.remove(&last_local);
        }
    }

    fn define_variable(&mut self, _global_id: u32, local_type: UValueType) {
        if self.function_compiler.scope_depth > 0 {
            self.mark_initialized();
            let last_local = self.last_local();
            self.function_compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = local_type;
            if !(local_type.is_aggregate() || local_type.is_array()) {
                self.function_compiler.local_count += 1;
            }
        }
        // if local_type == ValueTypeK::AnyFunction.intern() {
        //     self.compiler
        //         .func
        //         .chunk
        //         .write(Opcode::CopyClosure.into(), 0);
        // }
    }

    fn last_local(&mut self) -> usize {
        self.function_compiler
            .locals
            .iter()
            .map(|l| *l.0)
            .max()
            .unwrap()
    }

    fn mark_initialized(&mut self) {
        if self.function_compiler.scope_depth == 0 {
            return;
        }
        let last_local = self
            .function_compiler
            .locals
            .iter()
            .map(|l| *l.0)
            .max()
            .unwrap();
        self.function_compiler
            .locals
            .get_mut(&last_local)
            .unwrap()
            .depth = self.function_compiler.scope_depth;
    }

    fn error(&self, message: &str, from: &str, line: u32) {
        self.error_at(true, message, from, line);
    }

    fn error_at(&self, _prev: bool, message: &str, from: &str, line: u32) {
        if self.panic_mode.get() {
            return;
        }
        self.panic_mode.set(true);
        let err_msg = format!("[compiler] error at {}:{} - {}", from, line, message);
        tracing::error!("[compiler] error at {}:{} - {}", from, line, message);

        self.had_error.set(Some(err_msg.into()));
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

    fn write_statement(&mut self, c: IASMCommandType, p: Vec<IASMParam>, line: usize) {
        self.text.write_statement(c, p, line);
    }

    fn write_label(&mut self, label: &IASMLabel) {
        self.text.write_label(label.clone());
    }

    fn write_constant(&mut self, value: Value, t: UValueType, line: usize) -> IASMRegister {
        self.text.write_constant(value, t, line)
    }

    fn write_comment(&mut self, comment: SharedString) {
        self.text.write_comment(comment)
    }

    fn write_store(
        &mut self,
        to_loc_reg: IASMRegister,
        from_reg: IASMRegister,
        t: UValueType,
        line: usize,
    ) {
        if t.is_aggregate() {
            self.write_statement(
                IASMCommandType::MemCpy,
                vec![
                    IASMParam::Register(to_loc_reg),
                    IASMParam::Register(from_reg),
                    IASMParam::Immediate(Value::UInteger(t.num_words() as u64)),
                ],
                line,
            );
        } else {
            self.write_statement(
                IASMCommandType::Store,
                vec![
                    IASMParam::Register(to_loc_reg),
                    IASMParam::Register(from_reg),
                ],
                line,
            );
        }
    }

    fn write_load(
        &mut self,
        to_reg: IASMRegister,
        from_loc_reg: IASMRegister,
        t: UValueType,
        line: usize,
    ) {
        if t.is_aggregate() {
            self.write_statement(
                IASMCommandType::Move,
                vec![
                    IASMParam::Register(to_reg),
                    IASMParam::Register(from_loc_reg),
                ],
                line,
            );
        } else {
            self.write_statement(
                IASMCommandType::Load,
                vec![
                    IASMParam::Register(to_reg),
                    IASMParam::Register(from_loc_reg),
                ],
                line,
            );
        }
    }

    fn make_room_on_stack_for(&mut self, t: UValueType, line: usize) {
        for _ in 0..t.num_words() {
            self.write_statement(
                IASMCommandType::Push,
                vec![IASMParam::Register(IASMRegister::nil_reg())],
                line,
            );
            self.function_compiler.push_count += 1;
        }
    }
    fn pop_off_stack(&mut self, t: UValueType, line: usize) {
        for _ in 0..t.num_words() {
            self.write_statement(
                IASMCommandType::Pop,
                vec![],
                line,
            );
        }
    }

    fn map_reg_type(
        &mut self,
        old_reg: IASMRegister,
        new_type: UValueType,
        line: usize,
    ) -> IASMRegister {
        let new_reg = IASMRegister::new(new_type);
        self.write_statement(
            IASMCommandType::Move,
            vec![IASMParam::Register(new_reg), IASMParam::Register(old_reg)],
            line,
        );
        new_reg
    }

    fn rvalue(&mut self, reg: IASMRegister, line: usize) -> IASMRegister {
        let t = reg.get_type();
        if let ValueType::LValue(inner, _a) = t.as_ref() {
            if inner.is_aggregate() {
                return self.map_reg_type(reg, *inner, line);
            } else {
                let derefed = iasm::IASMRegister::new(*inner);

                self.write_statement(
                    IASMCommandType::Load,
                    vec![IASMParam::Register(derefed), IASMParam::Register(reg)],
                    line,
                );
                return derefed;
            }
        } else {
            reg
        }
    }
}

#[derive(Debug, Clone)]
struct Local {
    name: SharedString,
    depth: i32,
    mutable: bool,
    assigned: bool,
    captured: bool,
    local_type: UValueType,
}

struct FunctionCompiler {
    return_type: UValueType,

    locals: HashMap<usize, Local>,
    local_count: usize,
    push_count: usize,
    scope_depth: i32,

    enclosing: Option<Box<FunctionCompiler>>,
}

impl Default for FunctionCompiler {
    fn default() -> Self {
        FunctionCompiler {
            locals: HashMap::new(),
            return_type: ValueType::Nil.intern(),
            local_count: 0,
            push_count: 3,
            scope_depth: 0,
            enclosing: None,
        }
    }
}

impl FunctionCompiler {
    fn new(current: Option<FunctionCompiler>, _func_name: &str) -> FunctionCompiler {
        let mut c = FunctionCompiler {
            locals: HashMap::new(),
            return_type: ValueType::Nil.intern(),
            local_count: 0,
            scope_depth: 0,
            enclosing: current.map(Box::new),
            push_count: 3,
        };
        c.locals.insert(
            c.local_count,
            Local {
                name: "this".into(),
                depth: 0,
                mutable: false,
                assigned: false,
                captured: false,
                local_type: ValueType::Nil.intern(),
            },
        );
        // return addr and value loc
        c.local_count = 3;
        c
    }

    fn recover_enclosing(self) -> Option<Box<FunctionCompiler>> {
        self.enclosing
    }
}

impl EmitRegWalker {
    #[instrument(skip_all, level = "info")]
    pub fn compile(parsed: ParseResult) -> Result<IASMProg, SharedString> {
        debug!("ast {:?}\n{:#?}", parsed.ast, parsed.custom_structs);

        let mut comiler = EmitRegWalker::new(
            Box::new(FunctionCompiler::new(None, "script")),
            parsed.custom_structs.clone(),
        );
        comiler.text.start_function("script");

        for node in parsed.ast {
        comiler.visit(node);
        }
        let had_err = comiler.had_error.into_inner();

        if had_err.is_some() {
            tracing::error!("Compiling failed.");

            return Err(had_err.unwrap());
        } else {
            tracing::info!("Compiling succeeded.");
            Ok(comiler.text.merge())
        }
    }
}
