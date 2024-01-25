mod parse_table;

use crate::value::object::{Function, Object};
use crate::value::pointer::Pointer;
use crate::value::value_type::ValueType;
use crate::value::Value::{self, Number};
use crate::value::{
    number,
    value_type::{ValueTypeK, ValueTypeSet},
};
use num_enum::FromPrimitive;
use std::array;
use std::cell::Cell;

use std::collections::HashMap;

use std::num::ParseIntError;

use std::rc::Rc;

use crate::{
    chunk::Opcode,
    scanner::{Scanner, Token, TokenType},
};

#[derive(Debug, PartialEq, PartialOrd, FromPrimitive, Copy, Clone)]
#[repr(u8)]
enum Precedence {
    #[default]
    None,
    Assignment, // =
    Ternary,    // ? :
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Cast,       // as
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match Precedence::try_from(*self as u8 + 1) {
            Ok(p) => p,
            Err(_) => Precedence::None,
        }
    }
}

type ParseFn = fn(&mut Parser, bool) -> ValueType;
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    left_type: Rc<ValueTypeSet>,
    precedence: Precedence,
}

struct Parser {
    current: Token,
    previous: Token,
    scanner: Scanner,
    had_error: Cell<bool>,
    panic_mode: Cell<bool>,
    last_pushed_type: ValueType,
    compiler: Box<Compiler>,
    parse_table: [ParseRule; 52],
    static_data_segment: Vec<Value>,
    num_globals: usize,
    gloabls: HashMap<String, usize>,
    global_types: HashMap<usize, ValueType>,
}

impl Parser {
    fn new(
        scanner: Scanner,
        compiler: Box<Compiler>,
        native_functions: &mut HashMap<String, (usize, ValueType)>,
    ) -> Parser {
        let mut p = Parser {
            current: Token {
                token_type: TokenType::Error,
                lexeme: "".to_string(),
                line: 0,
            },
            previous: Token {
                token_type: TokenType::Error,
                lexeme: "".to_string(),
                line: 0,
            },
            scanner: scanner,
            had_error: false.into(),
            panic_mode: false.into(),
            compiler: compiler,
            last_pushed_type: ValueTypeK::Nil.intern(),
            parse_table: array::from_fn(|_| ParseRule {
                prefix: None,
                infix: None,
                left_type: Rc::new(ValueTypeSet::new(vec![])),
                precedence: Precedence::None,
            }),
            static_data_segment: Vec::new(),
            gloabls: HashMap::new(),
            global_types: HashMap::new(),
            num_globals: native_functions.len(),
        };
        for (name, (id, t)) in native_functions.iter() {
            p.gloabls.insert(name.clone(), *id);
            p.global_types.insert(*id, *t);
        }
        p.init_parse_table();
        p
    }
    fn advance(&mut self) {
        self.previous = std::mem::take(&mut self.current);

        loop {
            self.current = self.scanner.scan_token();
            if self.current.token_type != TokenType::Error {
                break;
            }

            self.error_at_current(&self.current.lexeme.clone());
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.current.token_type == token_type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.current.token_type != token_type {
            return false;
        }
        self.advance();
        true
    }

    fn expression(&mut self) -> ValueType {
        return self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
            self.var_declaration(true);
        } else if self.match_token(TokenType::Const) {
            if self.compiler.scope_depth == 0 {
                self.warn("Const has no effect in global scope")
            }
            self.var_declaration(false);
        } else if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode.get() {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode.set(false);

        while self.current.token_type != TokenType::EOF {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }

            match self.current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::Semicolon) {
        } else if self.match_token(TokenType::Break) {
            self.consume(TokenType::Semicolon, "Expect ';' after break.");
            let line = self.previous.line;
            self.compiler.func.chunk.write(Opcode::Break.into(), line);
            self.compiler.func.chunk.write(0xff, line);
            self.compiler.func.chunk.write(0xff, line);
        } else if self.match_token(TokenType::Continue) {
            self.consume(TokenType::Semicolon, "Expect ';' after continue.");
            let line = self.previous.line;
            self.compiler
                .func
                .chunk
                .write(Opcode::Continue.into(), line);
            self.compiler.func.chunk.write(0xff, line);
            self.compiler.func.chunk.write(0xff, line);
        } else if self.match_token(TokenType::Return) {
            if self.compiler.function_type == FunctionType::Script {
                self.error("Cannot return from top-level code.");
            }

            let rt = self.compiler.func.return_type;
            if self.match_token(TokenType::Semicolon) {
                if rt != ValueTypeK::Nil.intern() {
                    self.error(&format!(
                        "Type mismatch, expected {:?} got {:?}",
                        rt,
                        ValueTypeK::Nil.intern()
                    ));
                }
                self.emit_return();
            } else {
                let et = self.expression();
                if et != rt {
                    self.error(&format!("Type mismatch, expected {:?} got {:?}", rt, et));
                }
                self.consume(TokenType::Semicolon, "Expect ';' after return value.");
                let line = self.previous.line;
                self.compiler.func.chunk.write(Opcode::Return.into(), line);
            }
        } else {
            self.expression_statement();
        }
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let line = self.previous.line;
        self.compiler.scope_depth -= 1;
        while self.compiler.locals.len() > 0 {
            let last_local = self.last_local();
            if self.compiler.locals.get(&last_local).unwrap().depth <= self.compiler.scope_depth {
                break;
            }

            if self.compiler.locals.get(&last_local).unwrap().captured {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CloseUpvalue.into(), line);
            } else {
                self.compiler.func.chunk.write(Opcode::Pop.into(), line);
            }

            // if its an array, we need to pop the array off the stack
            if let ValueTypeK::Array(_, n) =
                self.compiler.locals.get(&last_local).unwrap().local_type
            {
                for _ in 0..*n {
                    self.compiler.func.chunk.write(Opcode::Pop.into(), line);
                    self.compiler.local_count -= 1;
                }
            }

            self.compiler.locals.remove(&last_local);
            self.compiler.local_count -= 1;
        }
    }

    fn block(&mut self) {
        while self.current.token_type != (TokenType::RightBrace)
            && self.current.token_type != (TokenType::EOF)
        {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn print_statement(&mut self) {
        let line = self.previous.line;
        let t = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        match t {
            ValueTypeK::Float => {
                self.compiler.func.chunk.write(Opcode::PrintFloat.into(), line);
            }
            ValueTypeK::Integer => {
                self.compiler.func.chunk.write(Opcode::PrintInt.into(), line);
            }
            ValueTypeK::Bool => {
                self.compiler.func.chunk.write(Opcode::PrintBool.into(), line);
            }
            ValueTypeK::String => {
                self.compiler.func.chunk.write(Opcode::PrintString.into(), line);
            }
            ValueTypeK::Nil => {
                self.compiler.func.chunk.write(Opcode::PrintNil.into(), line);
            }
            ValueTypeK::Pointer(_) => {
                self.compiler.func.chunk.write(Opcode::PrintPointer.into(), line);
            }
            ValueTypeK::Array(_, _) => {
                self.compiler.func.chunk.write(Opcode::PrintPointer.into(), line);
            }
            ValueTypeK::Function(_) => {
                self.compiler.func.chunk.write(Opcode::PrintPointer.into(), line);
            }
            _ => {
                self.error(format!("Cannot print this type. {:?}", t).as_str());
            }
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        let line = self.previous.line;
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(Opcode::JumpIfFalse.into());
        let mut line = self.previous.line;
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);
        self.statement();
        let else_jump = self.emit_jump(Opcode::Jump.into());
        self.patch_jump(then_jump);
        line = self.previous.line;
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);

        if self.match_token(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.compiler.func.chunk.code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(Opcode::JumpIfFalse.into());
        let mut line = self.previous.line;
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        line = self.previous.line;
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);
        self.fill_break_continue(loop_start);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.match_token(TokenType::Semicolon) {
            // No initializer.
        } else if self.match_token(TokenType::Var) {
            self.var_declaration(true);
        } else {
            self.expression_statement();
        }

        let loop_start = self.compiler.func.chunk.code.len();
        let mut exit_jump = None;
        let mut line = self.previous.line;
        if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exit_jump = Some(self.emit_jump(Opcode::JumpIfFalse.into()));
            self.compiler.func.chunk.write(Opcode::Pop.into(), line); // Condition.
        }
        line = self.previous.line;
        let mut increment_start = loop_start;
        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.emit_jump(Opcode::Jump.into());
            increment_start = self.compiler.func.chunk.code.len();
            self.expression();
            self.compiler.func.chunk.write(Opcode::Pop.into(), line);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(increment_start);
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.compiler.func.chunk.write(Opcode::Pop.into(), line); // condition.
        }
        self.fill_break_continue(increment_start);

        self.end_scope();
    }

    fn cast(&mut self, _can_assign: bool) -> ValueType {
        // cast(value, type)
        let line = self.previous.line;
        self.consume(TokenType::LeftParen, "Expect '(' after 'cast'.");
        let t = self.expression();
        self.consume(TokenType::Comma, "Expect ',' after value.");
        let cast_type = self.parse_complex_type();
        self.consume(TokenType::RightParen, "Expect ')' after type.");

        match (t, cast_type) {
            (x, y) if x == y => x,
            (ValueTypeK::Integer, ValueTypeK::Float) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastIntToFloat.into(), line);
                ValueTypeK::Float.intern()
            }
            (ValueTypeK::Float, ValueTypeK::Integer) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastFloatToInt.into(), line);
                ValueTypeK::Integer.intern()
            }
            (ValueTypeK::Integer, ValueTypeK::Bool) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastIntToBool.into(), line);
                ValueTypeK::Bool.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::Integer) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastBoolToInt.into(), line);
                ValueTypeK::Integer.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::Float) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastBoolToFloat.into(), line);
                ValueTypeK::Float.intern()
            }
            (ValueTypeK::Integer, ValueTypeK::String) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastIntToString.into(), line);
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Float, ValueTypeK::String) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastFloatToString.into(), line);
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Bool, ValueTypeK::String) => {
                self.compiler
                    .func
                    .chunk
                    .write(Opcode::CastBoolToString.into(), line);
                ValueTypeK::String.intern()
            }
            (ValueTypeK::Array(x, _), ValueTypeK::Pointer(y)) if x == y => {
                ValueTypeK::Pointer(x).intern()
            }
            (ValueTypeK::Pointer(_), ValueTypeK::Pointer(_)) => cast_type,
            _ => {
                self.error(&format!("Cannot cast {:?} to {:?}", t, cast_type));
                ValueTypeK::Err.intern()
            }
        }
    }

    fn parse_complex_type(&mut self) -> ValueType {
        let parset_type = 'a: {
            if self.match_token(TokenType::LeftParen) {
                let t = self.parse_complex_type();
                self.consume(TokenType::RightParen, "Expect ')' after type.");
                break 'a t;
            }

            if self.match_token(TokenType::SimpleType) {
                break 'a self.previous.lexeme.clone().into();
            }

            // if self.match_token(TokenType::LeftBracket) {
            //     let n = self.parse_index();
            //     self.consume(TokenType::RightBracket, "Expect ']' after index.");
            //     let t = self.parse_complex_type();
            //     return ValueType::Array(Box::new(t), n);
            // }

            if self.match_token(TokenType::Fun) {
                let mut param_types = Vec::new();
                self.consume(TokenType::LeftParen, "Expect '(' after 'fun'.");
                let mut _arg_count = 0;
                if self.current.token_type != TokenType::RightParen {
                    loop {
                        let p_type = self.parse_complex_type();
                        param_types.push(p_type);
                        _arg_count += 1;
                        if !self.match_token(TokenType::Comma) {
                            break;
                        }
                    }
                }
                self.consume(TokenType::RightParen, "Expect ')' after arguments.");
                let mut return_type = ValueTypeK::Nil.intern();
                if self.match_token(TokenType::RightArrow) {
                    return_type = self.parse_complex_type();
                }
                param_types.push(return_type);
                break 'a ValueTypeK::Function(param_types.as_slice().into()).intern();
            }

            // self.error("Expect type.");
            ValueTypeK::Err.intern()
        };
        if self.match_token(TokenType::Star) {
            return ValueTypeK::Pointer(parset_type).intern();
        }
        parset_type
    }

    fn fill_break_continue(&mut self, loop_start: usize) {
        let mut i = self.compiler.func.chunk.code.len() - 3;
        while i > loop_start {
            let op = self.compiler.func.chunk.code[i].0;
            if op == Opcode::Break.into()
                && self.compiler.func.chunk.code[i + 1].0 == 255
                && self.compiler.func.chunk.code[i + 2].0 == 255
            {
                self.compiler.func.chunk.code[i].0 = Opcode::Jump.into();
                self.patch_jump(i + 1);
            } else if op == Opcode::Continue.into()
                && self.compiler.func.chunk.code[i + 1].0 == 255
                && self.compiler.func.chunk.code[i + 2].0 == 255
            {
                let offset = i - loop_start + 3;
                self.compiler.func.chunk.code[i].0 = Opcode::Loop.into();
                self.compiler.func.chunk.code[i + 1].0 = ((offset >> 8) & 0xff) as u8;
                self.compiler.func.chunk.code[i + 2].0 = (offset & 0xff) as u8;
            }
            i -= 1;
        }
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let line = self.previous.line;
        self.compiler.func.chunk.write(Opcode::Loop.into(), line);
        let offset = self.compiler.func.chunk.code.len() - loop_start + 2;
        if offset > std::u16::MAX as usize {
            self.error("Loop body too large.");
        }

        self.compiler
            .func
            .chunk
            .write(((offset >> 8) & 0xff) as u8, line);
        self.compiler.func.chunk.write((offset & 0xff) as u8, line);
    }

    fn and_(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        let end_jump = self.emit_jump(Opcode::JumpIfFalse.into());
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
        ValueTypeK::Bool.intern()
    }

    fn or_(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        let else_jump = self.emit_jump(Opcode::JumpIfFalse.into());
        let end_jump = self.emit_jump(Opcode::Jump.into());
        self.patch_jump(else_jump);
        self.compiler.func.chunk.write(Opcode::Pop.into(), line);
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
        ValueTypeK::Bool.intern()
    }

    fn emit_jump(&mut self, instruction: u8) -> usize {
        let line = self.previous.line;
        self.compiler.func.chunk.write(instruction, line);
        self.compiler.func.chunk.write(0xff, line);
        self.compiler.func.chunk.write(0xff, line);
        self.compiler.func.chunk.code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.compiler.func.chunk.code.len() - offset - 2;
        if jump > std::u16::MAX as usize {
            self.error("Too much code to jump over.");
        }

        self.compiler.func.chunk.code[offset].0 = ((jump >> 8) & 0xff) as u8;
        self.compiler.func.chunk.code[offset + 1].0 = (jump & 0xff) as u8;
    }

    fn array_declaration(&mut self, global_id: i64, var_type: ValueType) {
        match self.parse_literal_index() {
            Ok(n) => {
                self.consume(TokenType::RightBracket, "brace");
                self.mark_initialized();
                if global_id == -1 {
                    let last_local = self.last_local();
                    self.compiler
                        .locals
                        .get_mut(&last_local)
                        .unwrap()
                        .local_type = ValueTypeK::Array(var_type, n as usize).intern();
                } else {
                    self.global_types.insert(
                        global_id as usize,
                        ValueTypeK::Array(var_type, n as usize).intern(),
                    );
                }

                self.define_array(global_id, Some(n));
            }
            Err(e) => self.error(&e.to_string()),
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );
    }

    fn define_array(&mut self, global_id: i64, mut size: Option<u32>) {
        let mut line = self.previous.line;
        self.match_token(TokenType::Equal);

        if global_id == -1 {
            self.compiler
                .func
                .chunk
                .write(Opcode::DefineStackArray as u8, line);

            let last_local = self.last_local();
            let mut elem_type = self.compiler.locals.get(&last_local).unwrap().local_type;

            if self.match_token(TokenType::LeftBrace) {
                let mut arg_count = 0;
                loop {
                    if self.current.token_type != TokenType::RightBrace {
                        let pt = self.expression();
                        if elem_type == ValueTypeK::Undef.intern() {
                            elem_type = pt;
                        }
                        self.compiler.local_count += 1;

                        if pt != elem_type {
                            self.error(
                                format!(
                                    "Element must have type {:?}, got {:?} instead",
                                    elem_type, pt
                                )
                                .as_str(),
                            );
                        }
                        if arg_count == 255 {
                            self.error("Cannot have more than 255 arguments.");
                        }
                        arg_count += 1;
                        if !self.match_token(TokenType::Comma) {
                            break;
                        }
                    }
                }

                self.consume(TokenType::RightBrace, "Expect '}' after arguments.");
                if let &ValueTypeK::Array(_, n) =
                    self.global_types.get(&(global_id as usize)).unwrap()
                {
                    if arg_count != *n {
                        self.error(&format!(
                            "Args in initializer don't match, expected {} got {}",
                            n, arg_count
                        ));
                    }
                }
                size = Some(arg_count as u32);

                self.compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = ValueTypeK::Array(elem_type, arg_count as usize).intern();
            } else {
                let n = size.expect("size is none");
                for _ in 0..n {
                    self.compiler.func.chunk.write(Opcode::Nil.into(), line);
                    self.compiler.local_count += 1;
                }
            }
        } else {
            // global array
            let mut elem_type = *self.global_types.get(&(global_id as usize)).unwrap();

            if self.match_token(TokenType::LeftBrace) {
                let mut arg_count = 0;
                loop {
                    if self.current.token_type != TokenType::RightBrace {
                        let pt = self.expression();
                        if elem_type == ValueTypeK::Undef.intern() {
                            elem_type = pt;
                        }

                        if pt != elem_type {
                            self.error(
                                format!(
                                    "Element must have type {:?}, got {:?} instead",
                                    elem_type, pt
                                )
                                .as_str(),
                            );
                        }
                        if arg_count == 255 {
                            self.error("Cannot have more than 255 arguments.");
                        }
                        arg_count += 1;
                        if !self.match_token(TokenType::Comma) {
                            break;
                        }
                    }
                }

                self.consume(TokenType::RightBrace, "Expect '}' after arguments.");
                if let &ValueTypeK::Array(_, n) =
                    self.global_types.get(&(global_id as usize)).unwrap()
                {
                    if arg_count != *n {
                        self.error(&format!(
                            "Args in initializer don't match, expected {} got {}",
                            n, arg_count
                        ));
                    }
                }
                size = Some(arg_count as u32);

                self.global_types.insert(
                    global_id as usize,
                    ValueTypeK::Array(elem_type, arg_count).intern(),
                );
            } else {
                let n = size.expect("size is none");
                for _ in 0..n {
                    self.compiler.func.chunk.write(Opcode::Nil.into(), line);
                }
            }

            *self.static_data_segment.last_mut().unwrap() =
                Value::Pointer(Pointer::Static(self.num_globals));
            // self.num_globals += 1;

            for _ in 0..size.unwrap() {
                self.static_data_segment.push(Value::Nil);
                self.num_globals += 1;
            }

            self.compiler.func.chunk.write_pool_opcode(
                Opcode::DefineGlobalArray,
                global_id as u32,
                line,
            );
            self.compiler.func.chunk.write(size.unwrap() as u8, line);
        }
    }

    fn var_declaration(&mut self, mutable: bool) {
        let line = self.previous.line;

        let global_id = self.parse_variable("Expect variable name.", mutable);

        let mut var_type: ValueType = ValueTypeK::Undef.intern();
        if self.match_token(TokenType::Colon) {
            var_type = self.parse_complex_type();
            if self.match_token(TokenType::LeftBracket) {
                self.array_declaration(global_id, var_type);
                return;
            }
        }
        if self.match_token(TokenType::Equal) {
            if self.current.token_type == TokenType::LeftBrace {
                self.define_array(global_id, None);
                return;
            }
            let expr_type = self.expression();

            if var_type == ValueTypeK::Undef.intern() {
                var_type = expr_type;
            }
            if expr_type != var_type {
                self.error(&format!(
                    "Type mismatch, expected {:?} got {:?}",
                    var_type, expr_type
                ));
            }
            if global_id == -1 {
                let last_local = self.last_local();
                self.compiler.locals.get_mut(&last_local).unwrap().assigned = true;
                self.compiler
                    .locals
                    .get_mut(&last_local)
                    .unwrap()
                    .local_type = var_type;
            } else {
                self.gloabls
                    .insert(self.previous.lexeme.clone(), global_id as usize);
                self.global_types.insert(global_id as usize, var_type);
            }
        } else {
            self.compiler.func.chunk.write(Opcode::Nil.into(), line);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global_id as u32, var_type);
    }

    fn define_variable(&mut self, global_id: u32, local_type: ValueType) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            let last_local = self.last_local();
            self.compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = local_type;
            return;
        }

        self.compiler
            .func
            .chunk
            .define_global(global_id, self.previous.line);
    }

    fn last_local(&mut self) -> usize {
        self.compiler.locals.iter().map(|l| *l.0).max().unwrap()
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        let last_local = self.compiler.locals.iter().map(|l| *l.0).max().unwrap();
        self.compiler.locals.get_mut(&last_local).unwrap().depth = self.compiler.scope_depth;
    }

    fn parse_variable(&mut self, error_message: &str, mutable: bool) -> i64 {
        self.consume(TokenType::Identifier, error_message);
        self.declare_variable(mutable);
        if self.compiler.scope_depth > 0 {
            let last_local = self.last_local();
            self.compiler.locals.get_mut(&last_local).unwrap().depth = self.compiler.scope_depth;
            return -1;
        }
        let name = self.previous.lexeme.clone();
        self.gloabls.insert(name.clone(), self.num_globals);
        self.global_types
            .insert(self.num_globals, ValueTypeK::Undef.intern());
        self.static_data_segment.push(Value::Nil);
        self.num_globals += 1;
        return *self.gloabls.get(&name).unwrap() as i64;
    }

    fn declare_variable(&mut self, mutable: bool) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous.lexeme.clone();
        for i in self.compiler.locals.keys() {
            let local = &self.compiler.locals.get(&i).unwrap();
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }

            if name == local.name.lexeme {
                self.error("Already variable with this name in this scope.");
            }
        }

        self.add_local(self.previous.clone(), mutable);
    }

    fn fun_declaration(&mut self) {
        let global_id = self.parse_variable("Expect function name.", false);
        if self.compiler.scope_depth > 0 {
            // self.compiler.locals.last_mut().unwrap().depth = self.compiler.scope_depth;
            let last_local = self.last_local();
            self.compiler.locals.get_mut(&last_local).unwrap().depth = self.compiler.scope_depth;
        }
        let name = self.previous.lexeme.clone();
        let t = self.function(FunctionType::Function, &name);

        if global_id == -1 {
            let last_local = self.last_local();
            self.compiler
                .locals
                .get_mut(&last_local)
                .unwrap()
                .local_type = t;
        } else {
            self.gloabls.insert(name.clone(), global_id as usize);
            self.global_types.insert(global_id as usize, t);
        }

        self.define_variable(global_id as u32, t);
    }

    fn lambda(&mut self, _can_assign: bool) -> ValueType {
        static mut ANON_ID: u32 = 0;
        let my_type = self.function(
            FunctionType::Function,
            format!("anon{}", unsafe { ANON_ID }).as_str(),
        );
        unsafe {
            ANON_ID += 1;
        };
        my_type
    }

    fn function(&mut self, function_type: FunctionType, func_name: &str) -> ValueType {
        let c = Compiler::new(
            function_type,
            Some(std::mem::take(&mut self.compiler)),
            func_name,
        );
        self.compiler = Box::new(c);
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        let mut param_types: Vec<ValueType> = Vec::new();
        if self.current.token_type != TokenType::RightParen {
            loop {
                self.compiler.func.arity += 1;
                if self.compiler.func.arity > 255 {
                    self.error_at_current("Cannot have more than 255 parameters.");
                }

                let constant = self.parse_variable("Expect parameter name.", false);
                self.consume(TokenType::Colon, "Expect ':' after parameter name.");

                let p_type = self.parse_complex_type();
                param_types.push(p_type);

                self.define_variable(constant as u32, p_type);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        let mut return_type = ValueTypeK::Nil.intern();
        if self.match_token(TokenType::RightArrow) {
            return_type = self.parse_complex_type();
        }
        param_types.push(return_type);
        self.compiler.func.return_type = return_type.into();
        let ft = ValueTypeK::Function(param_types.clone().as_slice().into()).intern();
        self.compiler.locals.insert(
            0,
            Local {
                name: Token {
                    token_type: TokenType::Identifier,
                    lexeme: func_name.to_string(),
                    line: 0,
                },
                depth: 0,
                mutable: false,
                assigned: false,
                captured: false,
                local_type: ft,
            },
        );

        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();
        self.emit_return();

        let c = std::mem::take(&mut self.compiler);
        let name = c.func.name.clone();
        if self.had_error.get() {
            c.func.chunk.dissassemble(&name);
        }

        let (rfunc, maybe_enclosing, upvalues) = c.recover_values();
        let Some(enclosing) = maybe_enclosing else {
            self.error("Cannot return from top-level code.");
            return ValueTypeK::Err.intern();
        };
        let func = Rc::new(rfunc);

        self.compiler = enclosing;

        if func.upvalue_count > 0 {
            // let f_id = self
            //     .compiler
            //     .func
            //     .chunk
            //     .add_constant(Value::Object(Object::Function(func.clone())));
            self.static_data_segment
                .push(Value::Object(Object::Function(func.clone())));
            self.num_globals += 1;
            let f_id = self.num_globals - 1;
            self.compiler.func.chunk.write_pool_opcode(
                Opcode::Closure,
                f_id as u32,
                self.previous.line,
            );

            for i in 0..func.upvalue_count {
                self.compiler
                    .func
                    .chunk
                    .write(upvalues[i].is_local as u8, self.previous.line);
                self.compiler
                    .func
                    .chunk
                    .write(upvalues[i].index as u8, self.previous.line);
            }
        } else {
            self.static_data_segment
                .push(Value::Object(Object::Function(func.clone())));
            self.num_globals += 1;
            let f_id = self.num_globals - 1;
            self.compiler.func.chunk.write_constant(
                Value::Pointer(crate::value::pointer::Pointer::Static(f_id)),
                self.previous.line,
            );
        }

        ft
    }

    fn emit_return(&mut self) {
        self.compiler
            .func
            .chunk
            .write(Opcode::Nil.into(), self.previous.line);
        self.compiler
            .func
            .chunk
            .write(Opcode::Return.into(), self.previous.line);
    }

    fn add_local(&mut self, name: Token, mutable: bool) {
        let local = Local {
            name: name,
            depth: -1,
            mutable: mutable,
            assigned: false,
            captured: false,
            local_type: ValueTypeK::Undef.intern(),
        };
        self.compiler
            .locals
            .insert(self.compiler.local_count, local);
        self.compiler.local_count += 1;
    }

    fn number(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        match self.previous.lexeme.parse::<i64>() {
            Ok(v) => {
                self.compiler
                    .func
                    .chunk
                    .write_constant(Number(number::Number::Integer(v)), line);
                ValueTypeK::Integer.intern()
            }
            Err(_) => {
                let value = self.previous.lexeme.parse::<f64>().unwrap();
                self.compiler
                    .func
                    .chunk
                    .write_constant(Number(number::Number::Float(value)), line);
                ValueTypeK::Float.intern()
            }
        }
    }

    fn grouping(&mut self, _can_assign: bool) -> ValueType {
        let t = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        t
    }

    fn ternary(&mut self, _can_assign: bool) -> ValueType {
        let t = self.parse_precedence(Precedence::Ternary.next());
        self.consume(TokenType::Colon, "Expect ':' after expression.");
        self.last_pushed_type = t;
        self.binary(_can_assign);
        return t;
    }

    fn unary(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        let operator_type = self.previous.token_type;
        // Compile the operand.
        let t = self.parse_precedence(Precedence::Unary);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus => self.compiler.func.chunk.write(Opcode::Negate.into(), line),
            TokenType::Bang => self.compiler.func.chunk.write(Opcode::Not.into(), line),
            _ => unreachable!(),
        }
        t
    }

    fn deref(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        // Compile the operand.
        let t = match self.parse_precedence(Precedence::Unary).decay() {
            &ValueTypeK::Pointer(pointee_type) => pointee_type,
            _ => {
                self.error("Expected pointer type");
                ValueTypeK::Err.intern()
            }
        };

        self.match_token(TokenType::Equal);
        if self.previous.token_type == TokenType::Equal {
            let expr_t = self.expression();
            if expr_t != t {
                self.error(&format!("Type mismatch, expected {:?} got {:?}", t, expr_t));
            }
            self.compiler
                .func
                .chunk
                .write(Opcode::DerefAssign.into(), line);
            return expr_t;
        } else {
            self.compiler
                .func
                .chunk
                .write(Opcode::DerefGet.into(), line);
            t
        }
    }

    fn parse_literal_index(&mut self) -> Result<u32, ParseIntError> {
        self.match_token(TokenType::Number);
        return self.previous.lexeme.parse::<u32>();
    }

    fn parse_expression_index(&mut self) {
        let t = self.expression();
        if t != ValueTypeK::Integer.intern() {
            self.error(&format!(
                "Index must be of type {:?}, got {:?}",
                ValueTypeK::Integer.intern(),
                t
            ));
        }
    }

    fn index(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;

        let pointer_type = self.last_pushed_type.decay();
        let &ValueTypeK::Pointer(pointee_type) = pointer_type else {
            self.error("Expected pointer type");
            return ValueTypeK::Err.intern();
        };

        self.parse_expression_index();
        self.consume(TokenType::RightBracket, "brace");
        // self.compiler
        //     .func
        //     .chunk
        //     .write_constant(Value::Number(number::Number::Integer(n.into())), line);
        self.compiler
            .func
            .chunk
            .write(Opcode::PointerAdd as u8, line);

        self.match_token(TokenType::Equal);
        if self.previous.token_type == TokenType::Equal {
            let t = self.expression();
            if pointee_type != t {
                self.error(&format!(
                    "Type mismatch, expected {:?} got {:?}",
                    pointee_type, t
                ));
            }
            self.compiler
                .func
                .chunk
                .write(Opcode::DerefAssign.into(), line);
        } else {
            self.compiler
                .func
                .chunk
                .write(Opcode::DerefGet.into(), line);
        }

        return pointee_type;
    }

    fn addr_of(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        if !self.match_token(TokenType::Identifier) {
            self.error("Expect identifier after '&'.");
        }
        // Compile the operand.
        let (var_id, _) = self.resolve_local(0);
        let _name = self.previous.lexeme.clone();
        // if !self.match_token(TokenType::LeftBracket) {
        if let Some(var_id) = var_id {
            self.compiler
                .func
                .chunk
                .write_constant(Number(number::Number::Integer(var_id as i64)), line);
            self.compiler
                .func
                .chunk
                .write(Opcode::RefLocal.into(), line);
            return ValueTypeK::Pointer(self.compiler.locals.get(&var_id).unwrap().local_type)
                .intern();
        } else if let Some(_up_id) = self.resolve_upvalue(0) {
            unimplemented!("upvalue addr")
            // self.compiler
            //     .func
            //     .chunk
            //     .write_constant(Number(number::Number::Integer(up_id as i64)), line);
            // self.compiler
            //     .func
            //     .chunk
            //     .write(Opcode::RefUpvalue.into(), line);
            // return ValueType::Pointer(Rc::new(
            //     self.compiler.upvalues[up_id].local_type,
            // ));
        } else {
            // global
            let glob_id = self.gloabls.get(&_name).unwrap();
            self.compiler.func.chunk.write_pool_opcode(
                Opcode::RefGlobal.into(),
                *glob_id as u32,
                line,
            );
            return ValueTypeK::Pointer(self.global_types.get(&glob_id).unwrap()).intern();
        }

        // } else {
        //     match self.parse_index() {
        //         Ok(n) => {
        //             if let Some(var_id) = var_id {
        //                 self.compiler.func.chunk.write_pool_opcode(
        //                     Opcode::GetLocal,
        //                     var_id as u32,
        //                     line,
        //                 )
        //             } else {
        //                 let glob_id = self
        //                     .compiler
        //                     .func
        //                     .chunk
        //                     .add_constant(Value::Object(Object::String(name.into())));
        //                 self.compiler
        //                     .func
        //                     .chunk
        //                     .write_pool_opcode(Opcode::GetGlobal, glob_id, line)
        //             }
        //             self.consume(TokenType::RightBracket, "brace");
        //             self.compiler
        //                 .func
        //                 .chunk
        //                 .write_constant(Value::Number(number::Number::Integer(n.into())), line);
        //             self.compiler.func.chunk.write(Opcode::Add as u8, line);
        //         }
        //     Err(e) => self.error(&e.to_string()),
        // }
    }

    fn binary(&mut self, _can_assign: bool) -> ValueType {
        let operator_type = self.previous.token_type;
        let line = self.previous.line;

        // Compile the right operand.
        let rule = &self.parse_table[operator_type as usize];
        let t = self.parse_precedence(rule.precedence.next());

        // Emit the operator instruction.
        match operator_type {
            TokenType::Plus => {
                match self.last_pushed_type.decay() {
                    ValueTypeK::Pointer(_)
                        if match t {
                            ValueTypeK::Integer => true,
                            _ => false,
                        } =>
                    {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::PointerAdd as u8, line);
                        return self.last_pushed_type;
                    }
                    x if x == t => {}
                    _ => {
                        self.error(&format!("Cannot add {:?} to {:?}", t, t));
                        return ValueTypeK::Err.intern();
                    }
                }

                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot add {:?} to {:?}",
                        self.last_pushed_type, t
                    ));
                }

                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::AddInt.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::AddFloat.into(), line);
                    }
                    ValueTypeK::String => {
                        self.compiler.func.chunk.write(Opcode::Concat.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot add {:?} to {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                return t;
            }
            TokenType::Minus => {
                match self.last_pushed_type.decay() {
                    ValueTypeK::Pointer(_)
                        if match t {
                            ValueTypeK::Integer => true,
                            _ => false,
                        } =>
                    {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::PointerSubtract as u8, line);
                        return self.last_pushed_type;
                    }
                    x if x == t => {}
                    _ => {
                        self.error(&format!("Cannot subtract {:?} from {:?}", t, t));
                        return ValueTypeK::Err.intern();
                    }
                }

                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot subtract {:?} from {:?}",
                        self.last_pushed_type, t
                    ));
                }

                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::SubtractInt.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::SubtractFloat.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot subtract {:?} from {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                return t;
            }
            TokenType::Star => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot multiply {:?} by {:?}",
                        self.last_pushed_type, t
                    ));
                }
                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::MultiplyInt.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::MultiplyFloat.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot multiply {:?} by {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                t
            }
            TokenType::Slash => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot divide {:?} by {:?}",
                        self.last_pushed_type, t
                    ));
                }
                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::DivideInt.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::DivideFloat.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot divide {:?} by {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                t
            }
            TokenType::Colon => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Both sides of ':' must have the same type, got {:?} and {:?}",
                        self.last_pushed_type, t
                    ));
                }
                self.compiler.func.chunk.write(Opcode::Ternary.into(), line);
                return t;
            }
            TokenType::EqualEqual => {
                if t != self.last_pushed_type {
                    self.warn(&format!(
                        "Comparison of different types ({:?} and {:?}) is always false, please cast to the same type",
                        self.last_pushed_type, t
                    ));
                    self.compiler.func.chunk.write(Opcode::Pop.into(), line);
                    self.compiler.func.chunk.write(Opcode::Pop.into(), line);
                    self.compiler.func.chunk.write(Opcode::False.into(), line);
                    return ValueTypeK::Bool.intern();
                }
                self.compiler.func.chunk.write(Opcode::Equal.into(), line);
                return ValueTypeK::Bool.intern();
            }
            TokenType::BangEqual => {
                if t != self.last_pushed_type {
                    self.warn(&format!(
                        "Comparison of different types ({:?} and {:?}) is always false (so != is always true), please cast to the same type",
                        self.last_pushed_type, t
                    ));
                    self.compiler.func.chunk.write(Opcode::Pop.into(), line);
                    self.compiler.func.chunk.write(Opcode::Pop.into(), line);
                    self.compiler.func.chunk.write(Opcode::True.into(), line);
                    return ValueTypeK::Bool.intern();
                }
                self.compiler.func.chunk.write(Opcode::Equal.into(), line);
                self.compiler.func.chunk.write(Opcode::Not.into(), line);
                return ValueTypeK::Bool.intern();
            }
            TokenType::Greater => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot compare {:?} and {:?}",
                        self.last_pushed_type, t
                    ));
                }
                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::GreaterInt.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::GreaterFloat.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot compare {:?} and {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            TokenType::GreaterEqual => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot compare {:?} and {:?}",
                        self.last_pushed_type, t
                    ));
                }
                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::LessInt.into(), line);
                        self.compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::LessFloat.into(), line);
                        self.compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot compare {:?} and {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            TokenType::Less => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot compare {:?} and {:?}",
                        self.last_pushed_type, t
                    ));
                }
                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::LessInt.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::LessFloat.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot compare {:?} and {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            TokenType::LessEqual => {
                if t != self.last_pushed_type {
                    self.error(&format!(
                        "Cannot compare {:?} and {:?}",
                        self.last_pushed_type, t
                    ));
                }
                match t {
                    ValueTypeK::Integer => {
                        self.compiler.func.chunk.write(Opcode::GreaterInt.into(), line);
                        self.compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    ValueTypeK::Float => {
                        self.compiler
                            .func
                            .chunk
                            .write(Opcode::GreaterFloat.into(), line);
                        self.compiler.func.chunk.write(Opcode::Not.into(), line);
                    }
                    _ => {
                        self.error(&format!(
                            "Cannot compare {:?} and {:?}",
                            self.last_pushed_type, t
                        ));
                    }
                }
                return ValueTypeK::Bool.intern();
            }
            _ => unreachable!(),
        }
    }

    fn call(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        if let ValueTypeK::Function(f) = self.last_pushed_type {
            let arg_count = self.argument_list(f);
            self.compiler
                .func
                .chunk
                .write_pool_opcode(Opcode::Call, arg_count.into(), line);

            let return_type = f.last().unwrap();
            return return_type;
        } else {
            self.error(
                format!(
                    "Can only call function types, got {:?}",
                    self.last_pushed_type
                )
                .as_str(),
            );
            return ValueTypeK::Err.intern();
        };
    }

    fn argument_list(&mut self, param_types: &[ValueType]) -> u8 {
        let mut arg_count = 0;
        if self.current.token_type != TokenType::RightParen {
            loop {
                let t = self.expression().decay();
                if t != param_types[arg_count] {
                    self.error(&format!(
                        "Type mismatch in function call argument {}, expected {:?} got {:?}",
                        arg_count, param_types[arg_count], t
                    ));
                }
                if arg_count == 255 {
                    self.error("Cannot have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        if arg_count != param_types.len() - 1 {
            self.error(&format!(
                "Wrong number of arguments, expected {} got {}",
                param_types.len() - 1,
                arg_count
            ));
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count as u8
    }

    fn literal(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        match self.previous.token_type {
            TokenType::False => {
                self.compiler.func.chunk.write(Opcode::False.into(), line);
                ValueTypeK::Bool.intern()
            }
            TokenType::Nil => {
                self.compiler.func.chunk.write(Opcode::Nil.into(), line);
                ValueTypeK::Nil.intern()
            }
            TokenType::True => {
                self.compiler.func.chunk.write(Opcode::True.into(), line);
                ValueTypeK::Bool.intern()
            }
            _ => unreachable!(),
        }
    }

    fn string(&mut self, _can_assign: bool) -> ValueType {
        let line = self.previous.line;
        let mut value = std::mem::take(&mut self.previous.lexeme);
        value = value.trim_matches('"').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        self.compiler
            .func
            .chunk
            .write_constant(Value::Object(Object::String(value.into())), line);
        return ValueTypeK::String.intern();
    }

    fn variable(&mut self, can_assign: bool) -> ValueType {
        let line = self.previous.line;
        let get_op: Opcode;
        let set_op: Opcode;
        let (arg, assignable) = self.resolve_local(0);
        let varid;
        if let Some(i) = arg {
            get_op = Opcode::GetLocal;
            set_op = Opcode::SetLocal;
            varid = i;
        } else if let Some(i) = self.resolve_upvalue(0) {
            get_op = Opcode::GetUpvalue;
            set_op = Opcode::SetUpvalue;
            varid = i;
        } else {
            let name = self.previous.lexeme.clone();
            let global_id = self.gloabls.get(&name);
            let Some(glob) = global_id else {
                self.error("Undefined variable.");
                return ValueTypeK::Err.intern();
            };
            varid = *glob;

            get_op = Opcode::GetGlobal;
            set_op = Opcode::SetGlobal;
        }

        if self.match_token(TokenType::Equal) && can_assign {
            if !assignable {
                self.error("Can't assign to const variable");
            }
            let assign_type = self.expression();
            if set_op == Opcode::SetLocal {
                if self.compiler.locals.get(&varid).expect("local").local_type
                    == ValueTypeK::Undef.intern()
                {
                    self.compiler
                        .locals
                        .get_mut(&varid)
                        .expect("local")
                        .local_type = assign_type;
                } else if assign_type != self.compiler.locals.get(&varid).expect("local").local_type
                {
                    self.error(format!("Cannot assign to variable of different type. Variable {} has type {:?} got {:?}", self.compiler.locals.get(&varid).unwrap().name.lexeme ,self.compiler.locals.get(&varid).unwrap().local_type, assign_type).as_str());
                    return ValueTypeK::Err.intern();
                }

                if self.compiler.locals.get(&varid).expect("local").mutable == false {
                    self.error("Cannot assign to const variable");
                    return ValueTypeK::Err.intern();
                }

                self.compiler
                    .locals
                    .get_mut(&varid)
                    .expect("local")
                    .assigned = true;
            }
            if set_op == Opcode::SetUpvalue {
                if self.compiler.upvalues[varid].upvalue_type == ValueTypeK::Undef.intern() {
                    self.compiler.upvalues[varid].upvalue_type = assign_type;
                } else if assign_type != self.compiler.upvalues[varid].upvalue_type {
                    self.error(format!("Cannot assign to captured variable of different type. Variable has type {:?} got {:?}", self.compiler.upvalues[varid].upvalue_type, assign_type).as_str());
                    return ValueTypeK::Err.intern();
                }
            }
            if set_op == Opcode::SetGlobal {
                let global_type = *self.global_types.get(&varid).unwrap();
                if global_type == ValueTypeK::Undef.intern() {
                    self.global_types.insert(varid, assign_type);
                } else if assign_type != global_type {
                    self.error(format!("Cannot assign to global variable of different type. Variable has type {:?} got {:?}", global_type, assign_type).as_str());
                    return ValueTypeK::Err.intern();
                }
            }

            self.compiler
                .func
                .chunk
                .write_pool_opcode(set_op, varid as u32, line);

            return assign_type;
        } else {
            self.compiler
                .func
                .chunk
                .write_pool_opcode(get_op, varid as u32, line);
            if get_op == Opcode::GetLocal {
                self.last_pushed_type = self.compiler.locals.get(&varid).expect("local").local_type;
                return self.last_pushed_type;
            }
            if get_op == Opcode::GetUpvalue {
                self.last_pushed_type = self.compiler.upvalues[varid].upvalue_type;
                return self.compiler.upvalues[varid].upvalue_type;
            }
            if get_op == Opcode::GetGlobal {
                self.last_pushed_type = *self.global_types.get(&varid).unwrap();
                return self.last_pushed_type;
            }
            return ValueTypeK::Err.intern();
        }
    }

    fn compiler_at(level: u32, compiler: &mut Compiler) -> Option<&mut Compiler> {
        if level == 0 {
            return Some(compiler);
        } else {
            return compiler
                .enclosing
                .as_mut()
                .and_then(|e| Self::compiler_at(level - 1, e));
        }
    }

    fn resolve_upvalue(&mut self, compiler_level: u32) -> Option<usize> {
        if let None = Self::compiler_at(compiler_level + 1, &mut self.compiler) {
            return None;
        };

        if let (Some(local), _) = self.resolve_local(compiler_level + 1) {
            let Some(compiler) = Self::compiler_at(compiler_level + 1, &mut self.compiler) else {
                return None;
            };
            compiler
                .locals
                .get_mut(&local)
                .expect("Local not found")
                .captured = true;
            let t = compiler
                .locals
                .get(&local)
                .expect("Local not found")
                .local_type;
            return Some(self.add_upvalue(local, true, compiler_level, t));
        };
        if let Some(upvalue) = self.resolve_upvalue(compiler_level + 1) {
            let Some(compiler) = Self::compiler_at(compiler_level + 1, &mut self.compiler) else {
                return None;
            };
            let t = compiler.upvalues[upvalue].upvalue_type;
            return Some(self.add_upvalue(upvalue, false, compiler_level, t));
        };

        None
    }

    fn add_upvalue(
        &mut self,
        idx: usize,
        is_local: bool,
        compiler_level: u32,
        upvalue_type: ValueType,
    ) -> usize {
        let compiler = Self::compiler_at(compiler_level, &mut self.compiler).unwrap();
        let upvalue_count = compiler.func.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &compiler.upvalues[i];
            if upvalue.index == idx && upvalue.is_local == is_local {
                return i;
            }
        }

        if upvalue_count == u8::MAX as usize {
            self.error("Trying to capture too many variables in closure");
            return 0;
        }
        compiler.upvalues[upvalue_count].is_local = is_local;
        compiler.upvalues[upvalue_count].index = idx;
        compiler.upvalues[upvalue_count].upvalue_type = upvalue_type;
        compiler.func.upvalue_count += 1;

        return compiler.func.upvalue_count - 1;
    }

    fn resolve_local(&mut self, compiler_level: u32) -> (Option<usize>, bool) {
        let Some(compiler) = Self::compiler_at(compiler_level, &mut self.compiler) else {
            return (None, false);
        };
        if compiler.locals.len() == 0 {
            return (None, true);
        }
        for (k, local) in compiler.locals.iter() {
            if self.previous.lexeme == local.name.lexeme {
                if local.depth == -1 {
                    self.error("Cannot read local variable in its own initializer.");
                    return (None, true);
                }
                return (Some(*k), local.mutable || !local.assigned);
            }
        }
        return (None, true);
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> ValueType {
        self.advance();
        let prefix_rule = &self.parse_table[self.previous.token_type as usize].prefix;
        if prefix_rule.is_none() {
            self.error("Expect expression.");
            return ValueTypeK::Err.intern();
        }

        let can_assign = precedence <= Precedence::Assignment;

        let mut t = prefix_rule.unwrap()(self, can_assign);

        while precedence <= self.parse_table[self.current.token_type as usize].precedence {
            self.advance();
            let infix_rule = &self.parse_table[self.previous.token_type as usize].infix;
            let infix_type = &self.parse_table[self.previous.token_type as usize].left_type;
            if !infix_type.contains(t) {
                self.error(&format!(
                    "Type mismatch in operator {:?}, expected one of {:?} got {:?}",
                    self.previous.token_type, infix_type, t
                ));
            }
            self.last_pushed_type = t;
            t = infix_rule.unwrap()(self, can_assign);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }

        t
    }

    fn error_at_current(&self, message: &str) {
        self.error_at(false, message);
    }

    fn error(&self, message: &str) {
        self.error_at(true, message);
    }

    fn error_at(&self, prev: bool, message: &str) {
        if self.panic_mode.get() {
            return;
        }
        self.panic_mode.set(true);
        let token = if prev { &self.previous } else { &self.current };
        print!("[line {}] error", token.line);

        if token.token_type == TokenType::EOF {
            print!(" at end");
        } else if token.token_type == TokenType::Error {
            // Nothing.
        } else {
            print!(" at '{}'", token.lexeme);
        }

        println!(": {}", message);
        self.had_error.set(true);
    }

    fn warn(&self, message: &str) {
        self.warn_at(true, message);
    }

    fn warn_at(&self, prev: bool, message: &str) {
        if self.panic_mode.get() {
            return;
        }
        let token = if prev { &self.previous } else { &self.current };
        print!("[line {}] warning", token.line);

        if token.token_type == TokenType::EOF {
            print!(" at end");
        } else if token.token_type == TokenType::Error {
            // Nothing.
        } else {
            print!(" at '{}'", token.lexeme);
        }

        println!(": {}", message);
    }
}

struct Local {
    name: Token,
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

struct Compiler {
    func: Box<Function>,
    function_type: FunctionType,
    upvalues: [Upvalue; u8::MAX as usize],

    locals: HashMap<usize, Local>,
    local_count: usize,
    scope_depth: i32,

    enclosing: Option<Box<Compiler>>,
}
struct Upvalue {
    pub index: usize,
    pub is_local: bool,
    pub upvalue_type: ValueType,
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler {
            locals: HashMap::new(),
            local_count: 0,
            scope_depth: 0,
            func: Box::new(Function::new(0, Rc::from("script"))),
            function_type: FunctionType::Script,
            enclosing: None,
            upvalues: array::from_fn(|_| Upvalue {
                index: 0,
                is_local: false,
                upvalue_type: ValueTypeK::Undef.intern(),
            }),
        }
    }
}

impl Compiler {
    fn new(function_type: FunctionType, current: Option<Compiler>, func_name: &str) -> Compiler {
        let mut c = Compiler {
            locals: HashMap::new(),
            local_count: 0,
            scope_depth: 0,
            func: Box::new(Function::new(
                0,
                if function_type == FunctionType::Script {
                    Rc::from("script")
                } else {
                    Rc::from(func_name)
                },
            )),
            function_type: function_type,
            enclosing: current.map(Box::new),
            upvalues: array::from_fn(|_| Upvalue {
                index: 0,
                is_local: false,
                upvalue_type: ValueTypeK::Undef.intern(),
            }),
        };
        c.locals.insert(
            c.local_count,
            Local {
                name: Token {
                    token_type: TokenType::Nil,
                    lexeme: "".to_string(),
                    line: 0,
                },
                depth: 0,
                mutable: false,
                assigned: false,
                captured: false,
                local_type: ValueTypeK::Nil.intern(),
            },
        );
        c.local_count += 1;
        c
    }

    fn recover_values(mut self) -> (Function, Option<Box<Compiler>>, [Upvalue; u8::MAX as usize]) {
        (
            std::mem::take(&mut self.func),
            self.enclosing,
            self.upvalues,
        )
    }
}

pub fn compile(
    source: String,
    native_functions: &mut HashMap<String, (usize, ValueType)>,
) -> (Option<Function>, Vec<Value>) {
    let lexer = Scanner::new(source);
    let compiler: Compiler;
    let end_line: usize;
    let had_err: bool;
    let static_data_segment: Vec<Value>;
    {
        let mut parser = Parser::new(
            lexer,
            Box::new(Compiler::new(FunctionType::Function, None, "script")),
            native_functions,
        );

        parser.advance();

        while parser.current.token_type != TokenType::EOF {
            parser.declaration();
        }
        // parser.expression();
        // parser.consume(TokenType::EOF, "Expect end of expression.");
        end_line = parser.previous.line;
        had_err = parser.had_error.get();
        compiler = *parser.compiler;
        static_data_segment = parser.static_data_segment;
    }
    let (mut func, _, _) = compiler.recover_values();
    let chunk = &mut func.chunk;

    if had_err {
        chunk.dissassemble(if func.name != "".into() {
            &func.name
        } else {
            "script"
        });
    };
    chunk.write(Opcode::Nil.into(), end_line);
    chunk.write(Opcode::Return.into(), end_line);
    if had_err {
        (None, static_data_segment)
    } else {
        (Some(func), static_data_segment)
    }
}
