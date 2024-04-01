mod parse_table;

use crate::common::ast::{self, ArrayDeclaration};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::common::ParseResult;
use crate::typing::ValueTypeK;
use crate::typing::{CustomStruct, StructEntry, ValueType};
use crate::value::Value::{self};
use super::Parser;
use num_enum::FromPrimitive;
use tracing::{debug, instrument};

use std::array;
use std::cell::{Cell, RefCell};

use std::collections::HashMap;

use std::num::ParseIntError;
use std::rc::Rc;

use crate::lexer::{Lexer, Token, TokenType};

use self::ast::{
    ASTNode, Declaration, Expression, FunctionDeclaration, FunctionExpression, HalfExpression,
    Statement, StructDeclaration, VarDeclaration,
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

type PrefixParseFn = fn(&mut SerenityParser, bool) -> Expression;
type InfixParseFn = fn(&mut SerenityParser, bool) -> HalfExpression;
struct ParseRule {
    prefix: Option<PrefixParseFn>,
    infix: Option<InfixParseFn>,
    precedence: Precedence,
}

pub struct SerenityParser {
    current: Token,
    previous: Token,
    lexer: Lexer,
    had_error: Cell<bool>,
    panic_mode: Cell<bool>,
    parse_table: [ParseRule; 53],
    custom_types: HashMap<String, CustomStruct>,
}

impl SerenityParser {
    pub fn new(lexer: Lexer) -> SerenityParser {
        let mut p = SerenityParser {
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
            lexer: lexer,
            had_error: false.into(),
            panic_mode: false.into(),
            parse_table: array::from_fn(|_| ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            }),
            custom_types: HashMap::new(),
        };

        p.init_parse_table();
        p
    }
    fn advance(&mut self) {
        self.previous = std::mem::take(&mut self.current);

        loop {
            self.current = self.lexer.scan_token();
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

    fn expression(&mut self) -> Expression {
        return self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) -> ASTNode {
        let ret;
        if self.match_token(TokenType::Var) {
            ret = self.var_declaration(true);
        } else if self.match_token(TokenType::Const) {
            ret = self.var_declaration(false);
        } else if self.match_token(TokenType::Fun) {
            ret = self.fun_declaration();
        } else if self.match_token(TokenType::Struct) {
            ret = self.struct_declaration();
        } else {
            let line = self.previous.line;
            ret = ASTNode::Statement(self.statement(), line);
        }
        if self.panic_mode.get() {
            self.synchronize();
        }
        ret
    }

    fn synchronize(&mut self) {
        self.panic_mode.set(false);

        while self.current.token_type != TokenType::EOF {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }

            match self.current.token_type {
                TokenType::Struct
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

    fn statement(&mut self) -> Statement {
        if self.match_token(TokenType::Print) {
            return self.print_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            let r = self.block();
            return Statement::Block(r);
        } else if self.match_token(TokenType::If) {
            return self.if_statement();
        } else if self.match_token(TokenType::While) {
            return self.while_statement();
        } else if self.match_token(TokenType::For) {
            return self.for_statement();
        } else if self.match_token(TokenType::Semicolon) {
            return Statement::Expression(Expression::Empty.into());
        } else if self.match_token(TokenType::Break) {
            self.consume(TokenType::Semicolon, "Expect ';' after break.");
            let _line = self.previous.line;
            return Statement::Break;
        } else if self.match_token(TokenType::Continue) {
            self.consume(TokenType::Semicolon, "Expect ';' after continue.");
            let _line = self.previous.line;
            return Statement::Continue;
        } else if self.match_token(TokenType::Return) {
            let mut return_node = None;
            if self.match_token(TokenType::Semicolon) {
            } else {
                let e_node = self.expression();
                return_node = e_node.into();
                self.consume(TokenType::Semicolon, "Expect ';' after return value.");
                let _line = self.previous.line;
            }
            return Statement::Return(return_node.map(Box::new));
        } else {
            return self.expression_statement();
        }
    }

    fn block(&mut self) -> Vec<ASTNode> {
        let mut statements = Vec::new();
        while self.current.token_type != (TokenType::RightBrace)
            && self.current.token_type != (TokenType::EOF)
        {
            statements.push(self.declaration());
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
        statements
    }

    fn print_statement(&mut self) -> Statement {
        let _line = self.previous.line;
        let node = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        return Statement::Print(Box::new(node));
    }

    fn expression_statement(&mut self) -> Statement {
        let node = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        let _line = self.previous.line;
        Statement::Expression(Box::new(node))
    }

    fn if_statement(&mut self) -> Statement {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        let cond_node = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let conseq = self.statement();

        let mut alt = None;
        if self.match_token(TokenType::Else) {
            alt = Some(self.statement());
        }
        Statement::If(Box::new(cond_node), Box::new(conseq), alt.map(Box::new))
    }

    fn while_statement(&mut self) -> Statement {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        let cond_node = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let _line = self.previous.line;

        let body = self.statement();

        Statement::While(Box::new(cond_node), Box::new(body))
    }

    fn for_statement(&mut self) -> Statement {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        let line = self.previous.line;

        let initializer: Option<ASTNode>;
        if self.match_token(TokenType::Semicolon) {
            initializer = ASTNode::Expression(Expression::Empty, line).into();
        } else if self.match_token(TokenType::Var) {
            initializer = self.var_declaration(true).into();
        } else {
            initializer = ASTNode::Statement(self.expression_statement(), line).into();
        }

        let mut cond_node = None;
        if !self.match_token(TokenType::Semicolon) {
            let n = self.expression();
            cond_node = Some(n);
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");
        }
        let mut increment_node = None;
        if !self.match_token(TokenType::RightParen) {
            let n = self.expression();
            increment_node = n.into();
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");
        }

        let body = self.statement();

        Statement::For(
            initializer.map(Box::new),
            cond_node.map(Box::new),
            increment_node.map(Box::new),
            Box::new(body),
        )
    }

    fn cast(&mut self, _can_assign: bool) -> Expression {
        // cast(value, type)
        let _line = self.previous.line;
        self.consume(TokenType::LeftParen, "Expect '(' after 'cast'.");
        let node = self.expression();
        self.consume(TokenType::Comma, "Expect ',' after value.");
        let cast_type = self.parse_complex_type(false);
        self.consume(TokenType::RightParen, "Expect ')' after type.");

        Expression::Cast(Box::new(node), cast_type, Cell::new(ValueTypeK::Nil.intern()))
    }

    fn struct_declaration(&mut self) -> ASTNode {
        self.consume(TokenType::Identifier, "Expect struct name.");
        let name = self.previous.clone();
        let line = self.previous.line;

        debug!("before Struct: {}", name.lexeme);

        if self.custom_types.contains_key(&name.lexeme) {
            self.error("Struct with that name already exists.");
        }

        let mut fields = HashMap::new();
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let mut offset = 0;
        self.custom_types.insert(
            name.lexeme.clone(),
            CustomStruct {
                name: name.lexeme.clone(),
                fields: Rc::new(RefCell::new(fields.clone())),
            },
        );
        while self.current.token_type != TokenType::RightBrace {
            self.consume(TokenType::Identifier, "Expect field name.");
            let field_name = self.previous.clone();
            self.consume(TokenType::Colon, "Expect ':' after field name.");
            let mut field_type = self.parse_complex_type(false);

            // if the field is this struct throw an error
            if let ValueTypeK::Struct(s) = field_type {
                if s.name == name.lexeme {
                    self.error("Struct cannot contain itself.");
                }
            }

            // if it is a pointer to this struct it is a pointer to selfstruct
            if let ValueTypeK::Pointer(s, _) = field_type {
                if let ValueTypeK::Struct(s) = *s {
                    field_type =
                        ValueTypeK::Pointer(ValueTypeK::SelfStruct(s.name.clone()).intern(), true)
                            .intern();
                }
            }

            fields.insert(
                field_name.lexeme.clone(),
                StructEntry {
                    value: field_type,
                    offset: offset,
                },
            );
            offset += field_type.num_words();
            if !self.match_token(TokenType::Comma) {
                break;
            }
        }
        self.consume(
            TokenType::RightBrace,
            "Expect '}' after struct declaration.",
        );
        self.consume(TokenType::Semicolon, "Expect ';' after struct declaration.");
        debug!("Struct: {:?}", self.custom_types.get(&name.lexeme));
        debug!("New fields: {:?}", fields);
        self.custom_types
            .get(&name.lexeme)
            .unwrap()
            .fields
            .as_ref()
            .borrow_mut()
            .clone_from(&fields);
        debug!(
            "after Struct: {:?}",
            self.custom_types.get(&name.lexeme).unwrap()
        );
        ASTNode::Declaration(Declaration::Struct(StructDeclaration {
            s: CustomStruct {
                name: name.lexeme.clone(),
                fields: Rc::clone(&self.custom_types.get(&name.lexeme).unwrap().fields),
            },
        }), line)
    }

    fn parse_complex_type(&mut self, is_const: bool) -> ValueType {
        let parset_type = 'a: {
            if self.match_token(TokenType::LeftParen) {
                let t = self.parse_complex_type(is_const);
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
                        let p_type = self.parse_complex_type(is_const);
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
                    return_type = self.parse_complex_type(is_const);
                }
                param_types.push(return_type);
                break 'a ValueTypeK::Closure(param_types.as_slice().into()).intern();
            }

            if self.match_token(TokenType::Struct) {
                self.consume(TokenType::Identifier, "Expect struct name.");
                let name = self.previous.lexeme.clone();
                if let Some(s) = self.custom_types.get(&name) {
                    break 'a ValueTypeK::Struct(s.clone()).intern();
                }
            }

            self.error("Expect type.");
            ValueTypeK::Err.intern()
        };
        if self.match_token(TokenType::Star) {
            return ValueTypeK::Pointer(parset_type, true).intern();
        }
        parset_type
    }

    fn dot(&mut self, _can_assign: bool) -> HalfExpression {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.previous.clone();
        let _line = self.previous.line;

        HalfExpression::Dot(name)
    }

    fn deref_dot(&mut self, _can_assign: bool) -> HalfExpression {
        self.consume(TokenType::Identifier, "Expect property name after '->'.");
        let name = self.previous.clone();
        let _line = self.previous.line;

        HalfExpression::DerefDot(name)
    }

    fn and_(&mut self, _can_assign: bool) -> HalfExpression {
        let _line = self.previous.line;
        let node = self.parse_precedence(Precedence::And);
        HalfExpression::And(node.into())
    }

    fn or_(&mut self, _can_assign: bool) -> HalfExpression {
        let _line = self.previous.line;
        let node = self.parse_precedence(Precedence::Or);
        HalfExpression::Or(node.into())
    }

    fn array_declaration(&mut self, var_name: String, var_type: ValueType) -> ASTNode {
        let mut init = Vec::new();
        let line = self.previous.line;
        match self.parse_literal_index() {
            Ok(n) => {
                self.consume(TokenType::RightBracket, "brace");

                init = self.define_array(Some(n));
            }
            Err(e) => self.error(&e.to_string()),
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );
        ASTNode::Declaration(Declaration::Array(ArrayDeclaration {
            elements: init,
            name: var_name,
            elem_tipe: var_type.into(),
            line: self.previous.line,
        }), line)
    }

    fn define_array(&mut self, size: Option<u32>) -> Vec<Expression> {
        let _line = self.previous.line;
        let mut init = Vec::new();
        if self.match_token(TokenType::Equal) || self.current.token_type == TokenType::LeftBrace {
            self.consume(TokenType::LeftBrace, "Expect '{' after array declaration.");
            if self.current.token_type != TokenType::RightBrace {
                loop {
                    let node = self.expression();
                    init.push(node);
                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBrace, "Expect '}' after array declaration.");

            if let Some(n) = size {
                if n as usize != init.len() {
                    self.error(&format!(
                        "Array size mismatch, expected {} got {}",
                        n,
                        init.len()
                    ));
                }
            }
        } else {
            if let Some(n) = size {
                init = vec![Expression::Empty; n as usize];
            }
        }

        init
    }

    fn var_declaration(&mut self, mutable: bool) -> ASTNode {
        let line = self.previous.line;

        self.consume(TokenType::Identifier, "Expect variable name.");
        let name = self.previous.lexeme.clone();

        let mut var_type = None;
        if self.match_token(TokenType::Colon) {
            var_type = self.parse_complex_type(!mutable).into();
            if self.match_token(TokenType::LeftBracket) {
                return self.array_declaration(name.clone(), var_type.unwrap());
            }
        }
        let mut initializer = None;
        if self.match_token(TokenType::Equal) {
            if self.current.token_type == TokenType::LeftBrace {
                let elems = self.define_array(None);
                return ASTNode::Declaration(Declaration::Array(ArrayDeclaration {
                    elements: elems.into(),
                    name: name,
                    elem_tipe: var_type,
                    line: line,
                }), line);
            }
            if self.current.token_type == TokenType::Struct {
                let s = self.struct_initializer();
                initializer = Some(s);
            } else {
                let node = self.expression();
                initializer = Some(node);
            }
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        ASTNode::Declaration(Declaration::Var(VarDeclaration {
            name: name,
            tipe: var_type.into(),
            initializer: initializer.map(Box::new),
            mutable: mutable,
            line: line,
        }), line)
    }

    fn struct_initializer(&mut self) -> Expression {
        let _line = self.previous.line;
        let ValueTypeK::Struct(s) = self.parse_complex_type(false) else {
            self.error("Expect struct type.");
            return Expression::Empty;
        };
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let mut fields = HashMap::new();
        while self.current.token_type != TokenType::RightBrace {
            self.consume(TokenType::Identifier, "Expect field name.");
            let name = self.previous.clone();
            self.consume(TokenType::Colon, "Expect ':' after field name.");
            let value = self.expression();
            fields.insert(name.lexeme.clone(), value);
            if !self.match_token(TokenType::Comma) {
                break;
            }
        }
        self.consume(
            TokenType::RightBrace,
            "Expect '}' after struct initializer.",
        );
        Expression::StructInitializer(s.clone(), fields)
    }

    fn fun_declaration(&mut self) -> ASTNode {
        self.consume(TokenType::Identifier, "Expect function name.");
        let name = self.previous.lexeme.clone();

        let node = self.function(&name);

        return ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
            name: name,
            body: node,
            line: self.previous.line,
        }), self.previous.line);
    }

    fn lambda(&mut self, _can_assign: bool) -> Expression {
        static ANON_ID : AtomicUsize =  AtomicUsize::new(0);
        let func_expr = self.function(format!("anon{}", ANON_ID.load(Ordering::Relaxed)).as_str());
        ANON_ID.fetch_add(1, Ordering::Relaxed);
        Expression::Function(func_expr)
    }

    fn function(&mut self, _func_name: &str) -> FunctionExpression {
        let mut params = Vec::new();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        let mut param_types: Vec<ValueType> = Vec::new();
        if self.current.token_type != TokenType::RightParen {
            loop {
                let mut mutable = true;
                if self.match_token(TokenType::Const) {
                    mutable = false;
                }
                self.consume(TokenType::Identifier, "Expect parameter name.");
                let name = self.previous.lexeme.clone();
                self.consume(TokenType::Colon, "Expect ':' after parameter name.");

                let p_type = self.parse_complex_type(!mutable);
                param_types.push(p_type);

                params.push((name, p_type));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        let mut return_type = ValueTypeK::Nil.intern();
        if self.match_token(TokenType::RightArrow) {
            return_type = self.parse_complex_type(false);
        }
        param_types.push(return_type);

        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        let nodes = self.block();

        FunctionExpression::new(params, nodes, return_type, _func_name.to_string())
    }

    fn assign(&mut self, _can_assign: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::Assignment);

        HalfExpression::Assign(node.into())
    }

    fn number(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        match self.previous.lexeme.parse::<i64>() {
            Ok(v) => Expression::Literal(Value::Integer(v)),

            Err(_) => {
                let value = self.previous.lexeme.parse::<f64>().unwrap();

                Expression::Literal(Value::Float(value))
            }
        }
    }

    fn char(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        let mut value = std::mem::take(&mut self.previous.lexeme);
        value = value.trim_matches('\'').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        let c = value.as_bytes()[0] as u8;

        Expression::Literal(Value::Char(c))
    }

    fn grouping(&mut self, _can_assign: bool) -> Expression {
        let expr = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        expr
    }

    fn ternary(&mut self, _can_assign: bool) -> HalfExpression {
        let expr = self.parse_precedence(Precedence::Ternary.next());
        self.consume(TokenType::Colon, "Expect ':' after expression.");
        let else_expr = self.parse_precedence(Precedence::Ternary);
        return HalfExpression::Ternary(Box::new(expr.into()), Box::new(else_expr.into()));
    }

    fn unary(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        let operator_type = self.previous.token_type;
        // Compile the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Unary(operator_type, Box::new(expr))
    }

    fn deref(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        // Compile the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        return Expression::Deref(Box::new(expr));
    }

    fn parse_literal_index(&mut self) -> Result<u32, ParseIntError> {
        self.match_token(TokenType::Number);
        return self.previous.lexeme.parse::<u32>();
    }

    fn parse_expression_index(&mut self) -> Expression {
        let e = self.expression();
        e
    }

    fn index(&mut self, _can_assign: bool) -> HalfExpression {
        let _line = self.previous.line;

        let expridx = self.parse_expression_index();
        self.consume(TokenType::RightBracket, "Expect ']' after index.");

        return HalfExpression::Index(Box::new(expridx));
    }

    fn addr_of(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        // parse the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Ref(Box::new(expr))
    }

    fn binary(&mut self, _can_assign: bool) -> HalfExpression {
        let operator_type = self.previous.token_type;
        let _line = self.previous.line;

        // Compile the right operand.
        let rule = &self.parse_table[operator_type as usize];
        let expr = self.parse_precedence(rule.precedence.next());

        // Emit the operator instruction.
        return HalfExpression::Binary(operator_type, Box::new(expr.into()));
    }

    fn call(&mut self, _can_assign: bool) -> HalfExpression {
        let _line = self.previous.line;
        let nodes = self.argument_list();
        HalfExpression::Call(nodes)
    }

    fn argument_list(&mut self) -> Vec<Expression> {
        let mut arg_count = 0;
        let mut nodes = Vec::new();
        if self.current.token_type != TokenType::RightParen {
            loop {
                let expr = self.expression();
                nodes.push(expr);

                if arg_count == 255 {
                    self.error("Cannot have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        nodes
    }

    fn literal(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        match self.previous.token_type {
            TokenType::False => Expression::Literal(Value::Bool(false)),
            TokenType::Nil => Expression::Literal(Value::Nil),
            TokenType::True => Expression::Literal(Value::Bool(true)),
            _ => unreachable!(),
        }
    }

    fn string(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;
        let mut value = std::mem::take(&mut self.previous.lexeme);
        value = value.trim_matches('"').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\a", "\x07");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        Expression::StringLiteral(value)
    }

    fn variable(&mut self, _can_assign: bool) -> Expression {
        let _line = self.previous.line;

        Expression::Variable(self.previous.clone())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Expression {
        self.advance();
        let prefix_rule = &self.parse_table[self.previous.token_type as usize].prefix;
        if prefix_rule.is_none() {
            self.error("Expect expression.");
            return Expression::Empty;
        }

        let can_assign = precedence <= Precedence::Assignment;

        let mut node = prefix_rule.unwrap()(self, can_assign);

        while precedence <= self.parse_table[self.current.token_type as usize].precedence {
            let _prev = self.previous.lexeme.clone();
            self.advance();
            let infix_rule = &self.parse_table[self.previous.token_type as usize].infix;

            let hnode = infix_rule.unwrap()(self, can_assign);
            node = hnode.fill(node);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }

        node
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

    fn _warn(&self, message: &str) {
        self._warn_at(true, message);
    }

    fn _warn_at(&self, prev: bool, message: &str) {
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

impl Parser for SerenityParser {
    #[instrument(skip(lexer))]
    fn parse(lexer: Lexer) -> ParseResult {
        
        let mut ret = ParseResult::default();
        {
            let mut parser = SerenityParser::new(lexer);

            parser.advance();

            let mut nodes = Vec::new();
            while parser.current.token_type != TokenType::EOF {
                nodes.push(parser.declaration());
            }
            
            //call the users main function
            nodes.push(ASTNode::Statement(Statement::Expression(Expression::Call(Expression::Variable(Token{ token_type: TokenType::Identifier, lexeme: "main".to_string(), line: 0 }).into(), Vec::new()).into()), 0));
            

            parser.consume(TokenType::EOF, "Expect end of file.");
            ret.ast = ASTNode::Module(nodes);
            ret.had_errors = parser.had_error.get();
            ret.custom_structs = parser.custom_types.clone();
        }

        if ret.had_errors {
            tracing::error!("Parse errors");
        } else {
            tracing::info!("Parse success");
        }

        ret
    }
}
