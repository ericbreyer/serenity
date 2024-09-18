mod pratt_parse;
mod recdec_parse;
mod templates;
mod test;

use std::{
    cell::Cell,
    collections::HashMap,
};

use anyhow::{Error, Result};
use num_enum::FromPrimitive;
use pratt_parse::ParseTable;
use tracing::instrument;

use crate::{
    lexer::{Lexer, Token, TokenType},
    prelude::*,
};

use super::Parser;

#[derive(Debug, PartialEq, PartialOrd, FromPrimitive, Copy, Clone, Default)]
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
    Mod,        // %
    Cast,       // as
    Unary,      // ! - #
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        if matches!(self, Self::Primary) {
            *self
        } else {
            Precedence::from((*self as u8) + 1)
        }
    }
}

pub struct SerenityParser {
    current: Token,
    previous: Token,
    lexer: Lexer,
    had_error: Cell<Option<Error>>,
    panic_mode: Cell<bool>,
    parse_table: ParseTable,
    custom_types: HashMap<SharedString, CustomStruct>,
    constants: HashMap<SharedString, Value>,
}

impl SerenityParser {
    pub fn new(lexer: Lexer) -> SerenityParser {
        SerenityParser {
            current: Token {
                token_type: TokenType::Error,
                lexeme: "".into(),
                line: 0,
            },
            previous: Token {
                token_type: TokenType::Error,
                lexeme: "".into(),
                line: 0,
            },
            lexer,
            had_error: None.into(),
            panic_mode: false.into(),
            parse_table: Self::parse_table(),
            custom_types: HashMap::new(),
            constants: HashMap::new(),
        }
    }
    fn advance(&mut self) {
        self.previous = self.current.clone();

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

    fn synchronize(&mut self) {
        self.panic_mode.set(false);

        while self.current.token_type != TokenType::Eof {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }

            match self.current.token_type {
                TokenType::Struct
                | TokenType::Type
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                // | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => (),
            }

            self.advance();
        }
    }

    #[instrument(skip(self))]
    fn parse_complex_type(
        &mut self,
        struct_name: Option<&SharedString>,
        type_params: Option<&Vec<SharedString>>,
    ) -> UValueType {
        let star = self.match_token(TokenType::Star);
        let parset_type = 'a: {
            if self.match_token(TokenType::LeftParen) {
                let t = self.parse_complex_type(struct_name, type_params);
                self.consume(TokenType::RightParen, "Expect ')' after type.");
                break 'a t;
            }

            if self.match_token(TokenType::LeftBracket) {
                let t = self.parse_complex_type(struct_name, type_params);
                let mut index = None;
                if self.match_token(TokenType::Semicolon) {
                    let sindex = self.expression().eval_constexpr();
                    index = Some(match sindex {
                        Some(Value::UInteger(index)) => index,
                        Some(Value::Integer(index)) if index >= 0 => index as u64,
                        _ => {
                            self.error("Expect constant unsigned index.");
                            break 'a ValueType::Err.intern();
                        }
                    } as usize);
                }

                self.consume(TokenType::RightBracket, "Expect ']' after index.");
                break 'a ValueType::Array(t, index).intern();
            }

            if self.match_token(TokenType::Identifier) {
                let name = self.previous.lexeme.clone();
                if let Some(s) = type_params.and_then(|tp| tp.contains(&name).then(|| name.clone()))
                {
                    break 'a ValueType::GenericParam(s.clone()).intern();
                }
            }

            if self.match_token(TokenType::SimpleType) {
                break 'a self.previous.lexeme.clone().into();
            }

            if self.match_token(TokenType::Fun) {
                let mut param_types = Vec::new();

                let mut captures = Vec::new();
                if self.match_token(TokenType::LeftBracket) {
                    loop {
                        let p_type = self.parse_complex_type(struct_name, type_params);
                        captures.push(p_type);
                        if !self.match_token(TokenType::Comma) {
                            break;
                        }
                    }
                    self.consume(TokenType::RightBracket, "Expect ']' after captures.");
                }

                self.consume(TokenType::LeftParen, "Expect '(' after 'fun'.");
                let mut _arg_count = 0;
                if self.current.token_type != TokenType::RightParen {
                    loop {
                        let p_type = self.parse_complex_type(struct_name, type_params);
                        param_types.push(p_type);
                        _arg_count += 1;
                        if !self.match_token(TokenType::Comma) {
                            break;
                        }
                    }
                }
                self.consume(TokenType::RightParen, "Expect ')' after arguments.");
                let mut return_type = ValueType::Nil.intern();
                if self.match_token(TokenType::RightArrow) {
                    return_type = self.parse_complex_type(struct_name, type_params);
                }

                break 'a ValueType::Closure(
                    param_types.as_slice().into(),
                    captures.as_slice().into(),
                    return_type,
                )
                .intern();
            }

            if self.match_token(TokenType::Struct) {
                self.consume(TokenType::Identifier, "Expect struct name.");
                let name = self.previous.lexeme.clone();

                if struct_name.is_some_and(|s| *s == name) {
                    let tps = if self.match_token(TokenType::Less) {
                        let mut params = Vec::new();
                        loop {
                            let p_type = self.parse_complex_type(struct_name, type_params);
                            params.push(p_type);
                            if !self.match_token(TokenType::Comma) {
                                break;
                            }
                        }
                        self.consume(TokenType::Greater, "Expect '>' after type parameters.");
                        params
                    } else {
                        Vec::new()
                    };

                    break 'a ValueType::SelfStruct(name, tps).intern();
                }
                if let Some(s) = self.custom_types.get(&name).cloned() {
                    if self.match_token(TokenType::Less) {
                        let mut params = Vec::new();
                        loop {
                            let p_type = self.parse_complex_type(struct_name, type_params);
                            params.push(p_type);
                            if !self.match_token(TokenType::Comma) {
                                break;
                            }
                        }
                        self.consume(TokenType::Greater, "Expect '>' after type parameters.");
                        if params.len() != s.type_vars.borrow().len() {
                            self.error("Expect same number of type parameters.");
                            break 'a ValueType::Err.intern();
                        }
                        let mut type_params = HashMap::new();
                        for (name, p_type) in s.type_vars.borrow().iter().zip(params) {
                            type_params.insert(name.clone(), p_type);
                        }
                        break 'a ValueType::Struct(s.clone())
                            .instantiate_generic(&mut type_params)
                            .intern();
                    }
                    break 'a ValueType::Struct(s.clone())
                        .instantiate_generic(&mut HashMap::new())
                        .intern();
                }
            }

            if self.match_token(TokenType::Impl) {
                self.consume(TokenType::Identifier, "Expect interface name.");
                let name: SharedString = format!("{}_impl", self.previous.lexeme).into();

                if struct_name.is_some_and(|s| *s == name) {
                    break 'a ValueType::SelfStruct(name, vec![]).intern();
                }
                if let Some(s) = self.custom_types.get(&name) {
                    break 'a ValueType::Struct(s.clone()).intern();
                }
            }

            self.error("Expect type.");
            ValueType::Err.intern()
        };
        if self.match_token(TokenType::Star) || star {
            return ValueType::Pointer(parset_type, true).intern();
        }
        parset_type
    }

    fn go(&mut self) -> Vec<ASTNode> {
        let mut nodes = Vec::new();

        // let mut weak_funcs = HashMap::new();

        while self.current.token_type != TokenType::Eof {
            let new_nodes = self.declaration();

            for node in new_nodes {
                // if let ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
                //     prototype,
                //     line_no,
                //     body,
                // })) = &node
                // {
                //     if *weak {
                //         weak_funcs.insert(name.clone(), nodes.len());
                //     } else if weak_funcs.contains_key(&name) {
                //         nodes[weak_funcs[&name]] = node.clone();
                //         continue;
                //     }
                // }

                nodes.push(node)
            }
        }
        nodes
    }

    //--------//
    // Errors //
    //--------//

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
        let mut err_msg = format!("[line {}] error", token.line);

        if token.token_type == TokenType::Eof {
            err_msg = format!("{err_msg} at end");
        } else if token.token_type == TokenType::Error {
            // Nothing.
        } else {
            err_msg = format!("{err_msg} at '{}'", token.lexeme);
        }

        err_msg = format!("{err_msg} : {message}\n");
        print!("{err_msg}");
        self.had_error.set(Some(Error::msg(err_msg)));
    }

    fn warn(&self, message: &str) {
        self.warn_at(true, message);
    }

    fn warn_at(&self, prev: bool, message: &str) {
        if self.panic_mode.get() {
            return;
        }
        let token = if prev { &self.previous } else { &self.current };
        let mut warn_msg = format!("[line {}] warn", token.line);

        if token.token_type == TokenType::Eof {
            warn_msg = format!("{warn_msg} at end");
        } else if token.token_type == TokenType::Error {
            // Nothing.
        } else {
            warn_msg = format!("{warn_msg} at '{}'", token.lexeme);
        }

        warn_msg = format!("{warn_msg} : {message}\n");
        print!("{warn_msg}");
    }
}

impl Parser for SerenityParser {
    fn parse(source: SharedString, name: SharedString) -> Result<ParseResult> {
        SerenityParser::parse_helper(source, name, true, HashMap::default())
    }
}

impl SerenityParser {
    fn parse_helper(
        source: SharedString,
        name: SharedString,
        _top_level: bool,
        custom_types: HashMap<SharedString, CustomStruct>,
    ) -> Result<ParseResult> {
        tracing::info!("Starting parse of {name}");
        let mut ret = ParseResult::default();
        let errors: Option<Error>;
        {
            let lexer = crate::lexer::Lexer::new(source);
            let mut parser = SerenityParser::new(lexer);
            parser.custom_types.extend(custom_types);
            parser.advance();

            let nodes = parser.go();

            // if top_level {
            //     //call the users main function
            //     nodes.push(ASTNode::CallMain(Box::new(ASTNode::Expression(
            //         Expression::Call(CallExpression {
            //             callee: Expression::Variable(VariableExpression {
            //                 token: Token {
            //                     token_type: TokenType::Identifier,
            //                     lexeme: "main".into(),
            //                     line: 0,
            //                 },
            //                 line_no: 0,
            //             }
            //             )

            //             .into(),
            //             arguments: Vec::new(),
            //             line_no: 0,
            //         }
            //         )
            //         .into(),
            //     ))));
            // }

            parser.consume(TokenType::Eof, "Expect end of file.");
            ret.ast.roots = nodes;
            errors = parser.had_error.into_inner();
            ret.custom_structs = parser.custom_types.clone();
        }

        if errors.is_some() {
            tracing::error!("Parse failed");
            return Err(errors.unwrap());
        } else {
            tracing::info!("Parse of {name} succeeded");
        }

        Ok(ret)
    }
}
