mod pratt_parse;
mod recdec_parse;
mod templates;
mod test;
mod parse_type;

use std::{cell::Cell, collections::HashMap};

use anyhow::{Error, Result};
use num_enum::FromPrimitive;
use pratt_parse::ParseTable;

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

    fn go(&mut self) -> Vec<ASTNode> {
        let mut nodes = Vec::new();

        while self.current.token_type != TokenType::Eof {
            let new_nodes = self.declaration();

            for node in new_nodes {
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
       _: bool,
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
