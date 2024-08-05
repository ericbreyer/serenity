use std::ops::Deref;

use crate::lexer;

use super::{ Precedence, SerenityParser, ParseRule};

pub struct  ParseTable([ParseRule; lexer::TokenType::num_variants()]);


impl Deref for ParseTable {
    type Target = [ParseRule; lexer::TokenType::num_variants()];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! no_pratt {
    () => {
        ParseRule {
            prefix: None,
            infix: None,

            precedence: Precedence::None,
        }
    };
}

impl SerenityParser {
    pub(in super::super) const fn parse_table() -> ParseTable {
        ParseTable([
            ParseRule {
                prefix: Some(SerenityParser::grouping),
                infix: Some(SerenityParser::call),
                precedence: Precedence::Call,
            }, // LeftParen
            no_pratt!(), // Right Paren
            no_pratt!(), // LeftBrace
            no_pratt!(), // RightBrace
            no_pratt!(), // Comma
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::dot),
                precedence: Precedence::Call,
            }, // Dot
            ParseRule {
                prefix: Some(SerenityParser::unary),
                infix: Some(SerenityParser::binary),
                precedence: Precedence::Term,
            }, // Minus
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),
                precedence: Precedence::Term,
            }, // Plus
            no_pratt!(), // Semicolon
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Factor,
            }, // Slash
            ParseRule {
                prefix: Some(SerenityParser::deref),
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Factor,
            }, // Star
            ParseRule {
                prefix: Some(SerenityParser::unary),
                infix: None,

                precedence: Precedence::None,
            }, // Bang
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Equality,
            }, // BangEqual
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::assign),

                precedence: Precedence::Assignment,
            }, // Equal
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Equality,
            }, // EqualEqual
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Comparison,
            }, // Greater
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Comparison,
            }, // GreaterEqual
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Comparison,
            }, // Less
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::binary),

                precedence: Precedence::Comparison,
            }, // LessEqual
            ParseRule {
                prefix: Some(SerenityParser::variable),
                infix: None,

                precedence: Precedence::None,
            }, // Identifier
            ParseRule {
                prefix: Some(SerenityParser::string),
                infix: None,

                precedence: Precedence::None,
            }, // Rc<str>
            ParseRule {
                prefix: Some(SerenityParser::number),
                infix: None,

                precedence: Precedence::None,
            }, // Number
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::and_),

                precedence: Precedence::And,
            }, // And
            no_pratt!(), // Struct
            no_pratt!(), // Else
            ParseRule {
                prefix: Some(SerenityParser::literal),
                infix: None,

                precedence: Precedence::None,
            }, // False
            no_pratt!(), // Fun
            no_pratt!(), // For
            no_pratt!(), // If
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::or_),

                precedence: Precedence::Or,
            }, // Or
            no_pratt!(), // Print
            no_pratt!(), // Return
            no_pratt!(), // Super
            no_pratt!(), // This
            ParseRule {
                prefix: Some(SerenityParser::literal),
                infix: None,

                precedence: Precedence::None,
            }, // True
            no_pratt!(), // Var
            no_pratt!(), // While
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::ternary),

                precedence: Precedence::Ternary,
            }, // QuestionMark
            no_pratt!(), // Colon
            ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::Call,
            }, // Double Colon
            no_pratt!(), // Const
            no_pratt!(), // Break
            no_pratt!(), // Continue
            ParseRule {
                prefix: Some(SerenityParser::lambda),
                infix: None,

                precedence: Precedence::None,
            }, // Lambda
            ParseRule {
                prefix: Some(SerenityParser::addr_of),
                infix: None,

                precedence: Precedence::None,
            }, // Amp
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::index),

                precedence: Precedence::Call,
            }, // LeftBracket
            no_pratt!(), // RightBracket
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::deref_dot),

                precedence: Precedence::Call,
            }, // RightArrow
            ParseRule {
                prefix: Some(SerenityParser::cast_prefix),
                infix: None,

                precedence: Precedence::None,
            }, // SimpleType
            ParseRule {
                prefix: Some(SerenityParser::cast),
                infix: None,

                precedence: Precedence::None,
            }, // Cast
            ParseRule {
                prefix: Some(SerenityParser::char),
                infix: None,

                precedence: Precedence::None,
            }, // Char
            ParseRule {
                prefix: None,
                // infix: Some(SerenityParser::pipe),
                infix: None,
                precedence: Precedence::Call,
            }, // Pipe
            no_pratt!(), // LeftParenBrace
            no_pratt!(), // RightParenBrace
            no_pratt!(), //Include
            no_pratt!(), // Interface
            no_pratt!(), // Impl
            no_pratt!(), // Implements
            no_pratt!(), // Type
            no_pratt!(), // EOF
            no_pratt!(), // Error
        ])
    }
}
