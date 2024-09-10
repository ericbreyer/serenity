use std::fmt::Debug;
use strum::EnumCount as _;
use strum_macros::{Display, EnumCount};
use trie_rs::{Trie, TrieBuilder};

use crate::prelude::*;

pub struct Lexer {
    source: SharedString,
    lexeme_start: usize,
    lexeme_current: usize,
    line: usize,
    keywords: Trie<u8>,
}

#[derive(Debug, Display,PartialEq, Copy, Clone, EnumCount)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Struct,
    Else,
    False,
    Fun,
    For,
    If,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    QuestionMark,
    Colon,
    DoubleColon,
    Const,
    Break,
    Continue,
    Lambda,
    Amp,
    LeftBracket,
    RightBracket,
    RightArrow,
    SimpleType,
    Cast,
    Char,
    Pipe,
    LeftParenBrace,
    RightParenBrace,
    Include,
    Interface,
    Impl,
    Implements,
    Type,
    Percent,
    Sizeof,
    Eof,
    Error,
}

impl TokenType {
    pub const fn num_variants() -> usize {
        Self::COUNT
    }
}

const KEYWORDS: [(&str, TokenType); 35] = [
    ("and", TokenType::And),
    ("struct", TokenType::Struct),
    ("else", TokenType::Else),
    ("false", TokenType::False),
    ("for", TokenType::For),
    ("fun", TokenType::Fun),
    ("fn", TokenType::Fun),
    ("if", TokenType::If),
    ("nil", TokenType::SimpleType),
    ("or", TokenType::Or),
    ("print", TokenType::Print),
    ("return", TokenType::Return),
    ("super", TokenType::Super),
    ("this", TokenType::This),
    ("true", TokenType::True),
    ("var", TokenType::Var),
    ("let", TokenType::Var),
    ("const", TokenType::Const),
    ("while", TokenType::While),
    ("break", TokenType::Break),
    ("continue", TokenType::Continue),
    ("lambda", TokenType::Lambda),
    ("λ", TokenType::Lambda),
    ("uint", TokenType::SimpleType),
    ("int", TokenType::SimpleType),
    ("float", TokenType::SimpleType),
    ("bool", TokenType::SimpleType),
    ("cast", TokenType::Cast),
    ("char", TokenType::SimpleType),
    ("include", TokenType::Include),
    ("interface", TokenType::Interface),
    ("impl", TokenType::Impl),
    ("implements", TokenType::Implements),
    ("type", TokenType::Type),
    ("sizeof", TokenType::Sizeof),
];

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: SharedString,
    pub line: usize,
}

impl Token {
    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::Eof
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, r#"<{:12} {:13?} [{}]>"#, self.token_type, self.lexeme, self.line)
    }
}

impl Lexer {
    pub fn new(source: SharedString) -> Lexer {
        Lexer {
            source,
            lexeme_start: 0,
            lexeme_current: 0,
            line: 1,
            keywords: {
                let mut tb = TrieBuilder::new();
                for (k, _) in &KEYWORDS {
                    tb.push(k.as_bytes());
                }
                tb.build()
            },
        }
    }
    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.lexeme_start = self.lexeme_current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        };

        let c = self.advance();
        if c.is_ascii_alphabetic() || c == '_' {
            while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
                self.advance();
            }
            return self.make_token(self.identifier_type());
        }

        if c.is_ascii_digit() {
            while self.peek().is_ascii_digit() {
                self.advance();
            }
            if self.peek() == '.' && self.peek_next().is_ascii_digit() {
                self.advance();
                while self.peek().is_ascii_digit() {
                    self.advance();
                }
            } else if self.peek() == 'u' {
                self.advance();
            }

            return self.make_token(TokenType::Number);
        }

        match c {
            '(' => {
                if self.advance_if_match('{') {
                    self.make_token(TokenType::LeftParenBrace)
                } else {
                    self.make_token(TokenType::LeftParen)
                }
            }
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => {
                if self.advance_if_match(')') {
                    self.make_token(TokenType::RightParenBrace)
                } else {
                    self.make_token(TokenType::RightBrace)
                }
            }
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => {
                if self.advance_if_match('>') {
                    self.make_token(TokenType::RightArrow)
                } else {
                    self.make_token(TokenType::Minus)
                }
            }
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '?' => self.make_token(TokenType::QuestionMark),
            ':' => {
                if self.advance_if_match(':') {
                    self.make_token(TokenType::DoubleColon)
                } else {
                    self.make_token(TokenType::Colon)
                }
            }
            '&' => self.make_token(TokenType::Amp),
            '[' => self.make_token(TokenType::LeftBracket),
            ']' => self.make_token(TokenType::RightBracket),
            '!' => {
                if self.advance_if_match('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.advance_if_match('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.advance_if_match('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.advance_if_match('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            '"' => {
                while self.peek() != '"' && !self.is_at_end() {
                    if self.peek() == '\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                if self.is_at_end() {
                    return self.error_token("Unterminated SharedString.");
                }
                self.advance();
                self.make_token(TokenType::String)
            }
            '\'' => {
                if self.peek() == '\\' {
                    self.advance();
                }
                self.advance();
                if self.peek() != '\'' {
                    return self.error_token("Unterminated char.");
                }
                self.advance();
                self.make_token(TokenType::Char)
            }
            '|' => {
                if self.advance_if_match('|') {
                    self.make_token(TokenType::Or)
                } else {
                    self.make_token(TokenType::Pipe)
                }
            }
            '%' => self.make_token(TokenType::Percent),
            '#' => self.make_token(TokenType::Cast),

            _ => self.error_token("Unexpected character."),
        }
    }
    fn is_at_end(&self) -> bool {
        self.lexeme_current >= self.source.len()
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: String::from(&self.source[self.lexeme_start..self.lexeme_current]).into(),
            line: self.line,
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            token_type: TokenType::Error,
            lexeme: message.into(),
            line: self.line,
        }
    }

    fn advance(&mut self) -> char {
        self.lexeme_current += 1;
        self.source.chars().nth(self.lexeme_current - 1).unwrap()
    }

    fn advance_if_match(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.lexeme_current).unwrap() != expected {
            return false;
        }

        self.lexeme_current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.lexeme_current).unwrap()
    }

    fn peek_next(&self) -> char {
        if self.lexeme_current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.lexeme_current + 1).unwrap()
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }
    fn identifier_type(&self) -> TokenType {
        let lexeme = &self.source[self.lexeme_start..self.lexeme_current];
        let is_keyword = self.keywords.exact_match(lexeme.as_bytes());
        if is_keyword {
            return KEYWORDS.iter().find(|(k, _)| k == &lexeme).unwrap().1;
        };
        TokenType::Identifier
    }
}
