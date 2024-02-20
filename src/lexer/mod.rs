use std::default::Default;

use trie_rs::{Trie, TrieBuilder};

pub struct Lexer {
    source: String,
    lexeme_start: usize,
    lexeme_current: usize,
    line: usize,
    keywords: Trie<u8>,
}

#[derive(Debug, PartialEq, Copy, Clone, Default)]
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
    Nil,
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
    EOF,
    #[default]
    Error,
}

const KEYWORDS: [(&str, TokenType); 25] = [
    ("and", TokenType::And),
    ("struct", TokenType::Struct),
    ("else", TokenType::Else),
    ("false", TokenType::False),
    ("for", TokenType::For),
    ("fun", TokenType::Fun),
    ("if", TokenType::If),
    ("nil", TokenType::Nil),
    ("or", TokenType::Or),
    ("print", TokenType::Print),
    ("return", TokenType::Return),
    ("super", TokenType::Super),
    ("this", TokenType::This),
    ("true", TokenType::True),
    ("var", TokenType::Var),
    ("const", TokenType::Const),
    ("while", TokenType::While),
    ("break", TokenType::Break),
    ("continue", TokenType::Continue),
    ("lambda", TokenType::Lambda),
    ("int", TokenType::SimpleType),
    ("float", TokenType::SimpleType),
    ("string", TokenType::SimpleType),
    ("bool", TokenType::SimpleType),
    ("cast", TokenType::Cast),
];

#[derive(Debug, Clone, Default)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        Lexer {
            source: source,
            lexeme_start: 0,
            lexeme_current: 0,
            line: 1,
            keywords: {
                let mut tb = TrieBuilder::new();
                for (k, _) in KEYWORDS.iter() {
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
            return self.make_token(TokenType::EOF);
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
            }
            return self.make_token(TokenType::Number);
        }

        match c {
            '(' => return self.make_token(TokenType::LeftParen),
            ')' => return self.make_token(TokenType::RightParen),
            '{' => return self.make_token(TokenType::LeftBrace),
            '}' => return self.make_token(TokenType::RightBrace),
            ';' => return self.make_token(TokenType::Semicolon),
            ',' => return self.make_token(TokenType::Comma),
            '.' => return self.make_token(TokenType::Dot),
            '-' => {
                return if self.advance_if_match('>') {
                    self.make_token(TokenType::RightArrow)
                } else {
                    self.make_token(TokenType::Minus)
                }
            }
            '+' => return self.make_token(TokenType::Plus),
            '/' => return self.make_token(TokenType::Slash),
            '*' => return self.make_token(TokenType::Star),
            '?' => return self.make_token(TokenType::QuestionMark),
            ':' => return self.make_token(TokenType::Colon),
            '&' => return self.make_token(TokenType::Amp),
            '[' => return self.make_token(TokenType::LeftBracket),
            ']' => return self.make_token(TokenType::RightBracket),
            '!' => {
                return if self.advance_if_match('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                return if self.advance_if_match('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                return if self.advance_if_match('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                return if self.advance_if_match('=') {
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
                    return self.error_token("Unterminated string.");
                }
                self.advance();
                return self.make_token(TokenType::String);
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
                return self.make_token(TokenType::Char);
            }

            _ => return self.error_token("Unexpected character."),
        }
    }
    fn is_at_end(&self) -> bool {
        self.lexeme_current >= self.source.len()
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type: token_type,
            lexeme: String::from(&self.source[self.lexeme_start..self.lexeme_current]),
            line: self.line,
        }
    }

    fn error_token(&self, message: &str) -> Token {
        Token {
            token_type: TokenType::Error,
            lexeme: String::from(message),
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
