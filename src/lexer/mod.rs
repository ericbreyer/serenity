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

#[derive(Debug, Display, PartialEq, Copy, Clone, EnumCount)]
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
    Mut,
    Eof,
    Error,
}

impl TokenType {
    pub const fn num_variants() -> usize {
        Self::COUNT
    }
}

const KEYWORDS: [(&str, TokenType); 34] = [
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
    ("mut", TokenType::Mut),
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
        write!(
            f,
            r#"<{:12} {:13?} [{}]>"#,
            self.token_type, self.lexeme, self.line
        )
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
            '$' => {
                while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
                    self.advance();
                }
                self.make_token(TokenType::SimpleType)
            }
            _ => self.error_token(format!("Unexpected character: {}", c).as_str()),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(source: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(source.into());
        let mut tokens = Vec::new();
        loop {
            let token = lexer.scan_token();
            let is_eof = token.is_eof();
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    #[test]
    fn test_simple_number() {
        let tokens = lex_all("42");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].lexeme, "42".into());
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_float_number() {
        let tokens = lex_all("3.14");
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].lexeme, "3.14".into());
    }

    #[test]
    fn test_unsigned_integer() {
        let tokens = lex_all("42u");
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].lexeme, "42u".into());
    }

    #[test]
    fn test_identifier() {
        let tokens = lex_all("variable_name");
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "variable_name".into());
    }

    #[test]
    fn test_keywords() {
        let keywords_to_test = vec![
            ("fun", TokenType::Fun),
            ("fn", TokenType::Fun),
            ("let", TokenType::Var),
            ("var", TokenType::Var),
            ("if", TokenType::If),
            ("else", TokenType::Else),
            ("while", TokenType::While),
            ("for", TokenType::For),
            ("return", TokenType::Return),
            ("true", TokenType::True),
            ("false", TokenType::False),
            ("struct", TokenType::Struct),
            ("interface", TokenType::Interface),
            ("impl", TokenType::Impl),
        ];

        for (keyword, expected_type) in keywords_to_test {
            let tokens = lex_all(keyword);
            assert_eq!(
                tokens[0].token_type, expected_type,
                "Keyword '{}' should lex as {:?}",
                keyword, expected_type
            );
        }
    }

    #[test]
    fn test_operators() {
        let tests = vec![
            ("+", TokenType::Plus),
            ("-", TokenType::Minus),
            ("*", TokenType::Star),
            ("/", TokenType::Slash),
            ("%", TokenType::Percent),
            ("!", TokenType::Bang),
            ("!=", TokenType::BangEqual),
            ("=", TokenType::Equal),
            ("==", TokenType::EqualEqual),
            ("<", TokenType::Less),
            ("<=", TokenType::LessEqual),
            (">", TokenType::Greater),
            (">=", TokenType::GreaterEqual),
            ("&", TokenType::Amp),
            ("|", TokenType::Pipe),
            ("||", TokenType::Or),
            ("->", TokenType::RightArrow),
        ];

        for (op, expected_type) in tests {
            let tokens = lex_all(op);
            assert_eq!(
                tokens[0].token_type, expected_type,
                "Operator '{}' should lex as {:?}",
                op, expected_type
            );
        }
    }

    #[test]
    fn test_delimiters() {
        let tests = vec![
            ("(", TokenType::LeftParen),
            (")", TokenType::RightParen),
            ("{", TokenType::LeftBrace),
            ("}", TokenType::RightBrace),
            ("[", TokenType::LeftBracket),
            ("]", TokenType::RightBracket),
            (";", TokenType::Semicolon),
            (",", TokenType::Comma),
            (".", TokenType::Dot),
            (":", TokenType::Colon),
            ("::", TokenType::DoubleColon),
            ("?", TokenType::QuestionMark),
        ];

        for (delim, expected_type) in tests {
            let tokens = lex_all(delim);
            assert_eq!(
                tokens[0].token_type, expected_type,
                "Delimiter '{}' should lex as {:?}",
                delim, expected_type
            );
        }
    }

    #[test]
    fn test_string_literal() {
        let tokens = lex_all(r#""hello world""#);
        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(tokens[0].lexeme, r#""hello world""#.into());
    }

    #[test]
    fn test_char_literal() {
        let tokens = lex_all("'a'");
        assert_eq!(tokens[0].token_type, TokenType::Char);
        assert_eq!(tokens[0].lexeme, "'a'".into());
    }

    #[test]
    fn test_char_escape() {
        let tokens = lex_all(r"'\n'");
        assert_eq!(tokens[0].token_type, TokenType::Char);
    }

    #[test]
    fn test_unterminated_string() {
        let tokens = lex_all(r#""unterminated"#);
        assert_eq!(tokens[0].token_type, TokenType::Error);
    }

    #[test]
    fn test_unterminated_char() {
        let tokens = lex_all(r"'a");
        assert_eq!(tokens[0].token_type, TokenType::Error);
    }

    #[test]
    fn test_line_tracking() {
        let tokens = lex_all("x\ny\nz");
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[2].line, 3);
    }

    #[test]
    fn test_comment_skipping() {
        let tokens = lex_all("x // comment\ny");
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "x".into());
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "y".into());
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_whitespace_skipping() {
        let tokens = lex_all("  x  \t  y  ");
        assert_eq!(tokens[0].lexeme, "x".into());
        assert_eq!(tokens[1].lexeme, "y".into());
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_lambda_keyword() {
        let tokens_lambda = lex_all("lambda");
        assert_eq!(tokens_lambda[0].token_type, TokenType::Lambda);
    }

    #[test]
    fn test_parenthesis_brace_combo() {
        let tokens = lex_all("({})");
        assert_eq!(tokens[0].token_type, TokenType::LeftParenBrace);
        assert_eq!(tokens[1].token_type, TokenType::RightParenBrace);
    }

    #[test]
    fn test_complex_expression() {
        let tokens = lex_all("let x: int = 42; x + 10");
        assert_eq!(tokens[0].token_type, TokenType::Var); // let
        assert_eq!(tokens[1].lexeme, "x".into());
        assert_eq!(tokens[2].token_type, TokenType::Colon);
        assert_eq!(tokens[3].token_type, TokenType::SimpleType); // int
        assert_eq!(tokens[4].token_type, TokenType::Equal);
        assert_eq!(tokens[5].token_type, TokenType::Number); // 42
        assert_eq!(tokens[6].token_type, TokenType::Semicolon);
    }

    #[test]
    fn test_multiple_operators() {
        let tokens = lex_all("a <= b && c >= d");
        assert_eq!(tokens[0].lexeme, "a".into());
        assert_eq!(tokens[1].token_type, TokenType::LessEqual);
        assert_eq!(tokens[2].lexeme, "b".into());
        assert_eq!(tokens[3].token_type, TokenType::Amp);
        assert_eq!(tokens[4].token_type, TokenType::Amp);
        assert_eq!(tokens[5].lexeme, "c".into());
        assert_eq!(tokens[6].token_type, TokenType::GreaterEqual);
        assert_eq!(tokens[7].lexeme, "d".into());
    }

    #[test]
    fn test_all_simple_types() {
        let simple_types = vec!["int", "uint", "float", "bool", "char", "nil"];
        for simple_type in simple_types {
            let tokens = lex_all(simple_type);
            assert_eq!(tokens[0].token_type, TokenType::SimpleType);
        }
    }

    #[test]
    fn test_underscore_in_identifier() {
        let tokens = lex_all("_private_var");
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "_private_var".into());
    }

    #[test]
    fn test_type_keyword() {
        let tokens = lex_all("type MyType");
        assert_eq!(tokens[0].token_type, TokenType::Type);
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "MyType".into());
    }
}
