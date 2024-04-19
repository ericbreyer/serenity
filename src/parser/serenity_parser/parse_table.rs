use super::{ParseRule, SerenityParser, Precedence};

impl SerenityParser {
    pub fn init_parse_table(&mut self) {
        
        let type_set_rules: [ParseRule; 56] = [
            ParseRule {
                prefix: Some(SerenityParser::grouping),
                infix: Some(SerenityParser::call),

                precedence: Precedence::Call,
            }, // LeftParen
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // RightParen
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // LeftBrace
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // RightBrace
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Comma
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
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Semicolon
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
            }, // String
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
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Struct
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Else
            ParseRule {
                prefix: Some(SerenityParser::literal),
                infix: None,

                precedence: Precedence::None,
            }, // False
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Fun
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // For
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // If
            ParseRule {
                prefix: Some(SerenityParser::literal),
                infix: None,

                precedence: Precedence::None,
            }, // Nil
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::or_),

                precedence: Precedence::Or,
            }, // Or
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Print
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Return
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Super
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // This
            ParseRule {
                prefix: Some(SerenityParser::literal),
                infix: None,

                precedence: Precedence::None,
            }, // True
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Var
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // While
            ParseRule {
                prefix: None,
                infix: Some(SerenityParser::ternary),

                precedence: Precedence::Ternary,
            }, // QuestionMark
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Colon
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Const
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Break
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Continue
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
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // RightBracket
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
                infix: Some(SerenityParser::pipe),
                precedence: Precedence::Call,
            }, // Pipe
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // LeftParenBrace
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // RightParenBrace
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // EOF
            ParseRule {
                prefix: None,
                infix: None,

                precedence: Precedence::None,
            }, // Error
        ];

        self.parse_table = type_set_rules;
    }
}