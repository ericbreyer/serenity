use std::rc::Rc;

use crate::value::value_type::{CustomStruct, ValueTypeK, ValueTypeSet};

use super::{Parser, Precedence, ParseRule};



impl Parser {
    pub fn init_parse_table(&mut self) {

    let type_set_all: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::All.intern(),
    ]));
    let type_set_none: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![]));
    let type_set_func: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::AnyFunction.intern(),
    ]));
    let type_set_num: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::Float.intern(),
        ValueTypeK::Integer.intern(),
    ]));
    let type_set_add: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::Float.intern(),
        ValueTypeK::Integer.intern(),
        ValueTypeK::String.intern(),
    ]));
    let type_set_bool: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::Bool.intern(),
    ]));
    let type_set_ptr: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::Pointer(ValueTypeK::All.intern(), false).intern(),
        ValueTypeK::Array(ValueTypeK::All.intern(), usize::MAX).intern(),
        ValueTypeK::String.intern(),
    ]));
    let type_set_struct: Rc<ValueTypeSet> = Rc::new(ValueTypeSet::new(vec![
        ValueTypeK::AnyStruct.intern(),
    ]));

    let type_set_rules: [ParseRule; 53] = [
        ParseRule {
            prefix: Some(Parser::grouping),
            infix: Some(Parser::call),
            left_type: Rc::clone(&type_set_func),
            precedence: Precedence::Call,
        }, // LeftParen
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // RightParen
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // LeftBrace
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // RightBrace
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Comma
        ParseRule {
            prefix: None,
            infix: Some(Parser::dot),
            left_type: Rc::clone(&type_set_struct),
            precedence: Precedence::Call,
        }, // Dot
        ParseRule {
            prefix: Some(Parser::unary),
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Term,
        }, // Minus
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_add),
            precedence: Precedence::Term,
        }, // Plus
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Semicolon
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Factor,
        }, // Slash
        ParseRule {
            prefix: Some(Parser::deref),
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Factor,
        }, // Star
        ParseRule {
            prefix: Some(Parser::unary),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Bang
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_all),
            precedence: Precedence::Equality,
        }, // BangEqual
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Equal
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_all),
            precedence: Precedence::Equality,
        }, // EqualEqual
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Comparison,
        }, // Greater
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Comparison,
        }, // GreaterEqual
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Comparison,
        }, // Less
        ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            left_type: Rc::clone(&type_set_num),
            precedence: Precedence::Comparison,
        }, // LessEqual
        ParseRule {
            prefix: Some(Parser::variable),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Identifier
        ParseRule {
            prefix: Some(Parser::string),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // String
        ParseRule {
            prefix: Some(Parser::number),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Number
        ParseRule {
            prefix: None,
            infix: Some(Parser::and_),
            left_type: Rc::clone(&type_set_bool),
            precedence: Precedence::And,
        }, // And
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Class
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Else
        ParseRule {
            prefix: Some(Parser::literal),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // False
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Fun
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // For
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // If
        ParseRule {
            prefix: Some(Parser::literal),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Nil
        ParseRule {
            prefix: None,
            infix: Some(Parser::or_),
            left_type: Rc::clone(&type_set_bool),
            precedence: Precedence::Or,
        }, // Or
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Print
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Return
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Super
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_func),
            precedence: Precedence::None,
        }, // This
        ParseRule {
            prefix: Some(Parser::literal),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // True
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_func),
            precedence: Precedence::None,
        }, // Var
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_func),
            precedence: Precedence::None,
        }, // While
        ParseRule {
            prefix: None,
            infix: Some(Parser::ternary),
            left_type: Rc::clone(&type_set_bool),
            precedence: Precedence::Ternary,
        }, // QuestionMark
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Colon
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Const
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Break
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Continue
        ParseRule {
            prefix: Some(Parser::lambda),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Lambda
        ParseRule {
            prefix: Some(Parser::addr_of),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Amp
        ParseRule {
            prefix: None,
            infix: Some(Parser::index),
            left_type: Rc::clone(&type_set_ptr),
            precedence: Precedence::Call,
        }, // LeftBracket
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // RightBracket
        ParseRule {
            prefix: None,
            infix: Some(Parser::deref_dot),
            left_type: Rc::clone(&type_set_ptr),
            precedence: Precedence::Call,
        }, // RightArrow
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // SimpleType
        ParseRule {
            prefix: Some(Parser::cast),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Cast
        ParseRule {
            prefix: Some(Parser::char),
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Char
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // EOF
        ParseRule {
            prefix: None,
            infix: None,
            left_type: Rc::clone(&type_set_none),
            precedence: Precedence::None,
        }, // Error
    ];

    self.parse_table = type_set_rules;
    }}