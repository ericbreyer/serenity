use std::{collections::HashMap, fmt::Debug};
mod to_str;
use to_str::ToStr;

use crate::{
    lexer::{Token, TokenType},
    value::Value,
    typing::{CustomStruct, ValueType, ValueTypeK},
};

#[derive(Clone)]
pub enum ASTNode {
    Err,
    Module(Vec<ASTNode>),
    Statement(Statement),
    Expression(Expression),
    Declaration(Declaration),
    Token(Token),
}

impl Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(0))
    }
}

#[derive(Clone)]
pub enum Statement {
    Print(Box<Expression>),
    Block(Vec<ASTNode>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    For(
        Option<Box<ASTNode>>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
    ),
    Break,
    Continue,
    Return(Option<Box<Expression>>),
    Expression(Box<Expression>),
}

#[derive(Clone)]
pub enum Expression {
    Literal(Value),
    StringLiteral(String),
    Grouping(Box<Expression>),
    Unary(TokenType, Box<Expression>),
    Deref(Box<Expression>),
    Ref(Box<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Binary(Box<Expression>, TokenType, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Variable(Token),
    Assign(Box<Expression>, Box<Expression>),
    Logical(Box<Expression>, TokenType, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Dot(Box<Expression>, Token),
    Set(Box<Expression>, Token, Box<Expression>),
    This(Token),
    Super(Token, Token),
    Function(FunctionExpression),
    Cast(Box<Expression>, ValueType),
    ArrayLiteral(Vec<Expression>),
    StructInitializer(CustomStruct, HashMap<String, Expression>),
    Empty,
    Nil,
}

pub enum HalfExpression {
    Nil,
    Binary(TokenType, Box<Expression>),
    Dot(Token),
    DerefDot(Token),
    And(Box<Expression>),
    Or(Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>),
    Index(Box<Expression>),
    Call(Vec<Expression>),
    Assign(Box<Expression>),
}

impl HalfExpression {
    pub fn fill(self, left: Expression) -> Expression {
        match self {
            HalfExpression::Nil => Expression::Nil,
            HalfExpression::Binary(t, r) => Expression::Binary(Box::new(left), t, r),
            HalfExpression::Dot(t) => Expression::Dot(Box::new(left), t),
            HalfExpression::And(r) => Expression::Logical(Box::new(left), TokenType::And, r),
            HalfExpression::Or(r) => Expression::Logical(Box::new(left), TokenType::Or, r),
            HalfExpression::Ternary(l, r) => Expression::Ternary(Box::new(left), l, r),
            HalfExpression::Index(r) => Expression::Index(Box::new(left), r),
            HalfExpression::Call(r) => Expression::Call(Box::new(left), r),
            HalfExpression::Assign(r) => Expression::Assign(Box::new(left), r),
            HalfExpression::DerefDot(t) => Expression::Dot(Box::new(Expression::Deref(Box::new(left))), t),
        }
    }
}

#[derive(Clone)]
pub struct FunctionExpression {
    pub params: Vec<(String, ValueType)>,
    pub body: Vec<ASTNode>,
    pub return_type: ValueType,
    pub name: String,
}

impl FunctionExpression {
    pub fn new(
        params: Vec<(String, ValueType)>,
        body: Vec<ASTNode>,
        return_type: ValueType,
        name: String,
    ) -> FunctionExpression {
        FunctionExpression {
            params,
            body,
            return_type,
            name
        }
    }
}

impl Default for FunctionExpression {
    fn default() -> Self {
        FunctionExpression {
            params: vec![],
            body: vec![],
            return_type: ValueTypeK::Nil.intern(),
            name: String::new(),
        }
    }
}

#[derive(Clone)]
pub enum Declaration {
    Var(VarDeclaration),
    Function(FunctionDeclaration),
    Struct(StructDeclaration),
    Array(ArrayDeclaration),
}

#[derive(Clone)]
pub struct VarDeclaration {
    pub name: String,
    pub tipe: Option<ValueType>,
    pub initializer: Option<Box<Expression>>,
    pub mutable: bool,
    pub line: usize,
}

impl Debug for VarDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone)]
pub struct ArrayDeclaration {
    pub name: String,
    pub elements: Vec<Expression>,
    pub elem_tipe: Option<ValueType>,
    pub line: usize,
}

impl Debug for ArrayDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub body: FunctionExpression,
    pub line: usize,
}

impl Debug for FunctionDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone)]
pub struct StructDeclaration {
    pub s: CustomStruct,
}

impl Debug for StructDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.s.name)
    }
}