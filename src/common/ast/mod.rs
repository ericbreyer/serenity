use std::{ cell::Cell, collections::HashMap, fmt::Debug };
mod to_str;
use to_str::ToStr;


use crate::{
    lexer::{ Token, TokenType },
    typing::{ CustomStruct, UValueType, ValueType },
    value::Value,
};

#[derive(Clone)]
pub enum ASTNode {
    Err,
    Module(String, Vec<ASTNode>),
    Statement(Statement),
    Expression(Expression),
    Declaration(Declaration),
    CallMain(Box<ASTNode>),
}

impl Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone)]
pub enum Statement {
    Print(Box<Expression>, usize),
    Block(Vec<ASTNode>, usize),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>, usize),
    While(Box<Expression>, Box<Statement>, usize),
    For(Option<Box<ASTNode>>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Statement>, usize),
    Break(usize),
    Continue(usize),
    Return(Option<Box<Expression>>, usize),
    Expression(Box<Expression>, usize),
}

impl Statement {
    pub fn line_no(&self) -> usize {
        match self {
            Statement::Print(_, l) => *l,
            Statement::Block(_, l) => *l,
            Statement::If(_, _, _, l) => *l,
            Statement::While(_, _, l) => *l,
            Statement::For(_, _, _, _, l) => *l,
            Statement::Break(l) => *l,
            Statement::Continue(l) => *l,
            Statement::Return(_, l) => *l,
            Statement::Expression(_, l) => *l,
        }
    }
}

#[derive(Clone)]
pub enum Expression {
    Literal(Value, usize),
    StringLiteral(String, usize),
    Unary(TokenType, Box<Expression>, usize),
    Deref(Box<Expression>, usize),
    Ref(Box<Expression>, usize),
    Index(Box<Expression>, Box<Expression>, usize),
    Binary(Box<Expression>, TokenType, Box<Expression>, usize),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>, usize),
    Variable(Token, usize),
    Assign(Box<Expression>, Box<Expression>, usize),
    Logical(Box<Expression>, TokenType, Box<Expression>, usize),
    Call(Box<Expression>, Vec<Expression>, usize),
    Dot(Box<Expression>, Token, usize),
    Function(FunctionExpression, usize),
    Cast(Box<Expression>, UValueType, Cell<UValueType>, usize),
    ArrayLiteral(Vec<Expression>, usize),
    StructInitializer(CustomStruct, HashMap<String, Expression>, usize),
    Pipe(Box<Expression>, usize), // Sudo expression for pipe operator
    PartialApplication(Box<Expression>, Vec<Expression>, usize), // Sudo expression for partial application

    Empty,
}

impl Expression {
    pub fn line_no(&self) -> usize {
        match self {
            Expression::Literal(_, l) => *l,
            Expression::StringLiteral(_, l) => *l,
            Expression::Unary(_, _, l) => *l,
            Expression::Deref(_, l) => *l,
            Expression::Ref(_, l) => *l,
            Expression::Index(_, _, l) => *l,
            Expression::Binary(_, _, _, l) => *l,
            Expression::Ternary(_, _, _, l) => *l,
            Expression::Variable(_, l) => *l,
            Expression::Assign(_, _, l) => *l,
            Expression::Logical(_, _, _, l) => *l,
            Expression::Call(_, _, l) => *l,
            Expression::Dot(_, _, l) => *l,
            Expression::Function(_, l) => *l,
            Expression::Cast(_, _, _, l) => *l,
            Expression::ArrayLiteral(_, l) => *l,
            Expression::StructInitializer(_, _, l) => *l,
            Expression::Pipe(_, l) => *l,
            Expression::PartialApplication(_, _, l) => *l,
            Expression::Empty => 0,
        }
    }
}

pub enum HalfExpression {
    Binary(TokenType, Box<Expression>),
    Dot(Token),
    DerefDot(Token),
    And(Box<Expression>),
    Or(Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>),
    Index(Box<Expression>),
    Call(Vec<Expression>),
    Assign(Box<Expression>),
    Pipe,
}

impl HalfExpression {
    pub fn fill(self, left: Expression) -> Expression {
        let line = left.line_no();
        match self {
            HalfExpression::Binary(t, r) => Expression::Binary(Box::new(left), t, r, line),
            HalfExpression::And(r) => Expression::Logical(Box::new(left), TokenType::And, r, line),
            HalfExpression::Or(r) => Expression::Logical(Box::new(left), TokenType::Or, r, line),
            HalfExpression::Ternary(l, r) => Expression::Ternary(Box::new(left), l, r, line),
            HalfExpression::Index(r) => Expression::Index(Box::new(left), r, line),
            HalfExpression::Call(r) => {
                if let Expression::PartialApplication(ref a, ref b, l) = left {
                    let mut v = b.clone();
                    v.extend(r.iter().cloned());
                    return Expression::Call(Box::new(*a.clone()), v, l)
                }
                Expression::Call(Box::new(left), r, line)
            }
            HalfExpression::Assign(r) => Expression::Assign(Box::new(left), r, line),
            HalfExpression::DerefDot(t) => {
                if let Expression::Pipe(ref a, l) = left {
                    return Expression::PartialApplication(Box::new(Expression::Dot(Box::new(Expression::Deref(Box::new(*a.clone()), l)), t, l)), vec![*a.clone()], l) 
                }

                Expression::Dot(Box::new(Expression::Deref(Box::new(left), line)), t, line)
            }
            HalfExpression::Dot(t) => {
                if let Expression::Pipe(ref a, l) = left {
                    return Expression::PartialApplication(Box::new(Expression::Dot(Box::new(*a.clone()), t, l)), vec![Expression::Ref(a.clone(), l)], l) 
                }
                Expression::Dot(Box::new(left), t, line)
            }
            HalfExpression::Pipe => Expression::Pipe(Box::new(left), line),
        }
    }
}

#[derive(Clone)]
pub struct FunctionExpression {
    pub params: Vec<(String, UValueType, bool)>,
    pub body: Vec<ASTNode>,
    pub return_type: UValueType,
    pub name: String,
}

impl FunctionExpression {
    pub fn new(
        params: Vec<(String, UValueType, bool)>,
        body: Vec<ASTNode>,
        return_type: UValueType,
        name: String
    ) -> FunctionExpression {
        FunctionExpression {
            params,
            body,
            return_type,
            name,
        }
    }
}

impl Default for FunctionExpression {
    fn default() -> Self {
        FunctionExpression {
            params: vec![],
            body: vec![],
            return_type: ValueType::Nil.intern(),
            name: String::new(),
        }
    }
}

#[derive(Clone)]
pub enum Declaration {
    Var(VarDeclaration, usize),
    Function(FunctionDeclaration, usize),
    Struct(StructDeclaration, usize),
    Array(ArrayDeclaration, usize),
}

#[derive(Clone)]
pub struct VarDeclaration {
    pub name: String,
    pub tipe: Option<UValueType>,
    pub initializer: Option<Box<Expression>>,
    pub mutable: bool,
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
    pub elem_tipe: Option<UValueType>,
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
