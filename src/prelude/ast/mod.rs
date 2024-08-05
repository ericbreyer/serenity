use std::{collections::HashMap, fmt::Debug};
mod to_str;

use crate::{
    lexer::{Token, TokenType},
    typing::{CustomStruct, UValueType, ValueType},
    value::Value,
    prelude::shared_strings::SharedString
};

pub trait NodeVisitor<T> {
    fn visit_statement(self, statement: &Statement) -> T;
    fn visit_expression(self, expression: &Expression) -> T;
    fn visit_declaration(self, declaration: &Declaration) -> T;
}

trait Node {
    fn accept<T>(&self, visitor: impl NodeVisitor<T>) -> T;
}

#[derive(Clone)]
pub enum ASTNode {
    Empty,
    Statement(Statement),
    Expression(Expression),
    Declaration(Declaration),
    CallMain(Box<ASTNode>),
}

impl Node for ASTNode {
    fn accept<T>(&self, visitor: impl NodeVisitor<T>) -> T{
        match self {
            ASTNode::Empty => panic!("Empty node should not be visited"),
            ASTNode::Statement(s) => s.accept(visitor),
            ASTNode::Expression(e) => e.accept(visitor),
            ASTNode::Declaration(d) => d.accept(visitor),
            ASTNode::CallMain(c) => c.accept(visitor),
        }
    }
}

impl ASTNode {
    pub fn to_string(&self) -> String {
        self.accept(to_str::ToStrVisitor::new(&mut [false; 100], 0, None))
    }
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
    If(
        Box<Expression>,
        Box<Statement>,
        Option<Box<Statement>>,
        usize,
    ),
    While(Box<Expression>, Box<Statement>, usize),
    For(
        Option<Box<ASTNode>>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
        Box<Statement>,
        usize,
    ),
    Break(usize),
    Continue(usize),
    Return(Option<Box<Expression>>, usize),
    Expression(Box<Expression>, usize),
}

impl Node for Statement {
    fn accept<T>(&self, visitor: impl NodeVisitor<T>) -> T {
        visitor.visit_statement(self)
    }
}

#[derive(Clone)]
pub enum Expression {
    Literal(Value, usize),
    StringLiteral(SharedString, usize),
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
    Cast(Box<Expression>, Option<UValueType>, usize),
    StructInitializer(CustomStruct, HashMap<SharedString, Expression>, usize),
    // Pipe(Box<Expression>, usize), // Sudo expression for pipe operator
    // PartialApplication(Box<Expression>, Vec<Expression>, usize), // Sudo expression for partial application
    Empty,
}

impl Node for Expression {
    fn accept<T>(&self, visitor: impl NodeVisitor<T>) -> T {
        visitor.visit_expression(self)
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", ASTNode::Expression(self.clone()))
    }
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
            Expression::Cast(_, _, l) => *l,
            Expression::StructInitializer(_, _, l) => *l,
            // Expression::Pipe(_, l) => *l,
            // Expression::PartialApplication(_, _, l) => *l,
            Expression::Empty => 0,
        }
    }
}

#[derive(Clone)]
pub struct FunctionExpression {
    pub captures: Vec<SharedString>,
    pub params: Vec<(SharedString, UValueType, bool)>,
    pub body: Vec<ASTNode>,
    pub return_type: UValueType,
    pub name: SharedString,
}

impl FunctionExpression {
    pub fn new(
        captures: Vec<SharedString>,
        params: Vec<(SharedString, UValueType, bool)>,
        body: Vec<ASTNode>,
        return_type: UValueType,
        name: SharedString,
    ) -> FunctionExpression {
        FunctionExpression {
            captures,
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
            captures: vec![],
            params: vec![],
            body: vec![],
            return_type: ValueType::Nil.intern(),
            name: "".into(),
        }
    }
}

#[derive(Clone)]
pub enum Declaration {
    Var(VarDeclaration, usize),
    Function(FunctionDeclaration, usize),
    // Struct(StructDeclaration, usize),
    Array(ArrayDeclaration, usize),
}

impl Node for Declaration {
    fn accept<T>(&self, visitor: impl NodeVisitor<T>) -> T {
        visitor.visit_declaration(self)
    }
}

#[derive(Clone)]
pub struct VarDeclaration {
    pub name: SharedString,
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
    pub name: SharedString,
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
    pub name: SharedString,
    pub body: FunctionExpression,
    pub weak: bool,
}

impl Debug for FunctionDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// #[derive(Clone)]
// pub struct StructDeclaration {
//     pub s: CustomStruct,
// }

// impl Debug for StructDeclaration {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.s.name)
//     }
// }
