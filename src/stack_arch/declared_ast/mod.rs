use std::{ collections::HashMap, fmt::Debug };
mod to_str;
pub use to_str::ToStr;


use crate::{
    lexer::{ Token, TokenType },
    typing::{ CustomStruct, UValueType, ValueType },
    value::Value,
};

#[derive(Clone)]
pub enum NDASTNode {
    Err,
    Module(SharedString, Vec<NDASTNode>),
    Statement(Statement),
    Expression(Expression),
    Declaration(Declaration),
    Group(Vec<NDASTNode>),
}
impl Debug for NDASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone)]
pub struct Local {
    pub name: SharedString,
    pub depth: i32,
    pub local_type: UValueType,
  }

impl Debug for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}: {:?}", self.name, self.local_type)
    }
  }
  

#[derive(Clone)]
pub enum Statement {
    Print(Box<Expression>, usize),
    Block(Vec<NDASTNode>, HashMap<usize, Local>, usize),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>, usize),
    While(Box<Expression>, Box<Statement>, usize),
    For(HashMap<usize, Local>, Option<Box<NDASTNode>>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Statement>, usize),
    Break(usize),
    Continue(usize),
    Return(Option<Box<Expression>>, usize),
    Expression(Box<Expression>, usize),
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
    Cast(Box<Expression>, Option<UValueType>, UValueType, usize),
    StructInitializer(CustomStruct, HashMap<SharedString, Expression>, usize),

    Empty,
}

impl Expression {
    pub fn line_no(&self) -> usize {
        match self {
           Expression::Literal(LiteralExpression{ value: _, line_no:  l}) => *l,
            Expression::StringLiteral(_, l) => *l,
            Expression::Unary(_, _, l) => *l,
            Expression::Deref(DerefExpression{ operand: _, line_no:  l}) => *l,
            Expression::Ref(_, l) => *l,
           Expression::Index(IndexExpression{ array: _, index:  _, line_no:  l}) => *l,
           Expression::Binary(BinaryExpression{ left: _, operator:  _, right:  _, line_no:  l}) => *l,
           Expression::Ternary(TernaryExpression{ condition: _, then_branch:  _, else_branch:  _, line_no:  l}) => *l,
            Expression::Variable(_, l) => *l,
           Expression::Assign(AssignExpression{ variable: _, value:  _, line_no:  l}) => *l,
           Expression::Logical(LogicalExpression{ left: _, operator:  _, right:  _, line_no:  l}) => *l,
           Expression::Call(CallExpression{ callee: _, arguments:  _, line_no:  l}) => *l,
           Expression::Dot(DotExpression{ object: _, field:  _, line_no:  l}) => *l,
            Expression::Function(_, l) => *l,
            Expression::Cast(_, _, _, l) => *l,
            Expression::StructInitializer(_, _, l) => *l,
            Expression::Empty => 0,
        }
    }
}

#[derive(Clone)]
pub struct FunctionExpression {
    pub captures: Vec<SharedString>,
    pub params: Vec<(SharedString, UValueType, bool)>,
    pub body: Vec<NDASTNode>,
    pub return_type: UValueType,
    pub name: SharedString,
    pub locals: HashMap<usize, Local>,
}

impl Default for FunctionExpression {
    fn default() -> Self {
        FunctionExpression {
            captures: vec![],
            params: vec![],
            body: vec![],
            return_type: ValueType::Nil.intern(),
            name: String,
            locals: HashMap::new(),
        }
    }
}


#[derive(Clone)]
pub enum Declaration {
  Var(VarDeclaration, usize),
  Function(FunctionDeclaration, usize),
  Array(ArrayDeclaration, usize),
  Empty,
}

#[derive(Clone)]
pub struct VarDeclaration {
  pub name: SharedString,
  pub tipe: UValueType,
  pub initializer: Option<Box<Expression>>,
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
  pub elem_tipe: UValueType,
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
}

impl Debug for FunctionDeclaration {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}