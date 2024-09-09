use std::{cell::RefCell, collections::HashMap, fmt::Debug};

mod to_str;

use crate::{
    lexer::{Token, TokenType},
    prelude::shared_strings::SharedString,
    typing::{CustomStruct, UValueType, ValueType},
    value::Value,
};

pub trait NodeVisitor<T> {
    fn visit_statement(&self, statement: &Statement) -> T;
    fn visit_expression(&self, expression: &Expression) -> T;
    fn visit_declaration(&self, declaration: &Declaration) -> T;
}

pub trait StatementVisitor<T> {
    // fn visit_print_statement(&self, statement: &PrintStatement) -> T;
    fn visit_block_statement(&self, statement: &BlockStatement) -> T;
    fn visit_if_statement(&self, statement: &IfStatement) -> T;
    fn visit_while_statement(&self, statement: &WhileStatement) -> T;
    fn visit_for_statement(&self, statement: &ForStatement) -> T;
    fn visit_break_statement(&self, statement: &BreakStatement) -> T;
    fn visit_continue_statement(&self, statement: &ContinueStatement) -> T;
    fn visit_return_statement(&self, statement: &ReturnStatement) -> T;
    fn visit_expression_statement(&self, statement: &ExpressionStatement) -> T;
}

pub trait ExpressionVisitor<T> {
    fn visit_literal_expression(&self, expression: &LiteralExpression) -> T;
    fn visit_string_literal_expression(&self, expression: &StringLiteralExpression) -> T;
    fn visit_unary_expression(&self, expression: &UnaryExpression) -> T;
    fn visit_deref_expression(&self, expression: &DerefExpression) -> T;
    fn visit_ref_expression(&self, expression: &RefExpression) -> T;
    fn visit_index_expression(&self, expression: &IndexExpression) -> T;
    fn visit_binary_expression(&self, expression: &BinaryExpression) -> T;
    fn visit_ternary_expression(&self, expression: &TernaryExpression) -> T;
    fn visit_variable_expression(&self, expression: &VariableExpression) -> T;
    fn visit_assign_expression(&self, expression: &AssignExpression) -> T;
    fn visit_logical_expression(&self, expression: &LogicalExpression) -> T;
    fn visit_call_expression(&self, expression: &CallExpression) -> T;
    fn visit_dot_expression(&self, expression: &DotExpression) -> T;
    fn visit_function_expression(&self, expression: &FunctionExpression) -> T;
    fn visit_cast_expression(&self, expression: &CastExpression) -> T;
    fn visit_struct_initializer_expression(&self, expression: &StructInitializerExpression) -> T;
    fn visit_sizeof_expression(&self, expression: &SizeofExpression) -> T;
}

pub trait DeclarationVisitor<T> {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> T;
    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> T;
}

pub trait Acceptor<T, V> {
    fn accept(&self, visitor: &V) -> T;
}

#[derive(Clone)]
pub enum ASTNode {
    Empty,
    Statement(Statement),
    Expression(Expression),
    Declaration(Declaration),
}

impl<T, V> Acceptor<T, V> for ASTNode
where
    V: NodeVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        match self {
            ASTNode::Empty => panic!("Empty node should not be visited"),
            ASTNode::Statement(s) => visitor.visit_statement(s),
            ASTNode::Expression(e) => visitor.visit_expression(e),
            ASTNode::Declaration(d) => visitor.visit_declaration(d),
        }
    }
}

impl ASTNode {
    pub fn to_string(&self) -> String {
        self.accept(&to_str::ToStrVisitor::new(&RefCell::new([false; 100]), 0, None))
    }
}

impl Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone)]
pub enum Statement {
    // Print(PrintStatement),
    Block(BlockStatement),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl<T, V> Acceptor<T, V> for Statement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        match self {
            // Statement::Print(s) => s.accept(visitor),
            Statement::Block(s) => s.accept(visitor),
            Statement::If(s) => s.accept(visitor),
            Statement::While(s) => s.accept(visitor),
            Statement::For(s) => s.accept(visitor),
            Statement::Break(s) => s.accept(visitor),
            Statement::Continue(s) => s.accept(visitor),
            Statement::Return(s) => s.accept(visitor),
            Statement::Expression(s) => s.accept(visitor),
        }
    }
}

impl Statement {
    pub fn as_node(&self) -> ASTNode {
        ASTNode::Statement(self.clone())
    }
}

// #[derive(Clone)]
// pub struct PrintStatement {
//     pub expr: Box<Expression>,
//     pub line_no: usize,
// }

// impl<T, V> Acceptor<T, V> for PrintStatement
// where
//     V: StatementVisitor<T>,
// {
//     fn accept(&self, visitor: &V) -> T {
//         visitor.visit_print_statement(self)
//     }
// }

#[derive(Clone)]
pub struct BlockStatement {
    pub statements: Vec<ASTNode>,
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for BlockStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_block_statement(self)
    }
}

#[derive(Clone)]
pub struct IfStatement {
    pub condition: Box<Expression>,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for IfStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_if_statement(self)
    }
}

#[derive(Clone)]
pub struct WhileStatement {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for WhileStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_while_statement(self)
    }
}

#[derive(Clone)]
pub struct ForStatement {
    pub init: Option<Box<ASTNode>>,
    pub condition: Option<Box<Expression>>,
    pub increment: Option<Box<Expression>>,
    pub body: Box<Statement>,
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for ForStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_for_statement(self)
    }
}

#[derive(Clone)]
pub struct BreakStatement {
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for BreakStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_break_statement(self)
    }
}

#[derive(Clone)]
pub struct ContinueStatement {
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for ContinueStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_continue_statement(self)
    }
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub value: Option<Box<Expression>>,
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for ReturnStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_return_statement(self)
    }
}

#[derive(Clone)]
pub struct ExpressionStatement {
    pub expr: Box<Expression>,
    pub line_no: usize,
}

impl<T, V> Acceptor<T, V> for ExpressionStatement
where
    V: StatementVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_expression_statement(self)
    }
}

#[derive(Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    StringLiteral(StringLiteralExpression),
    Unary(UnaryExpression),
    Deref(DerefExpression),
    Ref(RefExpression),
    Index(IndexExpression),
    Binary(BinaryExpression),
    Ternary(TernaryExpression),
    Variable(VariableExpression),
    Assign(AssignExpression),
    Logical(LogicalExpression),
    Call(CallExpression),
    Dot(DotExpression),
    Function(FunctionExpression),
    Cast(CastExpression),
    StructInitializer(StructInitializerExpression),
    Sizeof(SizeofExpression),
    Empty,
}

impl Expression {
    pub fn eval_constexpr(&self) -> Option<Value> {
        match self {
            Expression::Literal(e) => Some(e.value.clone()),
            Expression::Unary(e) => {
                let operand = e.operand.eval_constexpr()?;
                
                match (e.operator, operand) {
                    (TokenType::Minus, Value::Integer(i)) => Some(Value::Integer(-i)),
                    (TokenType::Minus, Value::UInteger(i)) => Some(Value::Integer(-(i as i64))),
                    (TokenType::Bang, Value::Bool(b)) => Some(Value::Bool(!b)),
                    _ => None,
                }
            }
            Expression::Binary(e) => {
                let left = e.left.eval_constexpr()?;
                let right = e.right.eval_constexpr()?;
                match (e.operator, left, right) {
                    (TokenType::Plus, Value::Integer(l), Value::Integer(r)) => Some(Value::Integer(l + r)),
                    (TokenType::Minus, Value::Integer(l), Value::Integer(r)) => Some(Value::Integer(l - r)),
                    (TokenType::Star, Value::Integer(l), Value::Integer(r)) => Some(Value::Integer(l * r)),
                    (TokenType::Slash, Value::Integer(l), Value::Integer(r)) => Some(Value::Integer(l / r)),
                    (TokenType::Plus, Value::UInteger(l), Value::UInteger(r)) => Some(Value::UInteger(l + r)),
                    (TokenType::Minus, Value::UInteger(l), Value::UInteger(r)) => Some(Value::UInteger(l - r)),
                    (TokenType::Star, Value::UInteger(l), Value::UInteger(r)) => Some(Value::UInteger(l * r)),
                    (TokenType::Slash, Value::UInteger(l), Value::UInteger(r)) => Some(Value::UInteger(l / r)),
                    (TokenType::Plus, Value::Float(l), Value::Float(r)) => Some(Value::Float(l + r)),
                    (TokenType::Minus, Value::Float(l), Value::Float(r)) => Some(Value::Float(l - r)),
                    (TokenType::Star, Value::Float(l), Value::Float(r)) => Some(Value::Float(l * r)),
                    (TokenType::Slash, Value::Float(l), Value::Float(r)) => Some(Value::Float(l / r)),
                    (TokenType::Greater, Value::Integer(l), Value::Integer(r)) => Some(Value::Bool(l > r)),
                    (TokenType::GreaterEqual, Value::Integer(l), Value::Integer(r)) => Some(Value::Bool(l >= r)),
                    (TokenType::Less, Value::Integer(l), Value::Integer(r)) => Some(Value::Bool(l < r)),
                    (TokenType::LessEqual, Value::Integer(l), Value::Integer(r)) => Some(Value::Bool(l <= r)),
                    (TokenType::EqualEqual, Value::Integer(l), Value::Integer(r)) => Some(Value::Bool(l == r)),
                    (TokenType::BangEqual, Value::Integer(l), Value::Integer(r)) => Some(Value::Bool(l != r)),
                    (TokenType::Greater, Value::UInteger(l), Value::UInteger(r)) => Some(Value::Bool(l > r)),
                    (TokenType::GreaterEqual, Value::UInteger(l), Value::UInteger(r)) => Some(Value::Bool(l >= r)),
                    (TokenType::Less, Value::UInteger(l), Value::UInteger(r)) => Some(Value::Bool(l < r)),
                    (TokenType::LessEqual, Value::UInteger(l), Value::UInteger(r)) => Some(Value::Bool(l <= r)),
                    (TokenType::EqualEqual, Value::UInteger(l), Value::UInteger(r)) => Some(Value::Bool(l == r)),
                    (TokenType::BangEqual, Value::UInteger(l), Value::UInteger(r)) => Some(Value::Bool(l != r)),
                    (TokenType::Greater, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l > r)),
                    (TokenType::GreaterEqual, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l >= r)),
                    (TokenType::Less, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l < r)),
                    (TokenType::LessEqual, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l <= r)),
                    (TokenType::EqualEqual, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l == r)),
                    (TokenType::BangEqual, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l != r)),
                    (TokenType::And, Value::Bool(l), Value::Bool(r)) => Some(Value::Bool(l && r)),
                    (TokenType::Or, Value::Bool(l), Value::Bool(r)) => Some(Value::Bool(l || r)),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct LiteralExpression {
    pub value: Value,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct StringLiteralExpression {
    pub value: SharedString,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct UnaryExpression {
    pub operator: TokenType,
    pub operand: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct DerefExpression {
    pub operand: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct RefExpression {
    pub operand: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct IndexExpression {
    pub array: Box<Expression>,
    pub index: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: TokenType,
    pub right: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct TernaryExpression {
    pub condition: Box<Expression>,
    pub then_branch: Box<Expression>,
    pub else_branch: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct VariableExpression {
    pub token: Token,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct AssignExpression {
    pub variable: Box<Expression>,
    pub value: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct LogicalExpression {
    pub left: Box<Expression>,
    pub operator: TokenType,
    pub right: Box<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct DotExpression {
    pub object: Box<Expression>,
    pub field: SharedString,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct CastExpression {
    pub expression: Box<Expression>,
    pub target_type: UValueType,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct StructInitializerExpression {
    pub struct_type: CustomStruct,
    pub fields: HashMap<SharedString, Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct SizeofExpression {
    pub tipe: UValueType,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct Prototype {
    pub name: SharedString,
    pub captures: Vec<SharedString>,
    pub params: Vec<(SharedString, UValueType, bool)>,
    pub return_type: UValueType,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct FunctionExpression {
    pub prototype: Prototype,
    pub body: Option<Vec<ASTNode>>,
    pub line_no: usize,
}

impl FunctionExpression {
    pub fn new(
        captures: Vec<SharedString>,
        params: Vec<(SharedString, UValueType, bool)>,
        body: Option<Vec<ASTNode>>,
        return_type: UValueType,
        name: SharedString,
    ) -> FunctionExpression {
        FunctionExpression {
            prototype: Prototype {
                name,
                captures,
                params,
                return_type,
                line_no: 0,
            },
            body,
            line_no: 0,
        }
    }
}

impl Default for FunctionExpression {
    fn default() -> Self {
        FunctionExpression {
            prototype: Prototype {
                name: "".into(),
                captures: Vec::new(),
                params: Vec::new(),
                return_type: ValueType::Nil.intern(),
                line_no: 0,
            },
            body: None,
            line_no: 0,
        }
    }
}

// Expression
impl<T, V> Acceptor<T, V> for Expression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        match self {
            Expression::Literal(e) => visitor.visit_literal_expression(e),
            Expression::StringLiteral(e) => visitor.visit_string_literal_expression(e),
            Expression::Unary(e) => visitor.visit_unary_expression(e),
            Expression::Deref(e) => visitor.visit_deref_expression(e),
            Expression::Ref(e) => visitor.visit_ref_expression(e),
            Expression::Index(e) => visitor.visit_index_expression(e),
            Expression::Binary(e) => visitor.visit_binary_expression(e),
            Expression::Ternary(e) => visitor.visit_ternary_expression(e),
            Expression::Variable(e) => visitor.visit_variable_expression(e),
            Expression::Assign(e) => visitor.visit_assign_expression(e),
            Expression::Logical(e) => visitor.visit_logical_expression(e),
            Expression::Call(e) => visitor.visit_call_expression(e),
            Expression::Dot(e) => visitor.visit_dot_expression(e),
            Expression::Function(e) => visitor.visit_function_expression(e),
            Expression::Cast(e) => visitor.visit_cast_expression(e),
            Expression::StructInitializer(e) => visitor.visit_struct_initializer_expression(e),
            Expression::Sizeof(e) => visitor.visit_sizeof_expression(e),
            Expression::Empty => panic!("Empty expression should not be visited"),
        }
    }
}

impl Expression {
    pub fn as_node(&self) -> ASTNode {
        ASTNode::Expression(self.clone())
    }
}

// LiteralExpression
impl<T, V> Acceptor<T, V> for LiteralExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_literal_expression(self)
    }
}

// StringLiteralExpression
impl<T, V> Acceptor<T, V> for StringLiteralExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_string_literal_expression(self)
    }
}

// UnaryExpression
impl<T, V> Acceptor<T, V> for UnaryExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_unary_expression(self)
    }
}

// DerefExpression
impl<T, V> Acceptor<T, V> for DerefExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_deref_expression(self)
    }
}

// RefExpression
impl<T, V> Acceptor<T, V> for RefExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_ref_expression(self)
    }
}

// IndexExpression
impl<T, V> Acceptor<T, V> for IndexExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_index_expression(self)
    }
}

// BinaryExpression
impl<T, V> Acceptor<T, V> for BinaryExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_binary_expression(self)
    }
}

// TernaryExpression
impl<T, V> Acceptor<T, V> for TernaryExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_ternary_expression(self)
    }
}

// VariableExpression
impl<T, V> Acceptor<T, V> for VariableExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_variable_expression(self)
    }
}

// AssignExpression
impl<T, V> Acceptor<T, V> for AssignExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_assign_expression(self)
    }
}

// LogicalExpression
impl<T, V> Acceptor<T, V> for LogicalExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_logical_expression(self)
    }
}

// CallExpression
impl<T, V> Acceptor<T, V> for CallExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_call_expression(self)
    }
}

// DotExpression
impl<T, V> Acceptor<T, V> for DotExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_dot_expression(self)
    }
}

// FunctionExpression
impl<T, V> Acceptor<T, V> for FunctionExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_function_expression(self)
    }
}

// CastExpression
impl<T, V> Acceptor<T, V> for CastExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_cast_expression(self)
    }
}

// StructInitializerExpression
impl<T, V> Acceptor<T, V> for StructInitializerExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_struct_initializer_expression(self)
    }
}

// SizeofExpression
impl<T, V> Acceptor<T, V> for SizeofExpression
where
    V: ExpressionVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_sizeof_expression(self)
    }
}

#[derive(Clone)]
pub enum Declaration {
    Var(VarDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Clone)]
pub struct VarDeclaration {
    pub name: SharedString,
    pub tipe: UValueType,
    pub initializer: Option<Box<Expression>>,
    pub mutable: bool,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct ArrayDeclaration {
    pub name: SharedString,
    pub elements: Vec<Expression>,
    pub elem_tipe: Option<UValueType>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub prototype: Prototype,
    pub line_no: usize,
    pub body: Option<Vec<ASTNode>>
}

// Declaration
impl<T, V> Acceptor<T, V> for Declaration
where
    V: DeclarationVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        match self {
            Declaration::Var(d) => d.accept(visitor),
            Declaration::Function(d) => d.accept(visitor),
        }
    }
}

impl Declaration {
    #[allow(dead_code)]
    pub fn as_node(&self) -> ASTNode {
        ASTNode::Declaration(self.clone())
    }
}

// VarDeclaration
impl<T, V> Acceptor<T, V> for VarDeclaration
where
    V: DeclarationVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_var_declaration(self)
    }
}

// FunctionDeclaration
impl<T, V> Acceptor<T, V> for FunctionDeclaration
where
    V: DeclarationVisitor<T>,
{
    fn accept(&self, visitor: &V) -> T {
        visitor.visit_function_declaration(self)
    }
}
