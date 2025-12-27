use core::fmt;
use std::{cell::RefCell, fmt::Debug, rc::Rc};

mod to_str;

use indexmap::IndexMap;

use crate::{
    lexer::{Token, TokenType},
    prelude::shared_strings::SharedString,
    typing::{Constraint, CustomStruct, UValueType, ValueType},
    value_literals::Value,
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
    fn visit_double_colon_expression(&self, expression: &DoubleColonExpression) -> T;
}

pub trait DeclarationVisitor<T> {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> T;
    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> T;
}

pub trait Acceptor<T, V> {
    fn accept(&self, visitor: &V) -> T;
}

#[derive(Clone)]
pub struct Ast {
    pub roots: Vec<ASTNode>,
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in &self.roots {
            write!(f, "{:?}", node)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum ASTNode {
    Empty,
    Statement(Statement),
    Expression(Expression),
    Declaration(Declaration),
}

impl ASTNode {
    pub fn is_empty(&self) -> bool {
        matches!(self, ASTNode::Empty)
    }

    pub fn vec_of(&self) -> Vec<ASTNode> {
        if self.is_empty() {
            vec![]
        } else {
            vec![self.clone()]
        }
    }
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

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.accept(&to_str::ToStrVisitor::new(
                &RefCell::new([false; 100]),
                0,
                None
            ))
        )
    }
}

impl Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
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
    DoubleColon(DoubleColonExpression),
    Empty,
}

impl Expression {
    #[allow(clippy::too_many_lines)]
    pub fn eval_constexpr(&self) -> Option<Value> {
        match self {
            Expression::Literal(e) => Some(e.value),
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
                    (TokenType::Plus, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Integer(l + r))
                    }
                    (TokenType::Minus, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Integer(l - r))
                    }
                    (TokenType::Star, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Integer(l * r))
                    }
                    (TokenType::Slash, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Integer(l / r))
                    }
                    (TokenType::Plus, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::UInteger(l + r))
                    }
                    (TokenType::Minus, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::UInteger(l - r))
                    }
                    (TokenType::Star, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::UInteger(l * r))
                    }
                    (TokenType::Slash, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::UInteger(l / r))
                    }
                    (TokenType::Plus, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Float(l + r))
                    }
                    (TokenType::Minus, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Float(l - r))
                    }
                    (TokenType::Star, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Float(l * r))
                    }
                    (TokenType::Slash, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Float(l / r))
                    }
                    (TokenType::Greater, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Bool(l > r))
                    }
                    (TokenType::GreaterEqual, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Bool(l >= r))
                    }
                    (TokenType::Less, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Bool(l < r))
                    }
                    (TokenType::LessEqual, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Bool(l <= r))
                    }
                    (TokenType::EqualEqual, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Bool(l == r))
                    }
                    (TokenType::BangEqual, Value::Integer(l), Value::Integer(r)) => {
                        Some(Value::Bool(l != r))
                    }
                    (TokenType::Greater, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::Bool(l > r))
                    }
                    (TokenType::GreaterEqual, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::Bool(l >= r))
                    }
                    (TokenType::Less, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::Bool(l < r))
                    }
                    (TokenType::LessEqual, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::Bool(l <= r))
                    }
                    (TokenType::EqualEqual, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::Bool(l == r))
                    }
                    (TokenType::BangEqual, Value::UInteger(l), Value::UInteger(r)) => {
                        Some(Value::Bool(l != r))
                    }
                    (TokenType::Greater, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Bool(l > r))
                    }
                    (TokenType::GreaterEqual, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Bool(l >= r))
                    }
                    (TokenType::Less, Value::Float(l), Value::Float(r)) => Some(Value::Bool(l < r)),
                    (TokenType::LessEqual, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Bool(l <= r))
                    }
                    (TokenType::EqualEqual, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Bool(l == r))
                    }
                    (TokenType::BangEqual, Value::Float(l), Value::Float(r)) => {
                        Some(Value::Bool(l != r))
                    }
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
pub struct DoubleColonExpression {
    pub typ: UValueType,
    pub acessor: SharedString,
    pub line_no: usize,
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
    pub token: Rc<RefCell<Token>>,
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
    pub struct_type: Box<CustomStruct>,
    pub fields: IndexMap<SharedString, Expression>,
    pub line_no: usize,
}

#[derive(Clone)]
pub struct SizeofExpression {
    pub tipe: UValueType,
    pub line_no: usize,
}

#[derive(Clone, Debug)]
pub struct Prototype {
    pub name: SharedString,
    pub captures: Vec<SharedString>,
    pub params: Vec<(SharedString, UValueType, bool)>,
    pub return_type: UValueType,
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
            Expression::DoubleColon(e) => visitor.visit_double_colon_expression(e),
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
pub struct _ArrayDeclaration {
    pub name: SharedString,
    pub elements: Vec<Expression>,
    pub elem_tipe: Option<UValueType>,
    pub line_no: usize,
}

#[derive(Clone, Debug)]
pub struct InstantiateAs {
    pub name: SharedString,
    pub types: IndexMap<SharedString, UValueType>,
}

#[derive(Clone, Debug)]
pub enum FunctionGenerics {
    Parametric(Rc<RefCell<Vec<InstantiateAs>>>),
    Monomorphic(IndexMap<SharedString, UValueType>),
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub prototype: Prototype,
    pub line_no: usize,
    pub body: Rc<Option<Vec<ASTNode>>>,
    pub type_params: IndexMap<SharedString, Box<[Constraint]>>,
    pub generic_instantiations: FunctionGenerics,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value_literals::Value;

    #[test]
    fn test_ast_node_is_empty() {
        let empty = ASTNode::Empty;
        assert!(empty.is_empty());

        let stmt = ASTNode::Statement(Statement::Break(BreakStatement { line_no: 1 }));
        assert!(!stmt.is_empty());
    }

    #[test]
    fn test_ast_node_vec_of_empty() {
        let empty = ASTNode::Empty;
        let vec = empty.vec_of();
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn test_ast_node_vec_of_non_empty() {
        let stmt = ASTNode::Statement(Statement::Break(BreakStatement { line_no: 1 }));
        let vec = stmt.vec_of();
        assert_eq!(vec.len(), 1);
    }

    #[test]
    fn test_block_statement_creation() {
        let block = BlockStatement {
            statements: vec![],
            line_no: 10,
        };
        assert_eq!(block.statements.len(), 0);
        assert_eq!(block.line_no, 10);
    }

    #[test]
    fn test_if_statement_creation() {
        let condition = Expression::Literal(LiteralExpression {
            value: Value::Bool(true),
            line_no: 5,
        });
        let then_branch = Statement::Break(BreakStatement { line_no: 6 });

        let if_stmt = IfStatement {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: None,
            line_no: 5,
        };

        assert!(if_stmt.else_branch.is_none());
        assert_eq!(if_stmt.line_no, 5);
    }

    #[test]
    fn test_if_statement_with_else() {
        let condition = Expression::Literal(LiteralExpression {
            value: Value::Bool(false),
            line_no: 5,
        });
        let then_branch = Statement::Break(BreakStatement { line_no: 6 });
        let else_branch = Statement::Continue(ContinueStatement { line_no: 8 });

        let if_stmt = IfStatement {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Some(Box::new(else_branch)),
            line_no: 5,
        };

        assert!(if_stmt.else_branch.is_some());
    }

    #[test]
    fn test_while_statement_creation() {
        let condition = Expression::Literal(LiteralExpression {
            value: Value::Bool(true),
            line_no: 10,
        });
        let body = Statement::Break(BreakStatement { line_no: 11 });

        let while_stmt = WhileStatement {
            condition: Box::new(condition),
            body: Box::new(body),
            line_no: 10,
        };

        assert_eq!(while_stmt.line_no, 10);
    }

    #[test]
    fn test_for_statement_creation() {
        let for_stmt = ForStatement {
            init: None,
            condition: None,
            increment: None,
            body: Box::new(Statement::Break(BreakStatement { line_no: 15 })),
            line_no: 14,
        };

        assert!(for_stmt.init.is_none());
        assert!(for_stmt.condition.is_none());
        assert!(for_stmt.increment.is_none());
    }

    #[test]
    fn test_for_statement_complete() {
        let init = ASTNode::Declaration(Declaration::Var(VarDeclaration {
            name: "i".into(),
            tipe: ValueType::Integer.intern(),
            initializer: Some(Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(0),
                line_no: 20,
            }))),
            mutable: true,
            line_no: 20,
        }));

        let condition = Expression::Literal(LiteralExpression {
            value: Value::Bool(true),
            line_no: 20,
        });

        let increment = Expression::Literal(LiteralExpression {
            value: Value::Integer(1),
            line_no: 20,
        });

        let for_stmt = ForStatement {
            init: Some(Box::new(init)),
            condition: Some(Box::new(condition)),
            increment: Some(Box::new(increment)),
            body: Box::new(Statement::Break(BreakStatement { line_no: 21 })),
            line_no: 20,
        };

        assert!(for_stmt.init.is_some());
        assert!(for_stmt.condition.is_some());
        assert!(for_stmt.increment.is_some());
    }

    #[test]
    fn test_break_statement_creation() {
        let break_stmt = BreakStatement { line_no: 42 };
        assert_eq!(break_stmt.line_no, 42);
    }

    #[test]
    fn test_continue_statement_creation() {
        let continue_stmt = ContinueStatement { line_no: 43 };
        assert_eq!(continue_stmt.line_no, 43);
    }

    #[test]
    fn test_return_statement_with_value() {
        let return_stmt = ReturnStatement {
            value: Some(Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(42),
                line_no: 50,
            }))),
            line_no: 50,
        };

        assert!(return_stmt.value.is_some());
    }

    #[test]
    fn test_return_statement_without_value() {
        let return_stmt = ReturnStatement {
            value: None,
            line_no: 51,
        };

        assert!(return_stmt.value.is_none());
    }

    #[test]
    fn test_expression_statement_creation() {
        let expr_stmt = ExpressionStatement {
            expr: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(100),
                line_no: 60,
            })),
            line_no: 60,
        };

        assert_eq!(expr_stmt.line_no, 60);
    }

    #[test]
    fn test_literal_expression_integer() {
        let lit = LiteralExpression {
            value: Value::Integer(42),
            line_no: 70,
        };
        assert_eq!(lit.value, Value::Integer(42));
        assert_eq!(lit.line_no, 70);
    }

    #[test]
    fn test_literal_expression_float() {
        let lit = LiteralExpression {
            value: Value::Float(3.14),
            line_no: 71,
        };
        assert_eq!(lit.value, Value::Float(3.14));
    }

    #[test]
    fn test_literal_expression_bool() {
        let lit = LiteralExpression {
            value: Value::Bool(true),
            line_no: 72,
        };
        assert_eq!(lit.value, Value::Bool(true));
    }

    #[test]
    fn test_string_literal_expression() {
        let str_lit = StringLiteralExpression {
            value: "hello".into(),
            line_no: 80,
        };
        assert_eq!(str_lit.value, SharedString::from("hello"));
    }

    #[test]
    fn test_unary_expression_creation() {
        let unary = UnaryExpression {
            operator: TokenType::Minus,
            operand: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(5),
                line_no: 85,
            })),
            line_no: 85,
        };
        assert_eq!(unary.operator, TokenType::Minus);
    }

    #[test]
    fn test_binary_expression_creation() {
        let binary = BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(1),
                line_no: 90,
            })),
            operator: TokenType::Plus,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(2),
                line_no: 90,
            })),
            line_no: 90,
        };
        assert_eq!(binary.operator, TokenType::Plus);
    }

    #[test]
    fn test_eval_constexpr_literal_integer() {
        let expr = Expression::Literal(LiteralExpression {
            value: Value::Integer(42),
            line_no: 100,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Integer(42)));
    }

    #[test]
    fn test_eval_constexpr_unary_minus_integer() {
        let expr = Expression::Unary(UnaryExpression {
            operator: TokenType::Minus,
            operand: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(5),
                line_no: 110,
            })),
            line_no: 110,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Integer(-5)));
    }

    #[test]
    fn test_eval_constexpr_unary_not_bool() {
        let expr = Expression::Unary(UnaryExpression {
            operator: TokenType::Bang,
            operand: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Bool(true),
                line_no: 115,
            })),
            line_no: 115,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(false)));
    }

    #[test]
    fn test_eval_constexpr_binary_addition() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(10),
                line_no: 120,
            })),
            operator: TokenType::Plus,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(20),
                line_no: 120,
            })),
            line_no: 120,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Integer(30)));
    }

    #[test]
    fn test_eval_constexpr_binary_subtraction() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(20),
                line_no: 125,
            })),
            operator: TokenType::Minus,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(5),
                line_no: 125,
            })),
            line_no: 125,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Integer(15)));
    }

    #[test]
    fn test_eval_constexpr_binary_multiplication() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(3),
                line_no: 130,
            })),
            operator: TokenType::Star,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(7),
                line_no: 130,
            })),
            line_no: 130,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Integer(21)));
    }

    #[test]
    fn test_eval_constexpr_binary_division() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(20),
                line_no: 135,
            })),
            operator: TokenType::Slash,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(4),
                line_no: 135,
            })),
            line_no: 135,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Integer(5)));
    }

    #[test]
    fn test_eval_constexpr_binary_float_addition() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Float(1.5),
                line_no: 140,
            })),
            operator: TokenType::Plus,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Float(2.5),
                line_no: 140,
            })),
            line_no: 140,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Float(4.0)));
    }

    #[test]
    fn test_eval_constexpr_comparison_greater() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(10),
                line_no: 145,
            })),
            operator: TokenType::Greater,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(5),
                line_no: 145,
            })),
            line_no: 145,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(true)));
    }

    #[test]
    fn test_eval_constexpr_comparison_less() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(3),
                line_no: 150,
            })),
            operator: TokenType::Less,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(8),
                line_no: 150,
            })),
            line_no: 150,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(true)));
    }

    #[test]
    fn test_eval_constexpr_equality() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(42),
                line_no: 155,
            })),
            operator: TokenType::EqualEqual,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(42),
                line_no: 155,
            })),
            line_no: 155,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(true)));
    }

    #[test]
    fn test_eval_constexpr_inequality() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(42),
                line_no: 160,
            })),
            operator: TokenType::BangEqual,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(10),
                line_no: 160,
            })),
            line_no: 160,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(true)));
    }

    #[test]
    fn test_eval_constexpr_logical_and() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Bool(true),
                line_no: 165,
            })),
            operator: TokenType::And,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Bool(true),
                line_no: 165,
            })),
            line_no: 165,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(true)));
    }

    #[test]
    fn test_eval_constexpr_logical_or() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Bool(false),
                line_no: 170,
            })),
            operator: TokenType::Or,
            right: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Bool(true),
                line_no: 170,
            })),
            line_no: 170,
        });
        assert_eq!(expr.eval_constexpr(), Some(Value::Bool(true)));
    }

    #[test]
    fn test_eval_constexpr_non_constant_returns_none() {
        use std::{cell::RefCell, rc::Rc};
        let token = Rc::new(RefCell::new(Token {
            token_type: TokenType::Identifier,
            lexeme: "x".into(),
            line: 180,
        }));
        let expr = Expression::Variable(VariableExpression {
            token,
            line_no: 180,
        });
        assert_eq!(expr.eval_constexpr(), None);
    }

    #[test]
    fn test_deref_expression_creation() {
        let deref = DerefExpression {
            operand: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(42),
                line_no: 190,
            })),
            line_no: 190,
        };
        assert_eq!(deref.line_no, 190);
    }

    #[test]
    fn test_ref_expression_creation() {
        use std::{cell::RefCell, rc::Rc};
        let token = Rc::new(RefCell::new(Token {
            token_type: TokenType::Identifier,
            lexeme: "x".into(),
            line: 195,
        }));
        let ref_expr = RefExpression {
            operand: Box::new(Expression::Variable(VariableExpression {
                token,
                line_no: 195,
            })),
            line_no: 195,
        };
        assert_eq!(ref_expr.line_no, 195);
    }

    #[test]
    fn test_index_expression_creation() {
        use std::{cell::RefCell, rc::Rc};
        let token = Rc::new(RefCell::new(Token {
            token_type: TokenType::Identifier,
            lexeme: "arr".into(),
            line: 200,
        }));
        let index = IndexExpression {
            array: Box::new(Expression::Variable(VariableExpression {
                token,
                line_no: 200,
            })),
            index: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(0),
                line_no: 200,
            })),
            line_no: 200,
        };
        assert_eq!(index.line_no, 200);
    }

    #[test]
    fn test_var_declaration_mutable() {
        let var_decl = VarDeclaration {
            name: "x".into(),
            tipe: ValueType::Integer.intern(),
            initializer: Some(Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(42),
                line_no: 210,
            }))),
            mutable: true,
            line_no: 210,
        };

        assert!(var_decl.mutable);
        assert_eq!(var_decl.name, SharedString::from("x"));
        assert!(var_decl.initializer.is_some());
    }

    #[test]
    fn test_var_declaration_immutable() {
        let var_decl = VarDeclaration {
            name: "y".into(),
            tipe: ValueType::Float.intern(),
            initializer: None,
            mutable: false,
            line_no: 215,
        };

        assert!(!var_decl.mutable);
        assert!(var_decl.initializer.is_none());
    }

    #[test]
    fn test_variable_expression_creation() {
        use std::{cell::RefCell, rc::Rc};
        let token = Rc::new(RefCell::new(Token {
            token_type: TokenType::Identifier,
            lexeme: "myVar".into(),
            line: 220,
        }));
        let var_expr = VariableExpression {
            token: token.clone(),
            line_no: 220,
        };
        assert_eq!(var_expr.token.borrow().lexeme, SharedString::from("myVar"));
    }

    #[test]
    fn test_assign_expression_creation() {
        use std::{cell::RefCell, rc::Rc};
        let token = Rc::new(RefCell::new(Token {
            token_type: TokenType::Identifier,
            lexeme: "x".into(),
            line: 225,
        }));
        let assign = AssignExpression {
            variable: Box::new(Expression::Variable(VariableExpression {
                token,
                line_no: 225,
            })),
            value: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(100),
                line_no: 225,
            })),
            line_no: 225,
        };
        assert_eq!(assign.line_no, 225);
    }

    #[test]
    fn test_cast_expression_creation() {
        let cast = CastExpression {
            expression: Box::new(Expression::Literal(LiteralExpression {
                value: Value::Float(3.14),
                line_no: 230,
            })),
            target_type: ValueType::Integer.intern(),
            line_no: 230,
        };
        assert_eq!(cast.target_type, ValueType::Integer.intern());
    }

    #[test]
    fn test_double_colon_expression_creation() {
        let double_colon = DoubleColonExpression {
            typ: ValueType::Integer.intern(),
            acessor: "method".into(),
            line_no: 235,
        };
        assert_eq!(double_colon.acessor, SharedString::from("method"));
    }

    #[test]
    fn test_expression_empty() {
        let empty_expr = Expression::Empty;
        assert!(matches!(empty_expr, Expression::Empty));
    }
}
