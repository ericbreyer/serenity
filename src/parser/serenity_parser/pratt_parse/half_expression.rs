use crate::{
    lexer::{Token, TokenType},
    prelude::*,
};

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
}

impl HalfExpression {
    pub fn fill(self, left: Expression, line: usize) -> Expression {

        match self {
            HalfExpression::Binary(t, r) => Expression::Binary(BinaryExpression {
                left: Box::new(left),
                operator: t,
                right: r,
                line_no: line,
            }),
            HalfExpression::And(r) => Expression::Logical(LogicalExpression {
                left: Box::new(left),
                operator: TokenType::And,
                right: r,
                line_no: line,
            }),
            HalfExpression::Or(r) => Expression::Logical(LogicalExpression {
                left: Box::new(left),
                operator: TokenType::Or,
                right: r,
                line_no: line,
            }),
            HalfExpression::Ternary(l, r) => Expression::Ternary(TernaryExpression {
                condition: Box::new(left),
                then_branch: l,
                else_branch: r,
                line_no: line,
            }),
            HalfExpression::Index(r) => Expression::Index(IndexExpression {
                array: Box::new(left),
                index: r,
                line_no: line,
            }),
            HalfExpression::Call(r) => Expression::Call(CallExpression {
                callee: Box::new(left),
                arguments: r,
                line_no: line,
            }),
            HalfExpression::Assign(r) => Expression::Assign(AssignExpression {
                variable: Box::new(left),
                value: r,
                line_no: line,
            }),
            HalfExpression::DerefDot(t) => Expression::Dot(DotExpression {
                object: Box::new(Expression::Deref(DerefExpression {
                    operand: Box::new(left),
                    line_no: line,
                })),
                field: t.lexeme,
                line_no: line,
            }),
            HalfExpression::Dot(t) => Expression::Dot(DotExpression {
                object: Box::new(left),
                field: t.lexeme,
                line_no: line,
            }),
        }
    }
}
