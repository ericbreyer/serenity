use crate::{
    prelude::*,
    lexer::{Token, TokenType},
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
    pub fn fill(self, left: Expression) -> Expression {
        let line = left.line_no();
        match self {
            HalfExpression::Binary(t, r) => Expression::Binary(Box::new(left), t, r, line),
            HalfExpression::And(r) => Expression::Logical(Box::new(left), TokenType::And, r, line),
            HalfExpression::Or(r) => Expression::Logical(Box::new(left), TokenType::Or, r, line),
            HalfExpression::Ternary(l, r) => Expression::Ternary(Box::new(left), l, r, line),
            HalfExpression::Index(r) => Expression::Index(Box::new(left), r, line),
            HalfExpression::Call(r) => Expression::Call(Box::new(left), r, line),
            HalfExpression::Assign(r) => Expression::Assign(Box::new(left), r, line),
            HalfExpression::DerefDot(t) => {
                Expression::Dot(Box::new(Expression::Deref(Box::new(left), line)), t, line)
            }
            HalfExpression::Dot(t) => Expression::Dot(Box::new(left), t, line),
        }
    }
}
