use std::{
    cell::RefCell,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    lexer::{Token, TokenType},
    prelude::*,
    typing::UValueType,
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
    PartialCall(Vec<Expression>, Vec<usize>),
    Assign(Box<Expression>),
    DoubleColon(Box<Expression>),
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
                callee: Box::new(left.clone()),
                arguments: r,
                line_no: line,
            }),
            HalfExpression::PartialCall(args, placeholders) => {
                thread_local! {
                    static ANON_ID: AtomicUsize = const { AtomicUsize::new(usize::MAX/2) };
                }

                // Generate parameter names for each placeholder
                let mut param_names: Vec<SharedString> = Vec::new();
                for i in 0..placeholders.len() {
                    param_names.push(format!("arg{}", i).into());
                }

                // Build params vector: (name, typevar, mutable)
                let mut params: Vec<(SharedString, UValueType, bool)> = Vec::new();
                let tv = ValueType::new_type_var(Box::new([]));
                for n in &param_names {
                    params.push((n.clone(), tv, false));
                }

                // Determine captures: any non-placeholder argument that's a variable
                let mut captures: Vec<SharedString> = Vec::new();
                for (i, a) in args.iter().enumerate() {
                    if placeholders.contains(&i) {
                        continue;
                    }
                    if let Expression::Variable(VariableExpression { token, .. }) = a {
                        let name = token.borrow().lexeme.clone();
                        if name != ("_".into()) && !captures.contains(&name) {
                            captures.push(name);
                        }
                    }
                }

                // Build new argument list for the inner call: replace placeholders with param
                // variables
                let mut call_args: Vec<Expression> = Vec::new();
                let mut placeholder_idx = 0usize;
                for (i, a) in args.into_iter().enumerate() {
                    if placeholders.contains(&i) {
                        // construct a Variable expression for the param
                        let pname = param_names[placeholder_idx].clone();
                        placeholder_idx += 1;
                        let tok = Token {
                            token_type: TokenType::Identifier,
                            lexeme: pname.clone(),
                            line,
                        };
                        call_args.push(Expression::Variable(VariableExpression {
                            token: Rc::new(RefCell::new(tok)),
                            line_no: line,
                        }));
                    } else {
                        call_args.push(a);
                    }
                }

                // Build return statement wrapping the call
                let call_expr = CallExpression {
                    callee: Box::new(left.clone()),
                    arguments: call_args,
                    line_no: line,
                };

                let ret_stmt = Statement::Return(ReturnStatement {
                    value: Some(Box::new(Expression::Call(call_expr))),
                    line_no: line,
                });

                let body = Some(vec![ASTNode::Statement(ret_stmt)]);

                let name = format!(
                    "anon{}",
                    ANON_ID.with(|ai| ai.fetch_add(1, Ordering::SeqCst))
                )
                .into();

                let func = FunctionExpression::new(
                    captures,
                    params,
                    body,
                    ValueType::new_type_var(Box::new([])),
                    name,
                );
                let mut f = func;
                f.line_no = line;
                Expression::Function(f)
            }
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
            HalfExpression::DoubleColon(t) => {
                let Expression::Variable(VariableExpression { token, .. }) = left else {
                    println!(
                        "Expected a variable before '::' but got {:?}",
                        left.as_node()
                    );
                    return Expression::Empty;
                };

                let typ = ValueType::from(token.borrow().lexeme.clone());

                let Expression::Variable(VariableExpression { token, line_no, .. }) = *t else {
                    println!("Expected a variable after '::' but got {:?}", t.as_node());
                    return Expression::Empty;
                };

                let x = Expression::DoubleColon(DoubleColonExpression {
                    typ: typ.intern(),
                    acessor: token.borrow().lexeme.clone(),
                    line_no,
                });
                x
            }
        }
    }
}
