mod half_expression;
mod parse_table;
use std::sync::atomic::{AtomicUsize, Ordering};

use half_expression::HalfExpression;
pub use parse_table::ParseTable;

use crate::{prelude::*, lexer::TokenType, value::Value};

use super::{Precedence, SerenityParser};

type PrefixParseFn = fn(&mut SerenityParser, bool) -> Expression;
type InfixParseFn = fn(&mut SerenityParser, bool) -> HalfExpression;

#[derive(Default)]
pub struct ParseRule {
    prefix: Option<PrefixParseFn>,
    infix: Option<InfixParseFn>,
    precedence: Precedence,
}
impl SerenityParser {
    //----------------------------//
    // Pratt Parser (Expressions) //
    //----------------------------//

    pub(super) fn parse_precedence(&mut self, precedence: Precedence) -> Expression {
        if self.current.token_type == TokenType::Struct {
            return self.type_expression();
        }

        self.advance();
        let prefix_rule = &self.parse_table[self.previous.token_type as usize].prefix;
        if prefix_rule.is_none() {
            self.error("Expect expression.");
            return Expression::Empty;
        }

        let can_assign = precedence <= Precedence::Assignment;

        let mut node = prefix_rule.unwrap()(self, can_assign);

        while precedence <= self.parse_table[self.current.token_type as usize].precedence {
            let _prev = self.previous.lexeme.clone();
            self.advance();
            let infix_rule = &self.parse_table[self.previous.token_type as usize].infix;

            let hnode = infix_rule.expect(&format!(
                "token {:?} has no infix rule",
                self.previous.token_type
            ))(self, can_assign);
            node = hnode.fill(node);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }

        node
    }

    fn dot(&mut self, _can_assign: bool) -> HalfExpression {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.previous.clone();

        HalfExpression::Dot(name)
    }

    fn deref_dot(&mut self, _can_assign: bool) -> HalfExpression {
        self.consume(TokenType::Identifier, "Expect property name after '->'.");
        let name = self.previous.clone();

        HalfExpression::DerefDot(name)
    }

    fn and_(&mut self, _can_assign: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::And);
        HalfExpression::And(node.into())
    }

    fn or_(&mut self, _can_assign: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::Or);
        HalfExpression::Or(node.into())
    }

    fn cast(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;

        self.consume(TokenType::LeftParen, "Expect '(' after 'cast'.");
        let node = self.expression();
        let cast_type = if self.match_token(TokenType::Comma) {
            Some(self.parse_complex_type(&None))
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after type.");

        Expression::Cast(Box::new(node), cast_type, line)
    }

    fn cast_prefix(&mut self, _can_assign: bool) -> Expression {
        // type(value)
        // (type*)(value)
        let line = self.previous.line;

        let cast_type: UValueType = self.previous.lexeme.clone().into();

        self.consume(TokenType::LeftParen, "Expect '(' after 'cast'.");
        let node = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");

        Expression::Cast(Box::new(node), Some(cast_type), line)
    }

    fn lambda(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        thread_local! {
        static ANON_ID: AtomicUsize = AtomicUsize::new(0);
        }
        let func_expr = self.function(
            format!(
                "anon{}",
                ANON_ID.with(|ai| ai.fetch_add(1, Ordering::Relaxed))
            )
            .as_str(),
        );
        Expression::Function(func_expr, line)
    }

    pub(super) fn function(&mut self, _func_name: &str) -> FunctionExpression {
        let mut captures = Vec::new();
        if self.match_token(TokenType::LeftBracket) {
            loop {
                self.consume(TokenType::Identifier, "Expect capture name.");
                captures.push(self.previous.lexeme.clone());
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
            self.consume(TokenType::RightBracket, "Expect ']' after captures.");
        }

        let mut params = Vec::new();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        let mut param_types: Vec<UValueType> = Vec::new();
        if self.current.token_type != TokenType::RightParen {
            loop {
                let mut mutable = true;
                if self.match_token(TokenType::Const) {
                    mutable = false;
                }

                if !self.match_token(TokenType::Identifier) {
                    break;
                }
                let name = self.previous.lexeme.clone();
                self.consume(TokenType::Colon, "Expect ':' after parameter name.");

                let p_type = self.parse_complex_type(&None);

                param_types.push(p_type);

                params.push((name, p_type, mutable));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        let mut return_type = ValueType::Nil.intern();
        if self.match_token(TokenType::RightArrow) {
            return_type = self.parse_complex_type(&None);
        }
        param_types.push(return_type);

        let mut nodes = vec![];
        if self.match_token(TokenType::LeftBrace){
            nodes = self.block();
        } else {
            self.consume(TokenType::Semicolon, "Need semicolon after weak decl");
        }

        FunctionExpression::new(captures, params, nodes, return_type, _func_name.into())
    }

    fn assign(&mut self, _can_assign: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::Assignment);

        HalfExpression::Assign(node.into())
    }

    fn number(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        if let Ok(n) = self.previous.lexeme.parse::<i64>() {
            Expression::Literal(Value::Integer(n), line)
        } else if let Ok(n) = self
            .previous
            .lexeme
            .strip_suffix('u')
            .unwrap_or("")
            .parse::<u64>()
        {
            Expression::Literal(Value::UInteger(n), line)
        } else if let Ok(n) = self.previous.lexeme.parse::<f64>() {
            Expression::Literal(Value::Float(n), line)
        } else {
            self.error("Invalid number.");
            Expression::Empty
        }
    }

    fn char(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        let mut value = self.previous.lexeme.clone().to_string();
        value = value.trim_matches('\'').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\0", "\0");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        let c = value.as_bytes()[0] as u8;

        Expression::Literal(Value::Char(c), line)
    }

    fn grouping(&mut self, _can_assign: bool) -> Expression {
        let expr = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        expr
    }

    fn ternary(&mut self, _can_assign: bool) -> HalfExpression {
        let expr = self.parse_precedence(Precedence::Ternary.next());
        self.consume(TokenType::Colon, "Expect ':' after expression.");
        let else_expr = self.parse_precedence(Precedence::Ternary);
        HalfExpression::Ternary(Box::new(expr), Box::new(else_expr))
    }

    fn unary(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        let operator_type = self.previous.token_type;
        // Compile the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Unary(operator_type, Box::new(expr), line)
    }

    fn deref(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        // Compile the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Deref(Box::new(expr), line)
    }

    fn parse_expression_index(&mut self) -> Expression {
        self.expression()
    }

    fn index(&mut self, _can_assign: bool) -> HalfExpression {
        let expridx = self.parse_expression_index();
        self.consume(TokenType::RightBracket, "Expect ']' after index.");

        HalfExpression::Index(Box::new(expridx))
    }

    fn addr_of(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        // parse the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Ref(Box::new(expr), line)
    }

    fn binary(&mut self, _can_assign: bool) -> HalfExpression {
        let operator_type = self.previous.token_type;

        // Compile the right operand.
        let rule = &self.parse_table[operator_type as usize];
        let expr = self.parse_precedence(rule.precedence.next());

        // Emit the operator instruction.
        HalfExpression::Binary(operator_type, Box::new(expr))
    }

    fn call(&mut self, _can_assign: bool) -> HalfExpression {
        let nodes = self.argument_list();
        HalfExpression::Call(nodes)
    }

    fn argument_list(&mut self) -> Vec<Expression> {
        let mut arg_count = 0;
        let mut nodes = Vec::new();
        if self.current.token_type != TokenType::RightParen {
            loop {
                let expr = self.expression();
                nodes.push(expr);

                if arg_count == 255 {
                    self.error("Cannot have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
                if self.current.token_type == TokenType::RightParen {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        nodes
    }

    fn literal(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        match self.previous.token_type {
            TokenType::False => Expression::Literal(Value::Bool(false), line),
            TokenType::True => Expression::Literal(Value::Bool(true), line),
            _ => unreachable!(),
        }
    }

    fn string(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        let mut value = self.previous.lexeme.clone().to_string();
        value = value.trim_matches('"').to_string();
        value = value.replace("\\n", "\n");
        value = value.replace("\\a", "\x07");
        value = value.replace("\\r", "\r");
        value = value.replace("\\t", "\t");
        value = value.replace("\\\\", "\\");
        value = value.replace("\\\"", "\"");
        value = value.replace("\\{", "{");
        value = value.replace("\\}", "}");
        Expression::StringLiteral(value.into(), line)
    }

    fn variable(&mut self, _can_assign: bool) -> Expression {
        let line = self.previous.line;
        Expression::Variable(self.previous.clone(), line)
    }

    fn type_expression(&mut self) -> Expression {
        let was_struct = self.current.token_type == TokenType::Struct;
        let t = self.parse_complex_type(&None);

        if was_struct && self.current.token_type == TokenType::LeftBrace {
            return self.struct_initializer(t);
        } else if self.current.token_type == TokenType::DoubleColon {
            self.advance();
            let Expression::Variable(mut tok, l) = self.parse_precedence(Precedence::Call) else {
                return Expression::Empty;
            };
            tok.lexeme = format!("{}_{}", t.unique_string(), tok.lexeme).into();
            return Expression::Variable(tok, l);
        } else {
            return Expression::Empty;
        }
    }
}
