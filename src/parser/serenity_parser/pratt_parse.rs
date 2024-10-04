mod half_expression;
mod parse_table;
use std::{rc::Rc, sync::atomic::{AtomicUsize, Ordering}};

use half_expression::HalfExpression;
pub use parse_table::ParseTable;

use crate::{lexer::TokenType, prelude::*, typing::UValueType, value_literals::Value};

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
            let _ = self.previous.lexeme.clone();
            self.advance();
            let infix_rule = &self.parse_table[self.previous.token_type as usize].infix;

            let hnode = infix_rule.unwrap_or_else(|| {
                panic!("token {:?} has no infix rule", self.previous.token_type)
            })(self, can_assign);
            node = hnode.fill(node, self.previous.line);
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }

        node
    }

    fn dot(&mut self,_: bool) -> HalfExpression {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.previous.clone();

        HalfExpression::Dot(name)
    }

    fn deref_dot(&mut self,_: bool) -> HalfExpression {
        self.consume(TokenType::Identifier, "Expect property name after '->'.");
        let name = self.previous.clone();

        HalfExpression::DerefDot(name)
    }

    fn and_(&mut self,_: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::And);
        HalfExpression::And(node.into())
    }

    fn or_(&mut self,_: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::Or);
        HalfExpression::Or(node.into())
    }

    fn cast(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;

        let (node, cast_type) = if self.match_token(TokenType::LeftParen) {
            let node = self.expression();
            let cast_type = if self.match_token(TokenType::Comma) {
                self.parse_type(None, None)
            } else {
                ValueType::new_type_var()
            };
            self.consume(TokenType::RightParen, "Expect ')' after type.");
            (node, cast_type)
        } else {
            let node = self.parse_precedence(Precedence::Unary);
            (node, ValueType::new_type_var())
        };

        Expression::Cast(CastExpression {
            expression: Box::new(node),
            target_type: cast_type,
            line_no,
        })
    }

    fn cast_prefix(&mut self,_: bool) -> Expression {
        // type(value)
        // (type*)(value)
        let line_no = self.previous.line;

        let cast_type: UValueType = ValueType::from(self.previous.lexeme.clone()).intern();

        self.consume(TokenType::LeftParen, "Expect '(' after 'cast'.");
        let node = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");

        Expression::Cast(CastExpression {
            expression: Box::new(node),
            target_type: cast_type,
            line_no,
        })
    }

    fn lambda(&mut self,_: bool) -> Expression {
        thread_local! {
        static ANON_ID: AtomicUsize = const { AtomicUsize::new(0) };
        }
        let func_expr = self.function(
            format!(
                "anon{}",
                ANON_ID.with(|ai| ai.fetch_add(1, Ordering::Relaxed))
            )
            .as_str(),
            Vec::new(),
        );
        Expression::Function(func_expr)
    }

    pub(super) fn function(&mut self, name: &str, type_params: Vec<SharedString>) -> FunctionExpression {
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
                let p_type = self.parse_type(None, Some(&type_params));

                if let ValueType::Array(_, _) = *p_type {
                    self.warn("Passing arrays by value may be expensive");
                }

                param_types.push(p_type);

                params.push((name, p_type, mutable));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        let mut return_type = ValueType::new_type_var();
        if self.match_token(TokenType::RightArrow) {
            return_type = self.parse_type(None, Some(&type_params));
        }
        param_types.push(return_type);

        let mut nodes = None;
        if self.match_token(TokenType::LeftBrace) {
            nodes = Some(self.block());
        } else {
            self.consume(TokenType::Semicolon, "Need semicolon after weak decl");
        }

        FunctionExpression::new(captures, params, nodes, return_type, name.into())
    }

    fn assign(&mut self,_: bool) -> HalfExpression {
        let node = self.parse_precedence(Precedence::Assignment);

        HalfExpression::Assign(node.into())
    }

    fn number(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;
        if let Ok(n) = self.previous.lexeme.parse::<i64>() {
            Expression::Literal(LiteralExpression {
                value: Value::Integer(n),
                line_no,
            })
        } else if let Ok(n) = self
            .previous
            .lexeme
            .strip_suffix('u')
            .unwrap_or("")
            .parse::<u64>()
        {
            Expression::Literal(LiteralExpression {
                value: Value::UInteger(n),
                line_no,
            })
        } else if let Ok(n) = self.previous.lexeme.parse::<f64>() {
            Expression::Literal(LiteralExpression {
                value: Value::Float(n),
                line_no,
            })
        } else {
            self.error("Invalid number.");
            Expression::Empty
        }
    }

    fn char(&mut self,_: bool) -> Expression {
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
        let c = value.as_bytes()[0];

        Expression::Literal(LiteralExpression {
            value: Value::Char(c),
            line_no: line,
        })
    }

    fn grouping(&mut self,_: bool) -> Expression {
        let expr = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        expr
    }

    fn ternary(&mut self,_: bool) -> HalfExpression {
        let expr = self.parse_precedence(Precedence::Ternary.next());
        self.consume(TokenType::Colon, "Expect ':' after expression.");
        let else_expr = self.parse_precedence(Precedence::Ternary);
        HalfExpression::Ternary(Box::new(expr), Box::new(else_expr))
    }

    fn unary(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;
        let operator = self.previous.token_type;
        // Compile the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Unary(UnaryExpression {
            operator,
            operand: Box::new(expr),
            line_no,
        })
    }

    fn sizeof(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;
        let tipe = self.parse_type(None, None);
        Expression::Sizeof(SizeofExpression { tipe, line_no })
    }

    fn deref(&mut self,_: bool) -> Expression {
        let line = self.previous.line;
        // Compile the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Deref(DerefExpression {
            operand: Box::new(expr),
            line_no: line,
        })
    }

    fn parse_expression_index(&mut self) -> Expression {
        self.expression()
    }

    fn index(&mut self,_: bool) -> HalfExpression {
        let expridx = self.parse_expression_index();
        self.consume(TokenType::RightBracket, "Expect ']' after index.");

        HalfExpression::Index(Box::new(expridx))
    }

    fn addr_of(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;
        // parse the operand.
        let expr = self.parse_precedence(Precedence::Unary);

        Expression::Ref(RefExpression {
            operand: Box::new(expr),
            line_no,
        })
    }

    fn binary(&mut self,_: bool) -> HalfExpression {
        let operator_type = self.previous.token_type;

        // Compile the right operand.
        let rule = &self.parse_table[operator_type as usize];
        let expr = self.parse_precedence(rule.precedence.next());

        // Emit the operator instruction.
        HalfExpression::Binary(operator_type, Box::new(expr))
    }

    fn call(&mut self,_: bool) -> HalfExpression {
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

    fn literal(&mut self,_: bool) -> Expression {
        let line = self.previous.line;
        match self.previous.token_type {
            TokenType::False => Expression::Literal(LiteralExpression {
                value: Value::Bool(false),
                line_no: line,
            }),
            TokenType::True => Expression::Literal(LiteralExpression {
                value: Value::Bool(true),
                line_no: line,
            }),
            _ => unreachable!(),
        }
    }

    fn string(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;
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
        Expression::StringLiteral(StringLiteralExpression {
            value: value.into(),
            line_no,
        })
    }

    fn variable(&mut self,_: bool) -> Expression {
        let line_no = self.previous.line;
        if self.constants.contains_key(&self.previous.lexeme) {
            return Expression::Literal(LiteralExpression {
                value: self.constants[&self.previous.lexeme],
                line_no,
            });
        }
        Expression::Variable(VariableExpression {
            token: Rc::new(self.previous.clone().into()),
            line_no,
        })
    }

    fn type_expression(&mut self) -> Expression {
        let was_struct = self.current.token_type == TokenType::Struct;
        let t = self.parse_type(None, None);

        if was_struct && self.current.token_type == TokenType::LeftBrace {
            self.struct_initializer(t)
        } else if self.current.token_type == TokenType::DoubleColon {
            self.advance();
            let Expression::Variable(VariableExpression { token, line_no }) =
                self.parse_precedence(Precedence::Call)
            else {
                return Expression::Empty;
            };
            token.borrow_mut().lexeme = format!("{}_{}", t.id_str(), token.borrow().lexeme).into();
            return Expression::Variable(VariableExpression { token, line_no });
        } else {
            return Expression::Empty;
        }
    }
}
