use std::{ cell::Cell, collections::HashMap, rc::Rc };

use tracing::{instrument, warn, Level };

use crate::{
    common::ast::{ Expression, FunctionExpression }, lexer::{ Token, TokenType }, typing::{ CustomStruct, UValueType, ValueType }, value::Value
};

use super::{ OptimizationWalker, Local, FunctionCompiler };

use crate::error;

impl OptimizationWalker {
    #[instrument(level = "trace", skip_all)]
    pub fn literal(&mut self, v: Value) -> Expression {
        Expression::Literal(v)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn string(&mut self, s: String) -> Expression {
        Expression::StringLiteral(s)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn unary(&mut self, token_t: TokenType, e: Expression) -> Expression {
        let _line = 0;
        let operator_type = token_t;

        

        // Compile the operand.
        let o = self.visit_expression(e, false);
        
        let status_quo = Expression::Unary(token_t, Box::new(o.clone()));

        let Expression::Literal(v) = o else {
            return status_quo;
        };

        // Emit the operator instruction.
        let l = match operator_type {
            TokenType::Minus =>
                match v {
                    Value::Integer(i) => Value::Integer(-i),
                    Value::Float(f) => Value::Float(-f),
                    _ => {
                        return status_quo;
                    }
                }
            TokenType::Bang => match v {
                Value::Bool(b) => Value::Bool(!b),
                _ => {
                    return status_quo;
                }
            }
            _ => unreachable!(),
        };
        Expression::Literal(l)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn deref(&mut self, e: Expression, _at: bool) -> Expression {
        Expression::Deref(Box::new(e))
    }

    #[instrument(level = "trace", skip_all)]
    pub fn addr_of(&mut self, e: Expression) -> Expression {
        Expression::Ref(Box::new(e))
    }

    #[instrument(level = "trace", skip_all)]
    pub fn index(&mut self, e: Expression, i: Expression, _at: bool) -> Expression {
        let _line = 0;
        let t = self.visit_expression(e, false);
        let i = self.visit_expression(i, false);
        Expression::Index(Box::new(t), Box::new(i))
    }

    #[instrument(level = "trace", skip_all)]
    pub fn binary(&mut self, l: Expression, operator: TokenType, r: Expression) -> Expression {
        let operator_type = operator;
        let _line = 0;

        // Compile the left operand.
        let le = self.visit_expression(l, false);

        // Compile the right operand.
        let re = self.visit_expression(r, false);

        let status_quo = Expression::Binary(Box::new(le.clone()), operator, Box::new(re.clone()));

        let Expression::Literal(lv) = le else {
            return status_quo;
        };
        let Expression::Literal(rv) = re else {
            return status_quo;
        };

        // Emit the operator instruction.
        let l = match operator_type {
            TokenType::Plus =>
                match (lv.clone(), rv.clone()) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Integer(l + r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Float(l + r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            TokenType::Minus =>
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Integer(l - r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Float(l - r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            TokenType::Star =>
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Integer(l * r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Float(l * r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            TokenType::Slash =>
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Integer(l / r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Float(l / r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            TokenType::EqualEqual => {
                match (lv.clone(), rv.clone()) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Bool(l == r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Bool(l == r)
                    }
                    (Value::Bool(l), Value::Bool(r)) => {
                        Value::Bool(l == r)
                    }
                    (Value::Nil, Value::Nil) => {
                        Value::Bool(true)
                    }
                    _ => {
                        self.warn(
                            &format!(
                                "Comparison of different types ({lv:?} and {rv:?}) is always false, please cast to the same type"
                            )
                        );
                        Value::Bool(false)
                    }
                }
            }
            TokenType::BangEqual => {
                match (lv.clone(), rv.clone()) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Bool(l != r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Bool(l != r)
                    }
                    (Value::Bool(l), Value::Bool(r)) => {
                        Value::Bool(l != r)
                    }
                    (Value::Nil, Value::Nil) => {
                        Value::Bool(false)
                    }
                    _ => {
                        self.warn(
                            &format!(
                                "Comparison of different types ({lv:?} and {rv:?}) is always true, please cast to the same type"
                            )
                        );
                        Value::Bool(true)
                    }
                }
            }
            TokenType::Greater => {
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Bool(l > r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Bool(l > r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            }
            TokenType::GreaterEqual => {
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Bool(l >= r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Bool(l >= r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
                
            }
            TokenType::Less => {
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Bool(l < r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Bool(l < r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            }
            TokenType::LessEqual => {
                match (lv, rv) {
                    (Value::Integer(l), Value::Integer(r)) => {
                        Value::Bool(l <= r)
                    }
                    (Value::Float(l), Value::Float(r)) => {
                        Value::Bool(l <= r)
                    }
                    _ => {
                        error!(self, "Operands must be two numbers.");
                        return status_quo;
                    }
                }
            }
            _ => unreachable!(),
        };
        Expression::Literal(l)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn ternary(&mut self, c: Expression, t: Expression, f: Expression) -> Expression {
        let _line = 0;
        let cond_v = self.visit_expression(c, false);
        let conseq_v = self.visit_expression(t, false);
        let alt_v = self.visit_expression(f, false);

        let status_quo = Expression::Ternary(Box::new(cond_v.clone()), Box::new(conseq_v.clone()), Box::new(alt_v.clone()));
        
        let Expression::Literal(Value::Bool(cond)) = cond_v else {
            return status_quo;
        };

        if cond {
            conseq_v
        } else {
            alt_v
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn variable(&mut self, tok: Token, _assignment_target: bool) -> Expression {
        let _line = 0;

        // let (arg, assignable) = self.resolve_local(0, &tok);
        // if let Some(i) = arg {
        //     if assignment_target {
        //         None
        //     } else {
        //         None
        //     }
        // } else if let Some(i) = self.resolve_upvalue(0, &tok) {
        //     if assignment_target {
        //         None
        //     } else {
        //         None
        //     }
        // } else if let Some(i) = self.gloabls.get(&tok.lexeme) {
        //     if assignment_target {
        //         None
        //     } else {
        //         None
        //     }
        // } else {
        //     error!(self, format!("Undefined variable '{}'.", tok.lexeme).as_str());
        //     None
        // };
        
        Expression::Variable(tok)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn assign(&mut self, l: Expression, r: Expression) -> Expression {
        let _line = 0;
        let t = self.visit_expression(l, true);

        let t2 = self.visit_expression(r, false);

        

        Expression::Assign(Box::new(t), Box::new(t2))
    }

    #[instrument(level = "trace", skip_all)]
    pub fn logical(&mut self, l: Expression, operator: TokenType, r: Expression) -> Expression {
        let _line = 0;
        let operator_type = operator;
        let lv = self.visit_expression(l, false);
        let rv = self.visit_expression(r, false);

        let status_quo = Expression::Logical(Box::new(lv.clone()), operator, Box::new(rv.clone()));

        
        match operator_type {
            TokenType::Or => {
                if let Expression::Literal(Value::Bool(l)) = lv {
                    if l {
                        lv
                    } else {
                    rv
                    }
                }
                else {
                    error!(self, "Operands must be two booleans.");
                    return status_quo;
                }
            }
            TokenType::And => {
                if let Expression::Literal(Value::Bool(l)) = lv {
                    if l {
                        rv
                    } else {
                        lv
                    }
                }
                else {
                    error!(self, "Operands must be two booleans.");
                    return status_quo;
                }
            }
            _ => unreachable!(),
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub fn call(&mut self, callee: Expression, args: Vec<Expression>) -> Expression {
        let _line = 0;
        let c = self.visit_expression(callee, false);
        let mut a = Vec::new();
        for arg in args {
            a.push(self.visit_expression(arg, false));
        }
        Expression::Call(Box::new(c), a)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn dot(&mut self, e: Expression, tok: Token, _at: bool) -> Expression {
        let _line = 0;
        let t = self.visit_expression(e, false);
        Expression::Dot(Box::new(t), tok)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn function(&mut self, mut function_expr: FunctionExpression) -> Expression {
        let c = FunctionCompiler::new(
            super::FunctionType::Function,
            Some(std::mem::take(&mut self.function_compiler)),
            &function_expr.name.clone()
        );
        self.function_compiler = Box::new(c);
        self.function_compiler.scope_depth += 1;

        //     loop {
        //         self.compiler.func.arity += 1;
        //         if self.compiler.func.arity > 255 {
        //             self.error_at_current("Cannot have more than 255 parameters.");
        //         }

        //         let mut mutable = true;
        //         if self.match_token(TokenType::Const) {
        //             mutable = false;
        //         }
        //         let id = self.parse_variable("Expect parameter name.", mutable);
        //         self.consume(TokenType::Colon, "Expect ':' after parameter name.");

        //         let p_type = self.parse_complex_type(!mutable);
        //         param_types.push(p_type);

        //         self.define_variable(id as u32, p_type);

        //         if !self.match_token(TokenType::Comma) {
        //             break;
        //         }
        //     }
        // }
        self.function_compiler.func.arity = function_expr.params.len();
        for param in &function_expr.params {
            let _mutable = true;

            let p_type = param.1;

            self.parse_variable(param.0.clone(), _mutable);
            let last_local = self.last_local();
            self.function_compiler.locals.get_mut(&last_local).unwrap().local_type = p_type;

            self.define_variable(0, p_type);
        }

        let return_type = function_expr.return_type;

        self.function_compiler.func.return_type = return_type;
        self.function_compiler.func.return_size = return_type.num_words();

        let mut param_types = function_expr.params
            .iter()
            .map(|(_, t)| *t)
            .collect::<Vec<_>>();
        param_types.push(return_type);

        let func_name = function_expr.name.clone();

        let ft = ValueType::Closure(param_types.clone().as_slice().into()).intern();
        self.function_compiler.locals.insert(0, Local {
            name: func_name.clone(),
            depth: 0,
            local_type: ft,
        });

        for d in &mut function_expr.body {
            self.visit(d);
        }

        *param_types.last_mut().unwrap() = self.function_compiler.func.return_type;
        let c = std::mem::take(&mut self.function_compiler);
        let name = c.func.name.clone();
        // if self.had_error.get() {
        c.func.chunk.disassemble(&name, Level::DEBUG);
        // }

        let (rfunc, maybe_enclosing) = c.recover_values();
        let Some(enclosing) = maybe_enclosing else {
            error!(self, "Cannot return from top-level code.");
            return Expression::Function(FunctionExpression {
                name: func_name,
                params: function_expr.params,
                return_type: function_expr.return_type,
                body: function_expr.body,
            });
        };
        let _func = Rc::new(rfunc);

        self.function_compiler = enclosing;
        
        Expression::Function(FunctionExpression {
            name: func_name,
            params: function_expr.params,
            return_type: function_expr.return_type,
            body: function_expr.body,
        })
    }

    #[instrument(level = "trace", skip_all)]
    pub fn cast(&mut self, value: Expression, cast_type: UValueType, o: Cell<UValueType>) -> Expression {
        let _line = 0;

        let e = self.visit_expression(value, false);

        let status_quo = Expression::Cast(Box::new(e.clone()), cast_type, o);

        let Expression::Literal(t) = e else {
            return status_quo;
        };

        let l = match (t.clone(), cast_type.as_ref()) {
            (Value::Integer(i), ValueType::Float) => {
                Value::Float(i as f64)
            }
            (Value::Float(f), ValueType::Integer) => {
                Value::Integer(f as i64)
            }
            (Value::Integer(i), ValueType::Char) => {
                Value::Char(i as u8)
            }
            (Value::Char(c), ValueType::Integer) => {
                Value::Integer(i64::from(c))
            }
            (Value::Char(c), ValueType::Float) => {
                Value::Float(f64::from(c))
            }
            (Value::Float(f), ValueType::Char) => {
                Value::Char(f as u8)
            }
            (Value::Char(c), ValueType::Bool) => {
                Value::Bool(c != 0)
            }
            (Value::Bool(b), ValueType::Char) => {
                Value::Char(u8::from(b))
            }
            (Value::Bool(b), ValueType::Integer) => {
                Value::Integer(i64::from(b))
            }
            (Value::Integer(i), ValueType::Bool) => {
                Value::Bool(i != 0)
            }
            (Value::Float(f), ValueType::Bool) => {
                Value::Bool(f != 0.0)
            }
            (Value::Bool(b), ValueType::Float) => {
                Value::Float(if b { 1.0 } else { 0.0 })
            }
            _ => {
                error!(self, format!("Cannot cast {t:?} to {cast_type:?}").as_str());
                return status_quo;
            }
        };
        Expression::Literal(l)
    }

    #[instrument(level = "trace", skip_all)]
    pub fn array_literal(&mut self, elements: Vec<Expression>) -> Expression {
        let _line = 0;
        let mut e = Vec::new();
        for element in elements {
            e.push(self.visit_expression(element, false));
        }
        Expression::ArrayLiteral(e)
    }

    pub fn struct_literal(
        &mut self,
        t: CustomStruct,
        fields: HashMap<String, Expression>
    ) -> Expression {
        let _line = 0;
        let mut f = HashMap::new();
        for (k, v) in fields {
            f.insert(k, self.visit_expression(v, false));
        }
        Expression::StructInitializer(t, f)
    }
}
