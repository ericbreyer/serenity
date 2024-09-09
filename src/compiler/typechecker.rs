use crate::{
    lexer::{Token, TokenType},
    prelude::*,
};
use anyhow::{Context as _, Result};
use core::str;

use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, VecDeque},
    ops::Deref,
    rc::Rc,
};

use super::ffi_funcs::FfiFunc;

#[derive(Debug, Clone)]
struct Environment {
    variables: RefCell<VecDeque<HashMap<String, UValueType>>>,
}

impl Environment {
    fn new() -> Self {
        let v = Environment {
            variables: VecDeque::new().into(),
        };
        v.begin_scope();
        v
    }

    fn begin_scope(&self) {
        self.variables.borrow_mut().push_front(HashMap::new());
    }
    fn end_scope(&self) {
        self.variables.borrow_mut().pop_front();
    }

    fn get_variable(&self, name: &str) -> Result<UValueType> {
        for scope in self.variables.borrow().iter() {
            if let Some(v) = scope.get(name) {
                return Ok(*v);
            }
        }
        Err(anyhow::anyhow!("Variable {} not found", name))
    }

    fn set_variable(&self, name: &str, value: UValueType) {
        self.variables
            .borrow_mut()
            .front_mut()
            .unwrap()
            .insert(name.into(), value);
    }
}

#[derive(Debug, Clone)]
pub struct Typechecker {
    variables: Rc<Environment>,
    types: Rc<HashMap<SharedString, CustomStruct>>,

    return_type: Cell<Option<UValueType>>,
}

impl Typechecker {
    pub fn compile(&self, ast: &ASTNode) -> Result<()> {
        ast.accept(self).context("Typechecker compile")
    }

    fn register_ffi_function(&self, ffi: &FfiFunc) {
        let FfiFunc {
            name,
            ret,
            args: _,
            va: _,
        } = ffi;

        self.variables
            .set_variable(name, ValueType::ExternalFn(*ret, (*name).into()).intern());
    }

    pub fn new(custom_structs: HashMap<SharedString, CustomStruct>, ffi_funcs: &[FfiFunc]) -> Self {
        let variables = Environment::new().into();

        let c = Typechecker {
            variables,
            types: custom_structs.into(),
            return_type: None.into(),
        };

        ffi_funcs
            .into_iter()
            .for_each(|f| c.register_ffi_function(f));

        c
    }

    pub fn compile_function(&self) -> Self {
        let c = self.clone();
        c.return_type.set(None);
        c
    }
}

impl Typechecker {
    fn get_variable(&self, name: &str) -> Result<UValueType> {
        self.variables.get_variable(name)
    }

    fn set_variable(&self, name: &str, value: UValueType) {
        self.variables.set_variable(name, value);
    }
}

impl NodeVisitor<Result<()>> for Typechecker {
    fn visit_statement(&self, statement: &Statement) -> Result<()> {
        statement.accept(self)
    }

    fn visit_expression(&self, expression: &Expression) -> Result<()> {
        let _v = expression.accept(self)?;
        Ok(())
    }

    fn visit_declaration(&self, declaration: &Declaration) -> Result<()> {
        declaration.accept(self)
    }
}

#[derive(Debug, Clone)]
struct ExprResultInner(UValueType);
impl Deref for ExprResultInner {
    type Target = ValueType;

    fn deref(&self) -> &Self::Target {
        &self.0.as_ref()
    }
}

type ExprResult = Result<ExprResultInner>;

impl ExprResultInner {
    fn new(serenity_type: UValueType) -> Self {
        Self(serenity_type)
    }

    fn rvalue(self) -> ExprResult {
        match self.0.as_ref() {
            ValueType::LValue(ref t, _) => match t.as_ref() {
                ValueType::Array(a, _) => Ok(ExprResultInner::new(ValueType::Pointer(*a, false).intern())),
                _ => Ok(ExprResultInner::new(*t)),
            },
            _ => Ok(self),
        }
    }
}

impl ExpressionVisitor<ExprResult> for Typechecker {
    fn visit_literal_expression(&self, expression: &LiteralExpression) -> ExprResult {
        Ok(match expression.value {
            Value::Integer(_i) => ExprResultInner::new(ValueType::Integer.intern()),
            Value::UInteger(_i) => ExprResultInner::new(ValueType::Integer.intern()),
            Value::Float(_f) => ExprResultInner::new(ValueType::Float.intern()),
            Value::Char(_c) => ExprResultInner::new(ValueType::Char.intern()),
            Value::Bool(_b) => ExprResultInner::new(ValueType::Bool.intern()),
        })
    }

    fn visit_string_literal_expression(&self, _expression: &StringLiteralExpression) -> ExprResult {
        Ok(ExprResultInner::new(
            ValueType::Pointer(ValueType::Char.intern(), false).intern(),
        ))
    }

    fn visit_unary_expression(&self, expression: &UnaryExpression) -> ExprResult {
        let operand_t = expression
            .operand
            .accept(self)
            .context("unary operand")?
            .rvalue()
            .context("rvalue")?;

        Ok(ExprResultInner::new(match expression.operator {
            TokenType::Minus => match operand_t.0.as_ref() {
                ValueType::Integer => ValueType::Integer.intern(),
                ValueType::Float => ValueType::Float.intern(),
                _ => return Err(anyhow::anyhow!("Invalid minus unary expression")),
            },
            TokenType::Bang => match operand_t.0.as_ref() {
                ValueType::Bool => ValueType::Bool.intern(),
                _ => return Err(anyhow::anyhow!("Invalid unary expression")),
            },
            _ => return Err(anyhow::anyhow!("Invalid unary expression ")),
        }))
    }

    fn visit_deref_expression(&self, expression: &DerefExpression) -> ExprResult {
        let expr = expression
            .operand
            .accept(self)
            .context("Deref expression operand")?
            .rvalue()
            .context("Rvalue")?;
        let ValueType::Pointer(t, _) = *expr else {
            return Err(anyhow::anyhow!("Invalid deref expression"));
        };
        Ok(ExprResultInner::new(ValueType::LValue(t, false).intern()))
    }

    fn visit_ref_expression(&self, expression: &RefExpression) -> ExprResult {
        let expr = expression
            .operand
            .accept(self)
            .context("Ref expression operand")?;
        let ValueType::LValue(t, _) = *expr else {
            return Err(anyhow::anyhow!("Invalid ref expression"));
        };
        return Ok(ExprResultInner::new(ValueType::Pointer(t, true).intern()));
    }

    fn visit_index_expression(&self, expression: &IndexExpression) -> ExprResult {
        let pointer = expression
            .array
            .accept(self)
            .context("Index expression array")?.rvalue()?;
        let index = expression
            .index
            .accept(self)
            .context("Index expression index")?
            .rvalue()
            .context("Rvalue")?;
        ValueType::unify(ValueType::Integer.intern(), index.0)?;

        
        let t = match pointer.0.as_ref() {
            ValueType::Pointer(a, _) => {
                a
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid index expression array type was {:?}",
                    pointer
                ))
            }
        };

        Ok(ExprResultInner::new(ValueType::LValue(*t, false).intern()))
    }

    fn visit_binary_expression(&self, expression: &BinaryExpression) -> ExprResult {
        let lhs_ptr = expression
            .left
            .accept(self)
            .context("Left hand side of binary expression")?;
        let lhs = lhs_ptr.clone().rvalue().context("Rvalue")?;
        let rhs = expression
            .right
            .accept(self)
            .context("Right hand side of binary expression")?
            .rvalue()
            .context("Rvalue")?;

        if matches!(*lhs, ValueType::Array(_, _) | ValueType::Pointer(_, _)) {
            ValueType::unify(ValueType::Integer.intern(), rhs.0)?;
        } else {
            ValueType::unify(lhs.0, rhs.0)?;
        }

        let expr = match (expression.operator, lhs.deref(), rhs.deref()) {
            (TokenType::Plus, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Integer.intern())
            }

            (TokenType::Plus, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Float.intern())
            }
            (TokenType::Plus, ValueType::Pointer(_, _), ValueType::Integer) => {
                ExprResultInner::new(lhs.0)
            }
            (TokenType::Plus, ValueType::Array(t1, s), ValueType::Integer) if s.is_some() => {
                ExprResultInner::new(ValueType::Pointer(*t1, false).intern())
            }
            (TokenType::Minus, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Integer.intern())
            }
            (TokenType::Minus, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Float.intern())
            }
            (TokenType::Star, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Integer.intern())
            }
            (TokenType::Star, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Float.intern())
            }
            (TokenType::Slash, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Integer.intern())
            }

            (TokenType::Slash, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Float.intern())
            }
            (TokenType::Greater, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }

            (TokenType::Greater, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::GreaterEqual, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }

            (TokenType::GreaterEqual, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::Less, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }

            (TokenType::Less, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::LessEqual, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }

            (TokenType::LessEqual, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::EqualEqual, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }

            (TokenType::EqualEqual, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::BangEqual, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::BangEqual, ValueType::Float, ValueType::Float) => {
                ExprResultInner::new(ValueType::Bool.intern())
            }
            (TokenType::Percent, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(ValueType::Integer.intern())
            }
            (_, ValueType::TypeVar(x), ValueType::TypeVar(y)) => {
                if x == y {
                    ExprResultInner::new(ValueType::TypeVar(*x).intern())
                } else {
                    return Err(anyhow::anyhow!(
                        "Invalid binary expression {:?} {:?} {:?}",
                        expression.operator,
                        lhs,
                        rhs
                    ));
                }
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid binary expression {:?} {:?} {:?}",
                    expression.operator,
                    lhs,
                    rhs
                ))
            }
        };

        Ok(expr)
    }

    fn visit_ternary_expression(&self, _expression: &TernaryExpression) -> ExprResult {
        todo!()
    }

    fn visit_variable_expression(&self, expression: &VariableExpression) -> ExprResult {
        let name = &expression.token.lexeme;
        let tipe = self.get_variable(name)?;
        Ok(ExprResultInner::new(
            ValueType::LValue(tipe, false).intern(),
        ))
    }

    fn visit_assign_expression(&self, expression: &AssignExpression) -> ExprResult {
        let rhs = expression
            .value
            .accept(self)
            .context("Assign expression rhs")?
            .rvalue()
            .context("Rvalue")?;
        let lhs = expression
            .variable
            .accept(self)
            .context("Assign expression lhs")?;

        let ValueType::LValue(t, _) = *lhs else {
            return Err(anyhow::anyhow!("Invalid assign expression lhs"));
        };
        ValueType::unify(t.decay(), rhs.0).context(format!(
            "Assign expression unification \n{:?}\n{:?}\nepxr: {:?}",
            t.decay(),
            rhs,
            ASTNode::Expression(Expression::Assign(expression.clone()))
        ))?;

        Ok(ExprResultInner::new(t))
    }

    fn visit_logical_expression(&self, expression: &LogicalExpression) -> ExprResult {
        let lhs = expression
            .left
            .accept(self)
            .context("Logical expression lhs")?
            .rvalue()
            .context("Rvalue")?;

        ValueType::unify(ValueType::Bool.intern(), lhs.0).context(format!(
            "Logical expression unification {:?} = {:?}",
            ValueType::Bool.intern(),
            lhs.0
        ))?;

        let rhs = expression
            .right
            .accept(self)
            .context("Logical expression rhs")?
            .rvalue()
            .context("Rvalue")?;

        ValueType::unify(ValueType::Bool.intern(), rhs.0).context(format!(
            "Logical expression unification {:?} = {:?}",
            ValueType::Bool.intern(),
            rhs.0
        ))?;

        Ok(ExprResultInner::new(ValueType::Bool.intern()))
    }

    fn visit_call_expression(&self, expression: &CallExpression) -> ExprResult {
        let callee = expression
            .callee
            .accept(self)
            .context("Evaluate Calee")?
            .rvalue()
            .context("Rvalue Calee")?;
        let args = expression
            .arguments
            .iter()
            .map(|arg| {
                arg.accept(self)
                    .context("Argument")?
                    .rvalue()
                    .context("Rvalue")
            })
            .collect::<Result<Vec<_>>>()?;
        let arg_types = args.iter().map(|a| a.0).collect::<Vec<_>>();

        let fn_type = callee;

        if let ValueType::ExternalFn(r, _name) = fn_type.deref() {
            return Ok(ExprResultInner::new(*r));
        }

        let ValueType::Closure(args, _uvals, r) = fn_type.deref() else {
            return Err(anyhow::anyhow!(
                "Invalid callee serenity type got {:?} {:?}",
                fn_type,
                ASTNode::Expression(Expression::Call(expression.clone()))
            ));
        };

        for arg in args.iter().zip(arg_types.iter()) {
            ValueType::unify(arg.0.decay(), arg.1.decay()).context(format!(
                "Call expression unification {:?} = {:?}",
                arg.0.decay(),
                arg.1.decay()
            ))?;
        }

        Ok(ExprResultInner::new(ValueType::LValue(*r, false).intern()))
    }

    fn visit_dot_expression(&self, expression: &DotExpression) -> ExprResult {
        let lhs = expression
            .object
            .accept(self)
            .context("Dot expression lhs")?;
        let ValueType::LValue(st, _) = *lhs else {
            return Err(anyhow::anyhow!("Invalid dot expression lhs"));
        };

        let st = st.fill_self_struct((*self.types).clone());
        let ValueType::Struct(t) = st.as_ref().clone() else {
            return Err(anyhow::anyhow!(
                "Invalid dot expression lhs got {:?} on {:?}",
                st,
                expression.line_no
            ));
        };

        let ofield = t
            .fields
            .borrow()
            .iter()
            .enumerate()
            .find(|(_i, (name, _t))| *name == &expression.field)
            .map(|(i, _)| i);

        if !ofield.is_some() {
            let method_name = &format!("{}_{}", st.unique_string(), expression.field).into();
            if let Some(method_name) = t.methods.borrow().get(method_name) {
                return Expression::Call(CallExpression {
                    callee: Box::new(Expression::Variable(VariableExpression {
                        token: Token {
                            lexeme: method_name.clone(),
                            token_type: TokenType::Identifier,
                            line: 0,
                        },
                        line_no: 0,
                    })),
                    arguments: vec![Expression::Ref(RefExpression {
                        operand: expression.object.clone(),
                        line_no: 0,
                    })],
                    line_no: 0,
                })
                .accept(self);
            }
            return Err(anyhow::anyhow!(
                "Field {} not found in struct {:?}",
                method_name,
                t.methods
            ));
        }

        Ok(ExprResultInner::new(
            ValueType::LValue(
                t.fields.clone().borrow()[&expression.field].value.clone(),
                false,
            )
            .intern(),
        ))
    }

    fn visit_function_expression(&self, expression: &FunctionExpression) -> ExprResult {
        let fn_type = self.function_type_serenity(&expression.prototype);
        let r_type = self.function_body(expression.prototype.clone(), expression.body.clone())?;

        ValueType::unify(expression.prototype.return_type, r_type).context(format!(
            "Function expression unification {:?} = {:?}",
            expression.prototype.return_type,
            r_type
        ))?;

        Ok(ExprResultInner::new(fn_type))
    }

    fn visit_cast_expression(&self, expression: &CastExpression) -> ExprResult {
        let _expr = expression
            .expression
            .accept(self)
            .context("Cast expression operand")?
            .rvalue()
            .context("Rvalue")?;

        // ValueType::unify(expr.0, expression.target_type.unwrap())?;

        Ok(ExprResultInner(expression.target_type))
    }

    fn visit_struct_initializer_expression(
        &self,
        expression: &StructInitializerExpression,
    ) -> ExprResult {
        for (name, expr) in expression.fields.iter() {
            let t = expression
                .struct_type
                .fields
                .borrow()
                .get(name)
                .map(|f| f.value.clone())
                .ok_or_else(|| anyhow::anyhow!("Field {} not found in struct", name))?;
            let expr_t = expr.accept(self)?.rvalue()?;
            ValueType::unify(t.decay(), expr_t.0).context(format!(
                "Struct initializer expression unification {:?} = {:?}",
                t,
                expr_t.0
            ))?;
        }
        Ok(ExprResultInner::new(
            ValueType::Struct(expression.struct_type.clone()).intern(),
        ))
    }

    fn visit_sizeof_expression(&self, _expression: &SizeofExpression) -> ExprResult {
        Ok(ExprResultInner::new(ValueType::Integer.intern()))
    }
}

impl Typechecker {
    fn function_type_serenity(&self, prototype: &Prototype) -> UValueType {
        ValueType::Closure(
            prototype
                .params
                .iter()
                .map(|(_s, t, _m)| t.decay())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype
                .captures
                .iter()
                .map(|capture| self.get_variable(capture).unwrap())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype.return_type,
        )
        .intern()
    }

    fn function_body(
        &self,
        prototype: Prototype,
        body: Option<Vec<ASTNode>>,
    ) -> Result<UValueType> {
        let args = &prototype.params;
        let caps = &prototype.captures;

        self.variables.begin_scope();

        for (_i, s) in caps.iter().enumerate() {
            let arg_type_serenity = self.get_variable(s).unwrap();
            self.set_variable(s, arg_type_serenity);
        }

        for (_i, &(ref s, t, _m)) in args.iter().enumerate() {
            self.set_variable(s, t.decay());
        }

        let ret = if let Some(body) = &body {
            let fc = self.compile_function();
            for statement in body {
                statement.accept(&fc).context("Function body")?;
            }
            let ret = fc
                .return_type
                .get()
                .unwrap_or_else(|| ValueType::Nil.intern());

            ValueType::unify(prototype.return_type, ret).context(format!(
                "Function return unification \n{:?}\n{:?}",
                prototype.return_type, ret
            ))?;
            ret
        } else {
            prototype.return_type
        };

        self.variables.end_scope();

        Ok(ret)
    }
}

impl DeclarationVisitor<Result<()>> for Typechecker {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> Result<()> {
        let var_type = declaration.tipe;
        if let Some(init) = &declaration.initializer {
            let init_type = init.accept(self)?.rvalue()?;
            ValueType::unify(var_type, init_type.0).context(format!(
                "Var declaration unification {:?} = {:?}",
                var_type, init_type.0
            ))?;
        }

        self.set_variable(&declaration.name, var_type);
        Ok(())
    }

    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> Result<()> {
        let _return_t = self
            .function_body(declaration.prototype.clone(), declaration.body.clone())
            .context(format!(
                "Function declaration {:?}",
                ASTNode::Declaration(Declaration::Function(declaration.clone()))
            ))?;
        // ValueType::unify(declaration.prototype.return_type, return_t)?;

        self.set_variable(
            &declaration.prototype.name,
            self.function_type_serenity(&declaration.prototype),
        );

        Ok(())
    }
}

impl StatementVisitor<Result<()>> for Typechecker {
    fn visit_block_statement(&self, statement: &BlockStatement) -> Result<()> {
        self.variables.begin_scope();
        for s in &statement.statements {
            s.accept(self)?;
        }
        self.variables.end_scope();
        Ok(())
    }

    fn visit_if_statement(&self, statement: &IfStatement) -> Result<()> {
        let cond = statement.condition.accept(self)?.rvalue()?;
        ValueType::unify(ValueType::Bool.intern(), cond.0).context(format!(
            "If statement unification {:?} = {:?}",
            ValueType::Bool.intern(),
            cond.0
        ))?;

        statement.then_branch.accept(self)?;

        if let Some(el) = &statement.else_branch {
            el.accept(self)?;
        }

        Ok(())
    }

    fn visit_while_statement(&self, statement: &WhileStatement) -> Result<()> {
        let cond = statement.condition.accept(self)?.rvalue()?;

        ValueType::unify(ValueType::Bool.intern(), cond.0)?;

        statement.body.accept(self)?;

        Ok(())
    }

    fn visit_for_statement(&self, _statement: &ForStatement) -> Result<()> {
        todo!()
    }

    fn visit_break_statement(&self, _statement: &BreakStatement) -> Result<()> {
        Ok(())
    }

    fn visit_continue_statement(&self, _statement: &ContinueStatement) -> Result<()> {
        Ok(())
    }

    fn visit_return_statement(&self, statement: &ReturnStatement) -> Result<()> {
        if let Some(value) = &statement.value {
            let v = value.accept(self)?.rvalue()?;
            self.return_type.set(Some(v.0));
        } else {
            self.return_type.set(Some(ValueType::Nil.intern()));
        }
        Ok(())
    }

    fn visit_expression_statement(&self, statement: &ExpressionStatement) -> Result<()> {
        statement
            .expr
            .accept(self)
            .context(format!("Expression statement line {:?}", statement.line_no))
            .map(|_| ())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{compiler::ffi_funcs, parser::Parser};
    use test_case::test_case;
    #[test_case(r##"
        fn main() -> int {
            let x: int = 1;
            return x;
        }
        "##; "var_expr_fully_typed")]
    #[test_case(r##"
        fn main() -> int {
            let x = 1;
            return x;
        }
        "##; "var_expr_inferred")]
    #[test_case(r##"
        fn main() -> int {
            let x;
            x = 1;
            return x;
        }
        "##; "var_expr_assign")]
    fn test_program_integer_return(prog: &str) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into()).unwrap();

        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());
        for n in ast.ast {
            let r = c.compile(&n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }
    }
}
