use crate::{lexer::TokenType, prelude::*};
use anyhow::{Context as _, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{AnyValue, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use std::{cell::RefCell, collections::HashMap, f32::consts::E};

struct LLVMCompiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: RefCell<HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>>,

    function: Option<&'a FunctionValue<'ctx>>,
}

pub fn compile<'a, 'ctx>(
    ast: &ASTNode,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,

) -> Result<()> {
    let compiler = LLVMCompiler {
        context,
        builder,
        module,
        variables: HashMap::new().into(),
        function: None,
    };

    ast.accept(&compiler)?;

    Ok(())
}

impl<'a, 'ctx> LLVMCompiler<'a, 'ctx> {
    fn with_function(
        &self,
        function: &'a FunctionValue<'ctx>,
    ) -> LLVMCompiler<'a, 'ctx> {
        LLVMCompiler {
            context: self.context,
            builder: self.builder,
            module: self.module,
            variables: HashMap::new().into(),
            function: Some(function),
        }
    }
}

impl NodeVisitor<Result<()>> for LLVMCompiler<'_, '_> {
    fn visit_statement(&self, statement: &Statement) -> Result<()> {
        statement.accept(self)
    }

    fn visit_expression(&self, expression: &Expression) -> Result<()> {
        let v = expression.accept(self)?;
        Ok(())
    }

    fn visit_declaration(&self, declaration: &Declaration) -> Result<()> {
        declaration.accept(self)
    }
}

impl<'a, 'ctx> ExpressionVisitor<Result<BasicValueEnum<'ctx>>> for LLVMCompiler<'a, 'ctx> {
    fn visit_literal_expression(
        &self,
        expression: &LiteralExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        Ok(match expression.value {
            Value::Integer(i) => self.context.i64_type().const_int(i as u64, false).into(),
            Value::UInteger(i) => self.context.i64_type().const_int(i as u64, true).into(),
            Value::Float(f) => self.context.f64_type().const_float(f).into(),
            Value::Char(c) => self.context.i8_type().const_int(c as u64, false).into(),
            Value::Bool(b) => self.context.bool_type().const_int(b as u64, false).into(),
            Value::Pointer(p) => {
                let ptrint = self
                    .context
                    .i64_type()
                    .const_int(p.to_word().to_u64(), false);
                let ptr = self.builder.build_int_to_ptr(
                    ptrint,
                    self.context.ptr_type(AddressSpace::default()),
                    "ptr",
                );
                ptr.unwrap().into()
            }
        })
    }

    fn visit_string_literal_expression(
        &self,
        _expression: &StringLiteralExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_unary_expression(&self, expression: &UnaryExpression) -> Result<BasicValueEnum<'ctx>> {
        let operand = expression
            .operand
            .accept(self)
            .context("Unary expression operand")?;
        let expr = match expression.operator {
            TokenType::Minus => match operand {
                BasicValueEnum::IntValue(operand) => self
                    .builder
                    .build_int_neg(operand, "negtmp")
                    .context("Build negation")
                    .map(BasicValueEnum::IntValue),
                BasicValueEnum::FloatValue(operand) => self
                    .builder
                    .build_float_neg(operand, "negtmp")
                    .context("Build negation")
                    .map(BasicValueEnum::FloatValue),
                _ => Err(anyhow::anyhow!(
                    "Invalid unary expression {:?} {:?}",
                    expression.operator,
                    operand
                )),
            }?,
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid unary expression {:?} {:?}",
                    expression.operator,
                    operand
                ))
            }
        };
        Ok(expr.into())
    }

    fn visit_deref_expression(
        &self,
        _expression: &DerefExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_ref_expression(&self, _expression: &RefExpression) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_index_expression(
        &self,
        _expression: &IndexExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_binary_expression(
        &self,
        expression: &BinaryExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        let lhs = expression
            .left
            .accept(self)
            .context("Left hand side of binary expression")?;
        let rhs = expression
            .right
            .accept(self)
            .context("Right hand side of binary expression")?;

        let expr = match (expression.operator, lhs, rhs) {
            (TokenType::Plus, BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                .builder
                .build_int_add(lhs, rhs, "addtmp")
                .context("Build addition")
                .map(BasicValueEnum::IntValue),
            (TokenType::Minus, BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                self.builder
                    .build_int_sub(lhs, rhs, "subtmp")
                    .context("Build subtraction")
                    .map(BasicValueEnum::IntValue)
            }
            (TokenType::Star, BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                .builder
                .build_int_mul(lhs, rhs, "multmp")
                .context("Build multiplication")
                .map(BasicValueEnum::IntValue),
            (TokenType::Slash, BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                self.builder
                    .build_int_signed_div(lhs, rhs, "divtmp")
                    .context("Build division")
                    .map(BasicValueEnum::IntValue)
            }

            (TokenType::Plus, BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                self.builder
                    .build_float_add(lhs, rhs, "addtmp")
                    .context("Build addition")
                    .map(BasicValueEnum::FloatValue)
            }
            (
                TokenType::Minus,
                BasicValueEnum::FloatValue(lhs),
                BasicValueEnum::FloatValue(rhs),
            ) => self
                .builder
                .build_float_sub(lhs, rhs, "subtmp")
                .context("Build subtraction")
                .map(BasicValueEnum::FloatValue),
            (TokenType::Star, BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                self.builder
                    .build_float_mul(lhs, rhs, "multmp")
                    .context("Build multiplication")
                    .map(BasicValueEnum::FloatValue)
            }
            (
                TokenType::Slash,
                BasicValueEnum::FloatValue(lhs),
                BasicValueEnum::FloatValue(rhs),
            ) => self
                .builder
                .build_float_div(lhs, rhs, "divtmp")
                .context("Build division")
                .map(BasicValueEnum::FloatValue),

            _ => Err(anyhow::anyhow!(
                "Invalid binary expression {:?} {:?} {:?}",
                expression.operator,
                lhs,
                rhs
            )),
        }?;

        Ok(expr.into())
    }

    fn visit_ternary_expression(
        &self,
        _expression: &TernaryExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_variable_expression(
        &self,
        _expression: &VariableExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_assign_expression(
        &self,
        _expression: &AssignExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_logical_expression(
        &self,
        _expression: &LogicalExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_call_expression(&self, _expression: &CallExpression) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_dot_expression(&self, _expression: &DotExpression) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_function_expression(
        &self,
        _expression: &FunctionExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_cast_expression(&self, _expression: &CastExpression) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }

    fn visit_struct_initializer_expression(
        &self,
        _expression: &StructInitializerExpression,
    ) -> Result<BasicValueEnum<'ctx>> {
        todo!()
    }
}
impl<'ctx> LLVMCompiler<'_, 'ctx> {
    fn get_variable(&self, name: &str) -> Result<(PointerValue, BasicTypeEnum)> {
        self.variables.borrow()
            .get(name)
            .cloned()
            .context(format!("Variable {} not found", name))
    }

    fn set_variable(&self, name: &str, value: (PointerValue<'ctx>, BasicTypeEnum<'ctx>)) {
        self.variables.borrow_mut().insert(name.into(), value);
    }
}
impl<'a, 'ctx> DeclarationVisitor<Result<()>> for LLVMCompiler<'a, 'ctx> {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> Result<()> {
        if self.function.is_none() {
            let var = self.module.add_global(
                (*declaration.tipe.unwrap()).clone().llvm(&self.context),
                Some(AddressSpace::default()),
                &declaration.name,
            );
            if let Some(initializer) = &declaration.initializer {
                let value = initializer.accept(self)?;
                var.set_initializer(&value);
            }
            self.set_variable(&declaration.name, (var.as_pointer_value(), var.get_value_type().try_into().map_err(|_| anyhow::anyhow!("Invalid type"))?));
            Ok(())
        } else {
            let function = self.function.unwrap();
            let var_type = (*declaration.tipe.unwrap()).clone().llvm(&self.context);
            
            let entry = function.get_first_basic_block().unwrap_or_else(|| self.context.append_basic_block(*function, "entry"));
            self.builder.position_at_end(entry);

            let var = self.builder.build_alloca(var_type, &declaration.name).context("Variable allocation")?;

            if let Some(initializer) = &declaration.initializer {
                let value = initializer.accept(self)?;
                self.builder.build_store(var, value)?;
            }

            self.set_variable(&declaration.name, (var, var_type));

            Ok(())
        }
    }

    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> Result<()> {
        let fn_type = (*declaration.prototype.return_type)
            .clone()
            .llvm(&self.context)
            .fn_type(&[], false);
        let fn_value =
            self.module
                .add_function(&format!("{}_fn", declaration.prototype.name), fn_type, None);

        let closure_type = self.context.struct_type(
            [
                declaration
                    .prototype
                    .captures
                    .iter()
                    .map(|capture| self.get_variable(&capture).unwrap().1)
                    .collect::<Vec<_>>()
                    .as_slice(),
                &[self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()],
            ]
            .concat()
            .as_slice(),
            false,
        );

        let closure = self.module.add_global(
            closure_type,
            Some(AddressSpace::default()),
            &declaration.prototype.name,
        );

        let mut struct_init = declaration
            .prototype
            .captures
            .iter()
            .map(|capture| self.get_variable(&capture).unwrap().0.as_basic_value_enum())
            .collect::<Vec<_>>();
        struct_init.push(
            fn_value
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum(),
        );

        closure.set_initializer(&closure_type.const_named_struct(struct_init.as_slice()));

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        if let Some(body) = &declaration.body {
            let compiler = self.with_function(&fn_value);
            for statement in body {
                statement.accept(&compiler).context("Function body")?;
            }
        }

        Ok(())
    }

    fn visit_array_declaration(&self, _declaration: &ArrayDeclaration) -> Result<()> {
        todo!()
    }
}

impl<'a, 'ctx> StatementVisitor<Result<()>> for LLVMCompiler<'a, 'ctx> {
    fn visit_print_statement(&self, _statement: &PrintStatement) -> Result<()> {
        todo!()
    }

    fn visit_block_statement(&self, _statement: &BlockStatement) -> Result<()> {
        todo!()
    }

    fn visit_if_statement(&self, _statement: &IfStatement) -> Result<()> {
        todo!()
    }

    fn visit_while_statement(&self, _statement: &WhileStatement) -> Result<()> {
        todo!()
    }

    fn visit_for_statement(&self, _statement: &ForStatement) -> Result<()> {
        todo!()
    }

    fn visit_break_statement(&self, _statement: &BreakStatement) -> Result<()> {
        todo!()
    }

    fn visit_continue_statement(&self, _statement: &ContinueStatement) -> Result<()> {
        todo!()
    }

    fn visit_return_statement(&self, statement: &ReturnStatement) -> Result<()> {
        if let Some(value) = &statement.value {
            let v = value.accept(self)?;
            self.builder.build_return(Some(&v))?;
        } else {
            self.builder.build_return(None)?;
        }
        Ok(())
    }

    fn visit_expression_statement(&self, statement: &ExpressionStatement) -> Result<()> {
        statement
            .expr
            .accept(self)
            .context("Expression statement")
            .map(|_| ())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use inkwell::OptimizationLevel;
    use insta::assert_snapshot;
    use test_case::test_case;

    #[test_case(Expression::Literal(LiteralExpression{value: Value::Integer(1), line_no: 0}), 1; "integer")]
    #[test_case(Expression::Binary(BinaryExpression{operator: TokenType::Plus, left: Box::new(Expression::Literal(LiteralExpression{value: Value::Integer(1), line_no: 0})), right: Box::new(Expression::Literal(LiteralExpression{value: Value::Integer(1), line_no: 0})), line_no: 0}), 2; "addition")]
    #[test_case(Expression::Unary(UnaryExpression{operator: TokenType::Minus, operand: Box::new(Expression::Literal(LiteralExpression{value: Value::Integer(1), line_no: 0})), line_no: 0}), -1; "negation")]
    fn test_expression_compile_integer(expression: Expression, expected: i64) {
        let anon_ast = ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
            prototype: Prototype {
                name: "anon".into(),
                captures: vec![],
                params: vec![],
                return_type: ValueType::Integer.intern(),
                line_no: 0,
            },
            line_no: 0,
            body: vec![ASTNode::Statement(Statement::Return(ReturnStatement {
                value: Some(Box::new(expression)),
                line_no: 0,
            }))]
            .into(),
        }));
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let r = compile(&anon_ast, &context, &module, &builder);
        assert!(r.is_ok(), "{:#}", r.unwrap_err());
        println!("---\n{}---\n", module.print_to_string().to_string());

        let jit = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        unsafe {
            let func = jit
                .get_function::<unsafe extern "C" fn() -> i64>("anon_fn")
                .unwrap();
            assert_eq!(func.call(), expected);
        }
    }

    #[test]
    fn test_global_var_declaration() {
        let anon_ast = ASTNode::Declaration(Declaration::Var(VarDeclaration {
            name: "x".into(),
            initializer: Some(Box::new(Expression::Literal(LiteralExpression {
                value: Value::Integer(1),
                line_no: 0,
            }))),
            tipe: ValueType::Integer.intern().into(),
            mutable: false,
            line_no: 0,
        }));
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let r = compile(&anon_ast, &context, &module, &builder);
        assert!(r.is_ok(), "{:#}", r.unwrap_err());
        println!("---\n{}---\n", module.print_to_string().to_string());

        assert_snapshot!(module.print_to_string().to_string());
    }

    #[test]
    fn test_local_var_declaration() {
        let anon_ast = ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
            prototype: Prototype {
                name: "anon".into(),
                captures: vec![],
                params: vec![],
                return_type: ValueType::Integer.intern(),
                line_no: 0,
            },
            line_no: 0,
            body: vec![ASTNode::Declaration(Declaration::Var(VarDeclaration {
                name: "x".into(),
                initializer: Some(Box::new(Expression::Literal(LiteralExpression {
                    value: Value::Integer(1),
                    line_no: 0,
                }))),
                tipe: ValueType::Integer.intern().into(),
                mutable: false,
                line_no: 0,
            }))]
            .into(),
        }));
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let r = compile(&anon_ast, &context, &module, &builder);
        assert!(r.is_ok(), "{:#}", r.unwrap_err());
        println!("---\n{}---\n", module.print_to_string().to_string());

        assert_snapshot!(module.print_to_string().to_string());
    }
}
