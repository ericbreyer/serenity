use crate::{lexer::TokenType, prelude::*};
use anyhow::{Context as _, Result};
use core::str;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{
        BasicMetadataTypeEnum, BasicType, StructType,
    },
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
        StructValue,
    },
    AddressSpace,
};
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    ops::Deref,
};

struct Variables<'ctx> {
    variables: RefCell<VecDeque<HashMap<String, (PointerValue<'ctx>, UValueType)>>>,
}

impl<'ctx> Variables<'ctx> {
    fn new() -> Self {
        let v = Variables {
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

    fn get_variable(&self, name: &str) -> Result<(PointerValue<'ctx>, UValueType)> {
        for scope in self.variables.borrow().iter() {
            if let Some(v) = scope.get(name) {
                return Ok(*v);
            }
        }
        Err(anyhow::anyhow!("Variable {} not found", name))
    }

    fn set_variable(&self, name: &str, value: (PointerValue<'ctx>, UValueType)) {
        self.variables
            .borrow_mut()
            .front_mut()
            .unwrap()
            .insert(name.into(), value);
    }
}

struct LLVMCompiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    variables: Variables<'ctx>,
}

pub fn compile<'ctx>(context: &'ctx Context, pr: ParseResult) -> Result<Module<'ctx>> {
    let compiler = LLVMCompiler::new(&context);
    for ast in pr.ast {
        compiler.compile(&ast)?;
    }
    Ok(compiler.module)
}

impl<'ctx> LLVMCompiler<'ctx> {
    fn compile(&self, ast: &ASTNode) -> Result<()> {
        let fc = LLVMFunctionCompiler::new(
            self.context,
            &self.module,
            &self.builder,
            &self.variables
        );
        fc.compile(ast)
    }

    fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();
        LLVMCompiler {
            context,
            builder,
            module,
            variables: Variables::new(),
        }
    }
}

struct LLVMFunctionCompiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: &'a Variables<'ctx>,

    function: Option<&'a FunctionValue<'ctx>>,
}

impl<'a, 'ctx> LLVMFunctionCompiler<'a, 'ctx> {
    pub fn compile(&self, ast: &ASTNode) -> Result<()> {
        ast.accept(self)
    }

    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        variables: &'a Variables<'ctx>,
    ) -> Self {
        LLVMFunctionCompiler {
            context,
            builder,
            module,
            variables,
            function: None,
        }
    }
}

impl<'a, 'ctx> LLVMFunctionCompiler<'a, 'ctx> {
    fn with_function(&self, function: &'a FunctionValue<'ctx>) -> LLVMFunctionCompiler<'a, 'ctx> {
        LLVMFunctionCompiler {
            context: self.context,
            builder: self.builder,
            module: self.module,
            variables: self.variables,
            function: Some(function),
        }
    }
    fn get_variable(&self, name: &str) -> Result<(PointerValue<'ctx>, UValueType)> {
        self.variables.get_variable(name)
    }

    fn set_variable(&self, name: &str, value: (PointerValue<'ctx>, UValueType)) {
        self.variables.set_variable(name, value);
    }
}

impl NodeVisitor<Result<()>> for LLVMFunctionCompiler<'_, '_> {
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
struct ExprResultInner<'ctx> {
    value: BasicValueEnum<'ctx>,
    serenity_type: UValueType,
}

type ExprResult<'ctx> = Result<ExprResultInner<'ctx>>;

impl<'ctx> ExprResultInner<'ctx> {
    fn new(value: BasicValueEnum<'ctx>, serenity_type: UValueType) -> Self {
        Self {
            value,
            serenity_type,
        }
    }

    fn rvalue(self, compiler: &LLVMFunctionCompiler<'_, 'ctx>) -> ExprResult<'ctx> {
        match self.serenity_type.as_ref() {
            ValueType::LValue(ref t, _) => {
                let ptr = self.value.into_pointer_value();
                let v = compiler
                    .builder
                    .build_load((t.deref()).clone().llvm(compiler.context), ptr, "rval")
                    .context("Rvalue")?;
                Ok(ExprResultInner::new(v, *t))
            }
            _ => Ok(self),
        }
    }
}

impl<'a, 'ctx> ExpressionVisitor<ExprResult<'ctx>> for LLVMFunctionCompiler<'a, 'ctx> {
    fn visit_literal_expression(&self, expression: &LiteralExpression) -> ExprResult<'ctx> {
        Ok(match expression.value {
            Value::Integer(i) => ExprResultInner::new(
                self.context.i64_type().const_int(i as u64, false).into(),
                ValueType::Integer.intern(),
            ),
            Value::UInteger(i) => ExprResultInner::new(
                self.context.i64_type().const_int(i, true).into(),
                ValueType::UInteger.intern(),
            ),
            Value::Float(f) => ExprResultInner::new(
                self.context.f64_type().const_float(f).into(),
                ValueType::Float.intern(),
            ),
            Value::Char(c) => ExprResultInner::new(
                self.context.i8_type().const_int(c as u64, false).into(),
                ValueType::Char.intern(),
            ),
            Value::Bool(b) => ExprResultInner::new(
                self.context.bool_type().const_int(b as u64, false).into(),
                ValueType::Bool.intern(),
            ),
            Value::Pointer(p) => {
                let ptrint = self
                    .context
                    .i64_type()
                    .const_int(p.to_word().to_u64(), false);
                let ptr = self.builder.build_int_to_ptr(
                    ptrint,
                    self.context.ptr_type(AddressSpace::default()),
                    "ptr",
                )?;
                ExprResultInner::new(
                    ptr.into(),
                    ValueType::Pointer(ValueType::Nil.intern(), false).intern(),
                )
            }
        })
    }

    fn visit_string_literal_expression(
        &self,
        _expression: &StringLiteralExpression,
    ) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_unary_expression(&self, expression: &UnaryExpression) -> ExprResult<'ctx> {
        let operand = expression
            .operand
            .accept(self)
            .context("Unary expression operand")?
            .rvalue(self)
            .context("Rvalue")?;
        let expr = match expression.operator {
            TokenType::Minus => match operand.serenity_type.as_ref() {
                ValueType::Integer => ExprResultInner::new(
                    self.builder
                        .build_int_neg(operand.value.into_int_value(), "negtmp")
                        .context("Build negation")
                        .map(BasicValueEnum::IntValue)?,
                    ValueType::UInteger.intern(),
                ),
                ValueType::Float => ExprResultInner::new(
                    self.builder
                        .build_float_neg(operand.value.into_float_value(), "negtmp")
                        .context("Build negation")
                        .map(BasicValueEnum::FloatValue)?,
                    ValueType::Float.intern(),
                ),
                ValueType::UInteger => ExprResultInner::new(
                    self.builder
                        .build_int_neg(operand.value.into_int_value(), "negtmp")
                        .context("Build negation")
                        .map(BasicValueEnum::IntValue)?,
                    ValueType::UInteger.intern(),
                ),
                _ => {
                    return Err(anyhow::anyhow!(
                        "Invalid unary expression {:?} {:?}",
                        expression.operator,
                        operand
                    ))
                }
            },
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid unary expression {:?} {:?}",
                    expression.operator,
                    operand
                ))
            }
        };
        Ok(expr)
    }

    fn visit_deref_expression(&self, expression: &DerefExpression) -> ExprResult<'ctx> {
        let expr = expression
            .operand
            .accept(self)
            .context("Deref expression operand")?
            .rvalue(self)
            .context("Rvalue")?;
        let ValueType::Pointer(t, _) = expr.serenity_type.as_ref() else {
            return Err(anyhow::anyhow!("Invalid deref expression"));
        };
        Ok(ExprResultInner::new(expr.value, ValueType::LValue(*t, false).intern()))
    }

    fn visit_ref_expression(&self, expression: &RefExpression) -> ExprResult<'ctx> {
        let expr = expression
            .operand
            .accept(self)
            .context("Ref expression operand")?;
        let ValueType::LValue(t, _) = expr.serenity_type.as_ref() else {
            return Err(anyhow::anyhow!("Invalid ref expression"));
        };
        return Ok(ExprResultInner::new(
            expr.value,
            ValueType::Pointer(*t, true).intern(),
        ));
    }

    fn visit_index_expression(&self, _expression: &IndexExpression) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_binary_expression(&self, expression: &BinaryExpression) -> ExprResult<'ctx> {
        let lhs = expression
            .left
            .accept(self)
            .context("Left hand side of binary expression")?
            .rvalue(self)
            .context("Rvalue")?;
        let rhs = expression
            .right
            .accept(self)
            .context("Right hand side of binary expression")?
            .rvalue(self)
            .context("Rvalue")?;

        let expr = match (
            expression.operator,
            lhs.serenity_type.as_ref(),
            rhs.serenity_type.as_ref(),
        ) {
            (TokenType::Plus, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_add(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "addtmp",
                    )
                    .context("Build addition")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Integer.intern(),
            ),
            (TokenType::Plus, ValueType::UInteger, ValueType::UInteger) => ExprResultInner::new(
                self.builder
                    .build_int_add(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "addtmp",
                    )
                    .context("Build addition")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::UInteger.intern(),
            ),
            (TokenType::Plus, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_add(
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "addtmp",
                    )
                    .context("Build addition")
                    .map(BasicValueEnum::FloatValue)?,
                ValueType::Float.intern(),
            ),
            (TokenType::Minus, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_sub(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "subtmp",
                    )
                    .context("Build subtraction")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Integer.intern(),
            ),
            (TokenType::Minus, ValueType::UInteger, ValueType::UInteger) => ExprResultInner::new(
                self.builder
                    .build_int_sub(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "subtmp",
                    )
                    .context("Build subtraction")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::UInteger.intern(),
            ),
            (TokenType::Minus, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_sub(
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "subtmp",
                    )
                    .context("Build subtraction")
                    .map(BasicValueEnum::FloatValue)?,
                ValueType::Float.intern(),
            ),
            (TokenType::Star, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_mul(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "multmp",
                    )
                    .context("Build multiplication")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Integer.intern(),
            ),
            (TokenType::Star, ValueType::UInteger, ValueType::UInteger) => ExprResultInner::new(
                self.builder
                    .build_int_mul(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "multmp",
                    )
                    .context("Build multiplication")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::UInteger.intern(),
            ),
            (TokenType::Star, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_mul(
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "multmp",
                    )
                    .context("Build multiplication")
                    .map(BasicValueEnum::FloatValue)?,
                ValueType::Float.intern(),
            ),
            (TokenType::Slash, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_signed_div(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "divtmp",
                    )
                    .context("Build division")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Integer.intern(),
            ),
            (TokenType::Slash, ValueType::UInteger, ValueType::UInteger) => ExprResultInner::new(
                self.builder
                    .build_int_unsigned_div(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "divtmp",
                    )
                    .context("Build division")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::UInteger.intern(),
            ),
            (TokenType::Slash, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_div(
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "divtmp",
                    )
                    .context("Build division")
                    .map(BasicValueEnum::FloatValue)?,
                ValueType::Float.intern(),
            ),
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

    fn visit_ternary_expression(&self, _expression: &TernaryExpression) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_variable_expression(&self, expression: &VariableExpression) -> ExprResult<'ctx> {
        let name = &expression.token.lexeme;
        let (ptr, tipe) = self.get_variable(name)?;
        Ok(ExprResultInner::new(
            ptr.as_basic_value_enum(),
            ValueType::LValue(tipe, false).intern(),
        ))
    }

    fn visit_assign_expression(&self, _expression: &AssignExpression) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_logical_expression(&self, _expression: &LogicalExpression) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_call_expression(&self, expression: &CallExpression) -> ExprResult<'ctx> {
        println!("module: {}", self.module.print_to_string().to_string());
        let callee = expression
            .callee
            .accept(self)
            .context("Evaluate Calee")?
            .rvalue(self)
            .context("Rvalue Calee")?;
        let args = expression
            .arguments
            .iter()
            .map(|arg| {
                arg.accept(self)
                    .context("Argument")?
                    .rvalue(self)
                    .context("Rvalue")
            })
            .collect::<Result<Vec<_>>>()?;
        let arg_values = args.iter().map(|a| a.value).collect::<Vec<_>>();
        let arg_types = args.iter().map(|a| a.serenity_type).collect::<Vec<_>>();
        let fn_type = callee.serenity_type.as_ref();

        let ValueType::Closure(_args, _uvals, r) = fn_type else {
            return Err(anyhow::anyhow!(
                "Invalid callee serenity type got {:?}",
                fn_type
            ));
        };

        // for upval in uvals.iter() {
        //     let upval_ptr = self.builder.build_struct_gep(callee.value.into_pointer_value(), upval, "upval");
        //     let upval_val = self.builder.build_load(upval_ptr, "upval_val");
        //     arg_values.push(upval_val);
        // }

        if !callee.value.is_struct_value() {
            return Err(anyhow::anyhow!(
                "Invalid callee llvm type got {:?}",
                callee.value
            ));
        }

        let fnptr = self
            .builder
            .build_extract_value(
                callee.value.into_struct_value(),
                callee.value.into_struct_value().count_fields() - 1_u32,
                "fnptr",
            )
            .context("Extract function pointer")?;

        let arg_type_meta = arg_types
            .iter()
            .map(|t| BasicMetadataTypeEnum::from(t.deref().clone().llvm(self.context)))
            .collect::<Vec<_>>();
        let arg_value_meta = arg_values
            .iter()
            .map(|v| BasicMetadataValueEnum::from(*v))
            .collect::<Vec<_>>();

        if !fnptr.is_pointer_value() {
            return Err(anyhow::anyhow!("Invalid fnptr llvm type got {:?}", fnptr));
        }

        let call = self.builder.build_indirect_call(
            r.deref()
                .clone()
                .llvm(self.context)
                .fn_type(arg_type_meta.as_slice(), false),
            fnptr.into_pointer_value(),
            arg_value_meta.as_slice(),
            "calltmp",
        )?;
        Ok(ExprResultInner::new(
            call.try_as_basic_value()
                .left()
                .ok_or_else(|| anyhow::anyhow!("Call failed"))?,
            *r,
        ))
    }

    fn visit_dot_expression(&self, _expression: &DotExpression) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_function_expression(&self, expression: &FunctionExpression) -> ExprResult<'ctx> {
        let fn_expr = self.function(expression.prototype.clone(), Some(expression.body.clone()))?;
        Ok(ExprResultInner::new(
            fn_expr.as_basic_value_enum(),
            self.function_type_serenity(&expression.prototype),
        ))
    }

    fn visit_cast_expression(&self, _expression: &CastExpression) -> ExprResult<'ctx> {
        todo!()
    }

    fn visit_struct_initializer_expression(
        &self,
        _expression: &StructInitializerExpression,
    ) -> ExprResult<'ctx> {
        todo!()
    }
}

impl<'a, 'ctx> LLVMFunctionCompiler<'a, 'ctx> {
    fn function_type_serenity(&self, prototype: &Prototype) -> UValueType {
        ValueType::Closure(
            prototype
                .params
                .iter()
                .map(|(_s, t, _m)| (*t))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype
                .captures
                .iter()
                .map(|capture| self.get_variable(capture).unwrap().1)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype.return_type,
        )
        .intern()
    }
    fn function_type_llvm(&self, prototype: &Prototype) -> StructType<'ctx> {
        self.context.struct_type(
            [
                prototype
                    .captures
                    .iter()
                    .map(|capture| {
                        self.get_variable(capture)
                            .unwrap()
                            .1
                            .deref()
                            .clone()
                            .llvm(self.context)
                    })
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
        )
    }

    fn function(
        &self,
        prototype: Prototype,
        body: Option<Vec<ASTNode>>,
    ) -> Result<StructValue<'ctx>> {
        let args = &prototype.params;

        let fn_type = (*prototype.return_type)
            .clone()
            .llvm(self.context)
            .fn_type(
                args.iter()
                    .map(|(_s, t, _m)| {
                        BasicMetadataTypeEnum::from(t.deref().clone().llvm(self.context))
                    })
                    .collect::<Vec<_>>()
                    .as_slice(),
                false,
            );

        let fn_value = self
            .module
            .add_function(&format!("{}_fn", prototype.name), fn_type, None);

        let closure_type = self.function_type_llvm(&prototype);

        let mut struct_init = prototype
            .captures
            .iter()
            .map(|capture| self.get_variable(capture).unwrap().0.as_basic_value_enum())
            .collect::<Vec<_>>();
        struct_init.push(
            fn_value
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum(),
        );
        let struct_init = closure_type.const_named_struct(struct_init.as_slice());

        let cur_block = self.builder.get_insert_block();
        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);
        self.variables.begin_scope();

        for (i, (s, t, _m)) in args.iter().enumerate() {
            let arg_type = t.deref().clone().llvm(self.context);
            let arg_v = fn_value
                .get_nth_param(i as u32)
                .context("Function parameter")?;
            let arg = self.builder.build_alloca(arg_type, s)?;
            self.builder.build_store(arg, arg_v)?;
            self.set_variable(s, (arg, (*t)));
        }

        if let Some(body) = &body {
            let compiler = self.with_function(&fn_value);
            for statement in body {
                statement.accept(&compiler).context("Function body")?;
            }
        }

        self.variables.end_scope();
        if cur_block.is_some() {
            self.builder.position_at_end(cur_block.unwrap());
        }

        Ok(struct_init)
    }
}

impl<'a, 'ctx> DeclarationVisitor<Result<()>> for LLVMFunctionCompiler<'a, 'ctx> {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> Result<()> {
        if self.function.is_none() {
            let var = self.module.add_global(
                (*declaration.tipe.unwrap()).clone().llvm(self.context),
                Some(AddressSpace::default()),
                &declaration.name,
            );
            let mut vt = None;
            if let Some(initializer) = &declaration.initializer {
                let value = initializer.accept(self)?.rvalue(self)?;
                var.set_initializer(&value.value);
                vt = Some(value.serenity_type);
            }
            let var_type = vt.unwrap_or_else(|| declaration.tipe.unwrap());
            self.set_variable(
                &declaration.name,
                (var.as_pointer_value(), var_type),
            );
            println!("var {}: {:?}", declaration.name, var_type);
            Ok(())
        } else {
            let function = self.function.unwrap();

            let entry = function
                .get_first_basic_block()
                .unwrap_or_else(|| self.context.append_basic_block(*function, "entry"));
            self.builder.position_at_end(entry);

            let mut init_expr_r = None;
            if let Some(initializer) = &declaration.initializer {
                init_expr_r = Some(initializer.accept(self)?.rvalue(self)?);
            }

            let var_type = init_expr_r
                .clone()
                .map(|e| e.serenity_type)
                .unwrap_or_else(|| declaration.tipe.unwrap());

            let llvm_type = var_type.deref().clone().llvm(self.context);

            let var = self
                .builder
                .build_alloca(llvm_type, &declaration.name)
                .context("Variable allocation")?;

            if let Some(initializer) = init_expr_r {
                self.builder.build_store(var, initializer.value)?;
            }

            self.set_variable(&declaration.name, (var, var_type));

            println!("var {}: {:?}", declaration.name, var_type);
            Ok(())
        }
    }

    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> Result<()> {
        let struct_init = self.function(declaration.prototype.clone(), declaration.body.clone())?;
        let closure_type = self.function_type_llvm(&declaration.prototype);

        let closure = if self.function.is_none() {
            self.module
                .add_global(
                    closure_type,
                    Some(AddressSpace::default()),
                    &declaration.prototype.name,
                )
                .as_pointer_value()
        } else {
            self.builder
                .build_alloca(closure_type, &declaration.prototype.name)?
        };

        self.set_variable(
            &declaration.prototype.name,
            (
                closure,
                ValueType::Closure([].into(), [].into(), ValueType::Integer.intern()).intern(),
            ),
        );

        if self.function.is_none() {
            let global_closure = self
                .module
                .get_global(&declaration.prototype.name)
                .context("Global closure")?;
            global_closure.set_initializer(&struct_init);
        } else {
            self.builder
                .build_store(closure, struct_init)
                .context("Closure store")?;
        }

        Ok(())
    }

    fn visit_array_declaration(&self, _declaration: &ArrayDeclaration) -> Result<()> {
        todo!()
    }
}

impl<'a, 'ctx> StatementVisitor<Result<()>> for LLVMFunctionCompiler<'a, 'ctx> {
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
            let v = value.accept(self)?.rvalue(self)?.value;
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
    use crate::parser::Parser;

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

        let ctx = Context::create();
        let c = LLVMCompiler::new(&ctx);
        let r = c.compile(&anon_ast);
        assert!(r.is_ok(), "{:#}", r.unwrap_err());
        println!("---\n{}---\n", c.module.print_to_string().to_string());

        let jit = c
            .module
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

        let ctx = Context::create();
        let c = LLVMCompiler::new(&ctx);
        let r = c.compile(&anon_ast);
        assert!(r.is_ok(), "{:#}", r.unwrap_err());
        println!("---\n{}---\n", c.module.print_to_string().to_string());

        assert_snapshot!(c.module.print_to_string().to_string());
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

        let ctx = Context::create();
        let c = LLVMCompiler::new(&ctx);
        let r = c.compile(&anon_ast);
        assert!(r.is_ok(), "{:#}", r.unwrap_err());
        println!("---\n{}---\n", c.module.print_to_string().to_string());

        assert_snapshot!(c.module.print_to_string().to_string());
    }

    #[test_case(r##"
        fn main() -> int {
            let x: int = 1;
            return x;
        }
        "##, 1; "var_expr")]
    #[test_case(r##"
        fn main() -> int {
            return 1;
        }
        "##, 1; "literal_expr")]
    #[test_case(r##"
        fn main() -> int {
            return 1 + 1;
        }
        "##, 2; "addition_expr")]
    #[test_case(r##"
        fn main() -> int {
            return 1 - 1;
        }
        "##, 0; "subtraction_expr")]
    #[test_case(r##"
        fn main() -> int {
            var x: int = 1;
            let p = &x;
            return *p;
        }
        "##, 1; "ref and deref")]
    #[test_case(r##"
        fn test() -> int {
            return -1;
        }
        fn main() -> int {
            return test();
        }
        "##, -1; "call")]
    #[test_case(r##"
        fn test(x: int) -> int {
            return x;
        }
        fn main() -> int {
            return test(2);
        }
        "##, 2; "call_args")]
    #[test_case(r##"
        fn test(x: int, y: int) -> int {
            return x + y;
        }
        fn main() -> int {
            return test(1, 2);
        }
        "##, 3; "call_args_2")]
    #[test_case(r##"
        fn test(x: int, y: int) -> int {
            return x + y;
        }
        fn main() -> int {
            let x: int = 1;
            return test(x, 2);
        }
        "##, 3; "call_args_var")]
    #[test_case(r##"
        fn main() -> int {
            fn test(x: int, y: int) -> int {
                return x + y;
            }
            return test(1, 2);
        }
        "##, 3; "local_fn")]
    #[test_case(r##"
        fn main() -> int {
            let test = lambda(x: int, y: int) -> int {
                return x + y;
            };
            let x: int = 1;
            return test(x, 2);
        }
        "##, 3; "anon_fn")]
    fn test_program_integer_return(prog: &str, expected: i64) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into()).unwrap();

        let ctx = Context::create();
        let c = LLVMCompiler::new(&ctx);
        for n in ast.ast {
            let r = c.compile(&n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }

        println!("---\n{}---\n", c.module.print_to_string().to_string());

        let jit = c
            .module.create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        unsafe {
            let func = jit
                .get_function::<unsafe extern "C" fn() -> i64>("main_fn")
                .unwrap();
            assert_eq!(func.call(), expected);
        };
    }
}
