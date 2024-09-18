use crate::{
    compiler::ffi_funcs::FfiFunc,
    lexer::{Token, TokenType},
    prelude::*,
};
use anyhow::{Context as _, Result};
use core::str;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, FunctionType, StructType},
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
    path::Path,
};
use variables::Variables;

mod prototypes;
mod variables;

pub struct LLVMCompiler<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    variables: Variables<'ctx>,
    types: HashMap<SharedString, CustomStruct>,
}

impl<'ctx> LLVMCompiler<'ctx> {
    pub fn compile(&self, ast: &ASTNode) -> Result<()> {
        let fc = LLVMFunctionCompiler::new(
            self.context,
            &self.module,
            &self.builder,
            &self.variables,
            &self.types,
        );
        fc.compile(ast).context("LLVM compilation")
    }

    pub fn module(self) -> Module<'ctx> {
        self.module
    }

    fn register_ffi_function(&self, ffi: &FfiFunc) -> Result<()> {
        let FfiFunc {
            name,
            ret,
            args,
            va,
        } = ffi;
        let fnvalue = self.module.add_function(
            name,
            ret.llvm(self.context)?.fn_type(
                args.iter()
                    .map(|t| anyhow::Ok(t.llvm(self.context)?.as_basic_type_enum()))
                    .map(|m| Ok(BasicMetadataTypeEnum::from(m?)))
                    .collect::<Result<Vec<_>>>()?
                    .as_slice(),
                *va,
            ),
            Some(Linkage::External),
        );
        self.variables.set_variable(
            name,
            (
                fnvalue.as_global_value().as_pointer_value(),
                ValueType::ExternalFn(*ret, (*name).into()).intern(),
            ),
        );
        Ok(())
    }

    pub fn new(
        context: &'ctx Context,
        custom_structs: HashMap<SharedString, CustomStruct>,
        ffi_functions: &[FfiFunc],
    ) -> Self {
        let module = context.create_module("main");
        let variables = Variables::new();

        inkwell::support::load_library_permanently(Path::new("/usr/lib/libSystem.dylib")).unwrap();
        inkwell::support::load_visible_symbols();

        let builder = context.create_builder();
        let c = LLVMCompiler {
            context,
            builder,
            module,
            variables,
            types: custom_structs,
        };
        ffi_functions
            .iter()
            .for_each(|f| c.register_ffi_function(f).unwrap());

        c
    }
}

struct LLVMFunctionCompiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    variables: &'a Variables<'ctx>,
    types: &'a HashMap<SharedString, CustomStruct>,

    function: Option<&'a FunctionValue<'ctx>>,
    break_continue_contexts: RefCell<VecDeque<(BasicBlock<'ctx>, BasicBlock<'ctx>)>>,
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
        types: &'a HashMap<SharedString, CustomStruct>,
    ) -> Self {
        LLVMFunctionCompiler {
            context,
            builder,
            module,
            variables,
            types,
            function: None,
            break_continue_contexts: VecDeque::new().into(),
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
            types: self.types,
            function: Some(function),
            break_continue_contexts: VecDeque::new().into(),
        }
    }

    fn get_variable(&self, name: &str) -> Result<(PointerValue<'ctx>, UValueType)> {
        self.variables.get_variable(name).map(|(ptr, t)| {
            let t = t.substitute();
            (ptr, t)
        })
    }

    fn set_variable(&self, name: &str, value: (PointerValue<'ctx>, UValueType)) {
        self.variables.set_variable(name, value);
    }

    fn make_alloca(&self, serenity_type: UValueType, name: &str) -> Result<PointerValue<'ctx>> {
        let serenity_type = serenity_type.substitute();
        if !serenity_type.has_size() {
            if matches!(
                serenity_type.as_ref(),
                ValueType::GenericParam(_) | ValueType::TypeVar(_)
            ) {
                return Err(anyhow::anyhow!(
                    "Cannot allocate variable of unknown type {:?}",
                    serenity_type
                ));
            }
            return Err(anyhow::anyhow!("Cannot allocate variable of unsized type {0:?}, consider adding an element of indirection as in *{0:?}", serenity_type));
        }

        // save the current block
        let current_block = self.builder.get_insert_block().unwrap();
        let first_block = self.function.unwrap().get_first_basic_block().unwrap();

        // Place the alloca at the beginning of the function
        if let Some(li) = first_block.get_first_instruction() {
            self.builder.position_before(&li);
        } else {
            self.builder.position_at_end(first_block);
        }

        let llvm_type = serenity_type.llvm(self.context)?;
        let var = self
            .builder
            .build_alloca(llvm_type, name)
            .context("Variable allocation")?;

        // return to the current block
        self.builder.position_at_end(current_block);

        Ok(var)
    }
}

impl NodeVisitor<Result<()>> for LLVMFunctionCompiler<'_, '_> {
    fn visit_statement(&self, statement: &Statement) -> Result<()> {
        statement.accept(self)
    }

    fn visit_expression(&self, expression: &Expression) -> Result<()> {
        expression.accept(self).map(|_| ())
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
        Ok(match self.serenity_type.as_ref() {
            ValueType::LValue(ref t, _) => match t.as_ref() {
                ValueType::Array(a, _) => {
                    ExprResultInner::new(
                        self.value,
                        ValueType::Pointer(*a, false).intern(),
                    )
                }
                _ => {
                    let ptr = self.value.into_pointer_value();
                    let v = compiler
                        .builder
                        .build_load(t.llvm(compiler.context)?, ptr, "rval")
                        .context("Rvalue")?;
                    ExprResultInner::new(v, t.substitute())
                }
            },
            _ => self,
        })
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
                ValueType::Integer.intern(),
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
        })
    }

    fn visit_string_literal_expression(
        &self,
        expression: &StringLiteralExpression,
    ) -> ExprResult<'ctx> {
        let s = expression.value.clone();
        let llvm_string_ptr = self.builder.build_global_string_ptr(&s, "str")?;

        Ok(ExprResultInner::new(
            llvm_string_ptr.as_basic_value_enum(),
            ValueType::Pointer(ValueType::Char.intern(), false).intern(),
        ))
    }

    fn visit_unary_expression(&self, expression: &UnaryExpression) -> ExprResult<'ctx> {
        let operand = expression
            .operand
            .accept(self)
            .context("Unary expression operand")?
            .rvalue(self)
            .context("Rvalue")?;
        let expr = match expression.operator {
            TokenType::Minus => match operand.serenity_type.substitute().as_ref() {
                ValueType::Integer => ExprResultInner::new(
                    self.builder
                        .build_int_neg(operand.value.into_int_value(), "negtmp")
                        .context("Build negation")
                        .map(BasicValueEnum::IntValue)?,
                    ValueType::Integer.intern(),
                ),
                ValueType::Float => ExprResultInner::new(
                    self.builder
                        .build_float_neg(operand.value.into_float_value(), "negtmp")
                        .context("Build negation")
                        .map(BasicValueEnum::FloatValue)?,
                    ValueType::Float.intern(),
                ),
                _ => {
                    return Err(anyhow::anyhow!(
                        "Invalid unary expression {:?} {:?}",
                        expression.operator,
                        operand
                    ))
                }
            },
            TokenType::Bang => match operand.serenity_type.substitute().as_ref() {
                ValueType::Bool => ExprResultInner::new(
                    self.builder
                        .build_not(operand.value.into_int_value(), "nottmp")
                        .context("Build not")
                        .map(BasicValueEnum::IntValue)?,
                    ValueType::Bool.intern(),
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
        let t = expr.serenity_type.substitute();
        let ValueType::Pointer(t, _) = t.as_ref() else {
            return Err(anyhow::anyhow!("Invalid deref expression"));
        };
        Ok(ExprResultInner::new(
            expr.value,
            ValueType::LValue(*t, false).intern(),
        ))
    }

    fn visit_ref_expression(&self, expression: &RefExpression) -> ExprResult<'ctx> {
        let expr = expression
            .operand
            .accept(self)
            .context("Ref expression operand")?;
        let t = expr.serenity_type.substitute();
        let ValueType::LValue(t, _) = t.as_ref() else {
            return Err(anyhow::anyhow!("Invalid ref expression"));
        };
        return Ok(ExprResultInner::new(
            expr.value,
            ValueType::Pointer(*t, true).intern(),
        ));
    }

    fn visit_index_expression(&self, expression: &IndexExpression) -> ExprResult<'ctx> {
        let array = expression
            .array
            .accept(self)
            .context("Index expression array")?
            .rvalue(self)
            .context("Rvalue")?;
        let index = expression
            .index
            .accept(self)
            .context("Index expression index")?
            .rvalue(self)
            .context("Rvalue")?;

        let a = array.serenity_type.substitute();

        let t = match a.as_ref() {
            ValueType::Pointer(t, _i) => t,
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid index expression array type was {:?}",
                    a
                ))
            }
        };

        let ptr = unsafe {
            self.builder.build_gep(
                t.llvm(self.context)?,
                array.value.into_pointer_value(),
                &[
                    // self.context.i32_type().const_zero(), // For the pointer base
                    index.value.into_int_value(), // Index into the array
                ],
                "indexptr",
            )?
        };

        Ok(ExprResultInner::new(
            ptr.into(),
            ValueType::LValue(*t, false).intern(),
        ))
    }

    fn visit_binary_expression(&self, expression: &BinaryExpression) -> ExprResult<'ctx> {
        let lhs_ptr = expression
            .left
            .accept(self)
            .context("Left hand side of binary expression")?;
        let lhs = lhs_ptr.clone().rvalue(self).context("Rvalue")?;
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
            (TokenType::Plus, ValueType::Pointer(t1, _), ValueType::Integer) => {
                let ptr = unsafe {
                    self.builder.build_gep(
                        t1.llvm(self.context)?.array_type(0),
                        lhs.value.into_pointer_value(),
                        &[rhs.value.into_int_value()],
                        "addptr",
                    )?
                };
                ExprResultInner::new(ptr.into(), lhs.serenity_type)
            }
            (TokenType::Plus, ValueType::Array(t1, s), ValueType::Integer) if s.is_some() => {
                let ptr = unsafe {
                    self.builder.build_gep(
                        t1.llvm(self.context)?.array_type(s.unwrap() as u32),
                        lhs_ptr.value.into_pointer_value(),
                        &[
                            self.context.i16_type().const_zero(),
                            rhs.value.into_int_value(),
                        ],
                        "subptr",
                    )?
                };
                ExprResultInner::new(ptr.into(), ValueType::Pointer(*t1, false).intern())
            }
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
            (TokenType::Greater, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SGT,
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "gttmp",
                    )
                    .context("Build greater than")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),

            (TokenType::Greater, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OGT,
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "gttmp",
                    )
                    .context("Build greater than")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::GreaterEqual, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGE,
                            lhs.value.into_int_value(),
                            rhs.value.into_int_value(),
                            "getmp",
                        )
                        .context("Build greater than or equal")
                        .map(BasicValueEnum::IntValue)?,
                    ValueType::Bool.intern(),
                )
            }

            (TokenType::GreaterEqual, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OGE,
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "getmp",
                    )
                    .context("Build greater than or equal")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::Less, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SLT,
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "lttmp",
                    )
                    .context("Build less than")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),

            (TokenType::Less, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OLT,
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "lttmp",
                    )
                    .context("Build less than")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::LessEqual, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SLE,
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "letmp",
                    )
                    .context("Build less than or equal")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),

            (TokenType::LessEqual, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OLE,
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "letmp",
                    )
                    .context("Build less than or equal")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::EqualEqual, ValueType::Integer, ValueType::Integer) => {
                ExprResultInner::new(
                    self.builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            lhs.value.into_int_value(),
                            rhs.value.into_int_value(),
                            "eqtmp",
                        )
                        .context("Build equal")
                        .map(BasicValueEnum::IntValue)?,
                    ValueType::Bool.intern(),
                )
            }

            (TokenType::EqualEqual, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::OEQ,
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "eqtmp",
                    )
                    .context("Build equal")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::BangEqual, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "neqtmp",
                    )
                    .context("Build not equal")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::BangEqual, ValueType::Float, ValueType::Float) => ExprResultInner::new(
                self.builder
                    .build_float_compare(
                        inkwell::FloatPredicate::ONE,
                        lhs.value.into_float_value(),
                        rhs.value.into_float_value(),
                        "neqtmp",
                    )
                    .context("Build not equal")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Bool.intern(),
            ),
            (TokenType::Percent, ValueType::Integer, ValueType::Integer) => ExprResultInner::new(
                self.builder
                    .build_int_signed_rem(
                        lhs.value.into_int_value(),
                        rhs.value.into_int_value(),
                        "remtmp",
                    )
                    .context("Build remainder")
                    .map(BasicValueEnum::IntValue)?,
                ValueType::Integer.intern(),
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

    fn visit_ternary_expression(&self, expression: &TernaryExpression) -> ExprResult<'ctx> {
        let cond = expression
            .condition
            .accept(self)
            .context("Ternary expression condition")?
            .rvalue(self)
            .context("Rvalue")?;
        let thenblock = self
            .context
            .append_basic_block(*self.function.unwrap(), "then");
        let elseblock = self
            .context
            .append_basic_block(*self.function.unwrap(), "else");
        let merge = self
            .context
            .append_basic_block(*self.function.unwrap(), "merge");

        self.builder
            .build_conditional_branch(cond.value.into_int_value(), thenblock, elseblock)
            .context("Build conditional branch")?;

        self.builder.position_at_end(thenblock);
        let then = expression
            .then_branch
            .accept(self)
            .context("Ternary expression then")?
            .rvalue(self)
            .context("Rvalue")?;
        let thenfrom = self
            .builder
            .get_insert_block()
            .context("Get insert block")?;
        self.builder.build_unconditional_branch(merge)?;

        self.builder.position_at_end(elseblock);
        let else_branch = expression
            .else_branch
            .accept(self)
            .context("Ternary expression else")?
            .rvalue(self)
            .context("Rvalue")?;
        let elsefrom = self
            .builder
            .get_insert_block()
            .context("Get insert block")?;
        self.builder.build_unconditional_branch(merge)?;

        self.builder.position_at_end(merge);
        let phi = self
            .builder
            .build_phi(then.value.get_type(), "iftmp")
            .context("Build phi")?;
        phi.add_incoming(&[(&then.value, thenfrom), (&else_branch.value, elsefrom)]);

        Ok(ExprResultInner::new(
            phi.as_basic_value(),
            then.serenity_type,
        ))
    }

    fn visit_variable_expression(&self, expression: &VariableExpression) -> ExprResult<'ctx> {
        let name = &expression.token.lexeme;
        let (ptr, tipe) = self.get_variable(name)?;
        Ok(ExprResultInner::new(
            ptr.as_basic_value_enum(),
            ValueType::LValue(tipe, false).intern(),
        ))
    }

    fn visit_assign_expression(&self, expression: &AssignExpression) -> ExprResult<'ctx> {
        let rhs = expression
            .value
            .accept(self)
            .with_context(|| {
                format!(
                    "Assign expression rhs {:?}",
                    ASTNode::Expression(*expression.value.clone())
                )
            })?
            .rvalue(self)
            .context("Rvalue")?;
        let lhs = expression
            .variable
            .accept(self)
            .context("Assign expression lhs")?;
        let t = lhs.serenity_type.substitute();
        let ValueType::LValue(t, _) = t.as_ref() else {
            return Err(anyhow::anyhow!("Invalid assign expression lhs"));
        };
        self.builder
            .build_store(lhs.value.into_pointer_value(), rhs.value)?;
        Ok(ExprResultInner::new(rhs.value, *t))
    }

    fn visit_logical_expression(&self, expression: &LogicalExpression) -> ExprResult<'ctx> {
        let rhsblock = self
            .context
            .append_basic_block(*self.function.unwrap(), "rhs");
        let merge = self
            .context
            .append_basic_block(*self.function.unwrap(), "merge");

        let lhs = expression
            .left
            .accept(self)
            .context("Logical expression lhs")?
            .rvalue(self)
            .context("Rvalue")?;
        let lhsfrom = self
            .builder
            .get_insert_block()
            .context("Get insert block")?;
        if expression.operator == TokenType::Or {
            self.builder
                .build_conditional_branch(lhs.value.into_int_value(), merge, rhsblock)
                .context("Build conditional branch")?;
        } else {
            self.builder
                .build_conditional_branch(lhs.value.into_int_value(), rhsblock, merge)
                .context("Build conditional branch")?;
        }

        self.builder.position_at_end(rhsblock);
        let rhs = expression
            .right
            .accept(self)
            .context("Logical expression rhs")?
            .rvalue(self)
            .context("Rvalue")?;
        let rhsfrom = self
            .builder
            .get_insert_block()
            .context("Get insert block")?;
        self.builder.build_unconditional_branch(merge)?;
        self.builder.position_at_end(merge);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "logicaltmp")?;
        phi.add_incoming(&[(&lhs.value, lhsfrom), (&rhs.value, rhsfrom)]);

        Ok(ExprResultInner::new(
            phi.as_basic_value(),
            ValueType::Bool.intern(),
        ))
    }

    fn visit_call_expression(&self, expression: &CallExpression) -> ExprResult<'ctx> {
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

        let fn_type = callee.serenity_type.substitute();

        if let ValueType::ExternalFn(r, name) = fn_type.as_ref() {
            let arg_value_meta = arg_values
                .iter()
                .map(|v| BasicMetadataValueEnum::from(*v))
                .collect::<Vec<_>>();

            let call = self.builder.build_call(
                self.module.get_function(name).context("Malloc function")?,
                arg_value_meta.as_slice(),
                "calltmp",
            )?;

            return Ok(ExprResultInner::new(
                call.try_as_basic_value()
                    .left()
                    .unwrap_or_else(|| self.context.bool_type().const_zero().as_basic_value_enum()),
                *r,
            ));
        }

        let ValueType::Closure(_, uvals, r) = fn_type.as_ref() else {
            return Err(anyhow::anyhow!(
                "Invalid callee serenity type got {:?} {:?}",
                fn_type,
                ASTNode::Expression(Expression::Call(expression.clone()))
            ));
        };

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
                uvals.len() as u32,
                "fnptr",
            )
            .context(format!(
                "Extract function pointer {} on {:?} {:?}",
                uvals.len(),
                callee.value,
                fn_type
            ))?;

        let mut upval_vals = vec![];
        let mut upval_types = vec![];
        for (i, upval) in uvals.iter().enumerate() {
            let upval_val = self.builder.build_extract_value(
                callee.value.into_struct_value(),
                i as u32,
                "upval",
            )?;
            upval_vals.push(upval_val);
            upval_types.push(*upval);
        }
        let arg_types = upval_types
            .iter()
            .chain(arg_types.iter())
            .collect::<Vec<_>>();
        let arg_values = upval_vals
            .iter()
            .chain(arg_values.iter())
            .collect::<Vec<_>>();

        let arg_type_meta = arg_types
            .iter()
            .map(|t| Ok(BasicMetadataTypeEnum::from(t.llvm(self.context)?)))
            .collect::<Result<Vec<_>>>()?;
        let arg_value_meta = arg_values
            .iter()
            .map(|v| BasicMetadataValueEnum::from(**v))
            .collect::<Vec<_>>();

        if !fnptr.is_pointer_value() {
            return Err(anyhow::anyhow!("Invalid fnptr llvm type got {:?}", fnptr));
        }

        let call = self.builder.build_indirect_call(
            r.deref()
                .clone()
                .llvm(self.context)?
                .fn_type(arg_type_meta.as_slice(), false),
            fnptr.into_pointer_value(),
            arg_value_meta.as_slice(),
            "calltmp",
        )?;
        let r_ptr = self.make_alloca(*r, "calltmp")?;
        self.builder
            .build_store(r_ptr, call.try_as_basic_value().left().unwrap())?;

        Ok(ExprResultInner::new(
            r_ptr.as_basic_value_enum(),
            ValueType::LValue(*r, false).intern(),
        ))
    }

    fn visit_dot_expression(&self, expression: &DotExpression) -> ExprResult<'ctx> {
        let lhs = expression
            .object
            .accept(self)
            .context("Dot expression lhs")?;
        let ValueType::LValue(st, _) = lhs.serenity_type.as_ref() else {
            return Err(anyhow::anyhow!("Invalid dot expression lhs"));
        };
        let d = st
            .substitute()
            .fill_self_struct(self.types.clone())
            .substitute();
        let ValueType::Struct(t) = d.as_ref() else {
            return Err(anyhow::anyhow!(
                "Invalid dot expression lhs got {:?} on {:?}",
                st,
                expression.line_no
            ));
        };
        let struct_type = t.llvm(self.context)?;
        let ofield = t
            .fields
            .borrow()
            .iter()
            .enumerate()
            .find(|(_i, (name, _t))| *name == &expression.field)
            .map(|(i, _)| i);

        if ofield.is_none() {
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

        let field = ofield.unwrap();

        let field_ptr = self.builder.build_struct_gep(
            struct_type,
            lhs.value.into_pointer_value(),
            field as u32,
            "field",
        )?;
        let x = Ok(ExprResultInner::new(
            field_ptr.as_basic_value_enum(),
            ValueType::LValue(t.fields.borrow()[&expression.field].value, false).intern(),
        ));
        x
    }

    fn visit_function_expression(&self, expression: &FunctionExpression) -> ExprResult<'ctx> {
        let fn_expr = self.function(expression.prototype.clone(), expression.body.clone())?;

        Ok(ExprResultInner::new(
            fn_expr.as_basic_value_enum(),
            self.function_type_serenity(&expression.prototype),
        ))
    }

    fn visit_cast_expression(&self, expression: &CastExpression) -> ExprResult<'ctx> {
        let expr = expression
            .expression
            .accept(self)
            .context("Cast expression operand")?
            .rvalue(self)
            .context("Rvalue")?;
        match (expr.serenity_type.as_ref(), expression.target_type.as_ref()) {
            (ValueType::Float, ValueType::Float) => todo!(),
            (ValueType::Float, ValueType::Integer) => {
                let v = self.builder.build_float_to_signed_int(
                    expr.value.into_float_value(),
                    self.context.i64_type(),
                    "fptoint",
                )?;
                Ok(ExprResultInner::new(v.into(), ValueType::Integer.intern()))
            }
            (ValueType::Integer, ValueType::Float) => {
                let v = self.builder.build_signed_int_to_float(
                    expr.value.into_int_value(),
                    self.context.f64_type(),
                    "inttofp",
                )?;
                Ok(ExprResultInner::new(v.into(), ValueType::Float.intern()))
            }
            (ValueType::Integer, ValueType::Integer) => {
                let v = self.builder.build_int_cast(
                    expr.value.into_int_value(),
                    self.context.i64_type(),
                    "intcast",
                )?;
                Ok(ExprResultInner::new(v.into(), ValueType::Integer.intern()))
            }
            (ValueType::Pointer(_, _), ValueType::Pointer(_, _)) => {
                Ok(ExprResultInner::new(expr.value, expression.target_type))
            }
            (_, _) => Err(anyhow::anyhow!(
                "Invalid cast expression {:?} {:?}",
                expr.serenity_type,
                expression.target_type
            )),
        }
    }

    fn visit_struct_initializer_expression(
        &self,
        expression: &StructInitializerExpression,
    ) -> ExprResult<'ctx> {
        let struct_value: StructValue<'ctx> = if self.function.is_none() {
            expression
                .struct_type
                .llvm(self.context)?
                .const_named_struct(
                    expression
                        .fields
                        .iter()
                        .map(|(_name, expr)| {
                            expr.accept(self)
                                .context("Struct initializer field")?
                                .rvalue(self)
                                .context("Rvalue")
                                .map(|v| v.value)
                        })
                        .collect::<Result<Vec<_>>>()?
                        .as_slice(),
                )
        } else {
            let struct_value_ptr =
                self.make_alloca(expression.struct_type.to_value_type(), "struct_init")?;
            for (i, (name, _)) in expression.struct_type.fields.borrow().iter().enumerate() {
                let field_ptr = self.builder.build_struct_gep(
                    expression.struct_type.llvm(self.context)?,
                    struct_value_ptr,
                    i as u32,
                    "field",
                )?;
                let expr = expression.fields.get(name).unwrap();
                let field_val = expr
                    .accept(self)
                    .context("Struct initializer field")?
                    .rvalue(self)
                    .context("Rvalue")?;
                self.builder.build_store(field_ptr, field_val.value)?;
            }
            let struct_value = self.builder.build_load(
                expression.struct_type.llvm(self.context)?,
                struct_value_ptr,
                "struct_init",
            )?;

            struct_value.into_struct_value()
        };
        Ok(ExprResultInner::new(
            struct_value.as_basic_value_enum(),
            ValueType::Struct(expression.struct_type.clone()).intern(),
        ))
    }

    fn visit_sizeof_expression(&self, expression: &SizeofExpression) -> ExprResult<'ctx> {
        let literal = expression
            .tipe
            .llvm(self.context)?
            .size_of()
            .context("Sizeof expression")?;

        Ok(ExprResultInner::new(
            literal.as_basic_value_enum(),
            ValueType::Integer.intern(),
        ))
    }
}

impl<'a, 'ctx> LLVMFunctionCompiler<'a, 'ctx> {
    fn function(
        &self,
        prototype: Prototype,
        body: Option<Vec<ASTNode>>,
    ) -> Result<StructValue<'ctx>> {
        // the type of the function itself: fn(captures ..., args ... ) -> return_type
        let fn_type = self.function_prototype_llvm(&prototype)?;

        // if the function already exists (e.g. a prototype was
        // declared before the function definition) we use that
        let fn_value = self
            .module
            .get_function(&format!("{}_fn", prototype.name))
            .unwrap_or_else(|| {
                self.module
                    .add_function(&format!("{}_fn", prototype.name), fn_type, None)
            });

        // the type of the closure: struct { captures ..., fn_ptr }
        let closure_type = self.closure_type_llvm(&prototype)?;

        let mut struct_init = prototype
            .captures
            .iter()
            .map(|capture| {
                self.builder
                    .build_load(
                        self.get_variable(capture).unwrap().1.llvm(self.context)?,
                        self.get_variable(capture).unwrap().0,
                        capture,
                    )
                    .context("Capture load")
            })
            .collect::<Result<Vec<_>>>()?;
        struct_init.push(
            fn_value
                .as_global_value()
                .as_pointer_value()
                .as_basic_value_enum(),
        );

        let struct_value = if self.function.is_none() {
            closure_type.const_named_struct(struct_init.as_slice())
        } else {
            let struct_value_ptr =
                self.make_alloca(self.function_type_serenity(&prototype), "closure")?;
            for (i, v) in struct_init.iter().enumerate() {
                let ptr = self.builder.build_struct_gep(
                    closure_type,
                    struct_value_ptr,
                    i as u32,
                    "upval",
                )?;
                self.builder.build_store(ptr, *v)?;
            }
            self.builder
                .build_load(closure_type, struct_value_ptr, "closure")?
                .into_struct_value()
        };
        if let Some(body) = &body {
            let cur_block = self.builder.get_insert_block();
            let entry = self.context.append_basic_block(fn_value, "entry");
            self.builder.position_at_end(entry);
            self.variables.begin_scope();
            let fncompiler = self.with_function(&fn_value);

            for (i, s) in prototype.captures.iter().enumerate() {
                let arg_type_serenity = self.get_variable(s).unwrap().1;
                let arg_v = fn_value
                    .get_nth_param(i as u32)
                    .context("Upvalue parameter")?;
                let arg = fncompiler.make_alloca(arg_type_serenity, s)?;
                self.builder.build_store(arg, arg_v)?;
                self.set_variable(s, (arg, self.get_variable(s).unwrap().1));
            }

            for (i, &(ref s, t, _m)) in prototype.params.iter().enumerate() {
                let arg_v = fn_value
                    .get_nth_param((i + prototype.captures.len()) as u32)
                    .context("Function parameter")?;
                let arg = fncompiler.make_alloca(t.decay(), s)?;
                self.builder.build_store(arg, arg_v)?;
                self.set_variable(s, (arg, t.decay()));
            }

            for statement in body {
                statement.accept(&fncompiler).context("Function body")?;
            }
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_none()
            {
                let ret = fn_type.get_return_type().map(|t| t.const_zero());
                self.builder
                    .build_return(ret.as_ref().map(|r| r as &dyn BasicValue))?;
            }

            self.variables.end_scope();
            if let Some(b) = cur_block {
                self.builder.position_at_end(b);
            }
        }

        Ok(struct_value)
    }
}

impl<'a, 'ctx> DeclarationVisitor<Result<()>> for LLVMFunctionCompiler<'a, 'ctx> {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> Result<()> {
        if self.function.is_none() {
            let mut value = None;
            if let Some(initializer) = &declaration.initializer {
                value = Some(initializer.accept(self)?.rvalue(self)?);
            }
            let var_type = declaration.tipe;

            let var = self.module.add_global(
                var_type.llvm(self.context).context("Var decl type")?,
                Some(AddressSpace::default()),
                &declaration.name,
            );

            if let Some(value) = value {
                var.set_initializer(&value.value);
            } else {
                var.set_initializer(
                    &var_type
                        .llvm(self.context)
                        .context("Var decl initializer")?
                        .const_zero(),
                );
            }

            self.set_variable(&declaration.name, (var.as_pointer_value(), var_type));
            Ok(())
        } else {
            let mut init_expr_r = None;
            if let Some(initializer) = &declaration.initializer {
                init_expr_r = Some(initializer.accept(self)?.rvalue(self)?);
            }

            let var_type = declaration.tipe.substitute();

            let current_block = self.builder.get_insert_block().unwrap();
            if self
                .function
                .unwrap()
                .get_first_basic_block()
                .unwrap()
                .get_last_instruction()
                .is_some()
            {
                self.builder.position_before(
                    &self
                        .function
                        .unwrap()
                        .get_first_basic_block()
                        .unwrap()
                        .get_last_instruction()
                        .unwrap(),
                );
            } else {
                self.builder
                    .position_at_end(self.function.unwrap().get_first_basic_block().unwrap());
            }
            let var = self
                .make_alloca(var_type, &declaration.name)
                .context("Variable allocation")?;
            self.builder.position_at_end(current_block);

            if let Some(initializer) = init_expr_r {
                self.builder.build_store(var, initializer.value)?;
            }

            self.set_variable(&declaration.name, (var, var_type));

            Ok(())
        }
    }

    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> Result<()> {
        let struct_init = self.function(declaration.prototype.clone(), declaration.body.clone())?;
        let closure_type = self.closure_type_llvm(&declaration.prototype)?;

        let closure = if self.function.is_none() {
            self.module
                .get_global(&declaration.prototype.name)
                .map(|g| g.as_pointer_value())
                .unwrap_or_else(|| {
                    self.module
                        .add_global(
                            closure_type,
                            Some(AddressSpace::default()),
                            &declaration.prototype.name,
                        )
                        .as_pointer_value()
                })
        } else {
            self.make_alloca(
                self.function_type_serenity(&declaration.prototype),
                &declaration.prototype.name,
            )?
        };

        self.set_variable(
            &declaration.prototype.name,
            (closure, self.function_type_serenity(&declaration.prototype)),
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
}

impl<'a, 'ctx> StatementVisitor<Result<()>> for LLVMFunctionCompiler<'a, 'ctx> {
    fn visit_block_statement(&self, statement: &BlockStatement) -> Result<()> {
        self.variables.begin_scope();
        for s in &statement.statements {
            s.accept(self)?;
        }
        self.variables.end_scope();
        Ok(())
    }

    fn visit_if_statement(&self, statement: &IfStatement) -> Result<()> {
        let then_block = self
            .context
            .append_basic_block(*self.function.unwrap(), "then");
        let else_block = self
            .context
            .append_basic_block(*self.function.unwrap(), "else");
        let merge_block = self
            .context
            .append_basic_block(*self.function.unwrap(), "ifcont");

        let cond = statement
            .condition
            .accept(self)?
            .rvalue(self)?
            .value
            .into_int_value();
        self.builder
            .build_conditional_branch(cond, then_block, else_block)?;

        self.builder.position_at_end(then_block);
        statement.then_branch.accept(self)?;
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder.build_unconditional_branch(merge_block)?;
        }

        self.builder.position_at_end(else_block);

        if let Some(el) = &statement.else_branch {
            el.accept(self)?;
        }
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder.build_unconditional_branch(merge_block)?;
        }
        self.builder.position_at_end(merge_block);

        Ok(())
    }

    fn visit_while_statement(&self, statement: &WhileStatement) -> Result<()> {
        let loop_block = self.context.append_basic_block(
            *self.function.unwrap(),
            &format!("loop_{}", statement.line_no),
        );
        let body_block = self.context.append_basic_block(
            *self.function.unwrap(),
            &format!("body_{}", statement.line_no),
        );
        let merge_block = self.context.append_basic_block(
            *self.function.unwrap(),
            &format!("merge_{}", statement.line_no),
        );

        self.break_continue_contexts
            .borrow_mut()
            .push_front((merge_block, loop_block));
        self.builder.build_unconditional_branch(loop_block)?;

        self.builder.position_at_end(loop_block);
        let cond = statement
            .condition
            .accept(self)?
            .rvalue(self)?
            .value
            .into_int_value();
        self.builder
            .build_conditional_branch(cond, body_block, merge_block)?;
        self.builder.position_at_end(body_block);
        statement.body.accept(self)?;
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder.build_unconditional_branch(loop_block)?;
        }
        self.builder.position_at_end(body_block);

        if body_block.get_terminator().is_none() {
            self.builder.build_unconditional_branch(loop_block)?;
        }
        // if body_block.get_previous_basic_block().unwrap().get_terminator().is_none() {
        //     self.builder.position_at_end(body_block.get_previous_basic_block().unwrap());
        //     self.builder.build_unconditional_branch(loop_block)?;
        // }

        self.builder.position_at_end(merge_block);
        self.break_continue_contexts.borrow_mut().pop_front();

        Ok(())
    }

    fn visit_for_statement(&self, statement: &ForStatement) -> Result<()> {
        self.variables.begin_scope();
        let loop_block = self.context.append_basic_block(
            *self.function.unwrap(),
            &format!("loop_{}", statement.line_no),
        );
        let body_block = self.context.append_basic_block(
            *self.function.unwrap(),
            &format!("body_{}", statement.line_no),
        );
        let merge_block = self.context.append_basic_block(
            *self.function.unwrap(),
            &format!("merge_{}", statement.line_no),
        );

        self.break_continue_contexts
            .borrow_mut()
            .push_front((merge_block, loop_block));

        if let Some(init) = &statement.init {
            init.accept(self)?;
        }

        self.builder.build_unconditional_branch(loop_block)?;

        self.builder.position_at_end(loop_block);

        let cond = statement
            .condition
            .as_ref()
            .map(|c| {
                c.accept(self)
                    .context("For statement condition")
                    .map(|c| anyhow::Ok(c.rvalue(self)?.value.into_int_value()))
            })
            .transpose()?;
        self.builder.build_conditional_branch(
            cond.unwrap_or_else(|| Ok(self.context.bool_type().const_all_ones()))?,
            body_block,
            merge_block,
        )?;

        self.builder.position_at_end(body_block);
        statement.body.accept(self)?;
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            if let Some(update) = &statement.increment {
                update.accept(self)?;
            }
            self.builder.build_unconditional_branch(loop_block)?;
        }
        self.builder.position_at_end(merge_block);

        self.break_continue_contexts.borrow_mut().pop_front();
        self.variables.end_scope();

        Ok(())
    }

    fn visit_break_statement(&self, _statement: &BreakStatement) -> Result<()> {
        self.builder.build_unconditional_branch(
            self.break_continue_contexts
                .borrow()
                .front()
                .map(|(b, _)| *b)
                .context("Break statement")?,
        )?;
        Ok(())
    }

    fn visit_continue_statement(&self, _statement: &ContinueStatement) -> Result<()> {
        self.builder.build_unconditional_branch(
            self.break_continue_contexts
                .borrow()
                .front()
                .map(|(_, c)| *c)
                .context("Continue statement")?,
        )?;
        Ok(())
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
    use super::*;
    use crate::parser::Parser;
    use inkwell::OptimizationLevel;
    use test_case::test_case;
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
    #[test_case(r##"
        fn main() -> int {
            if (true) {
                return 1;
            }
        }
        "##, 1; "if block")]
    #[test_case(r##"
        fn main() -> int {
            if (false) {
                return 1;
            } else {
                return 2;
            }
        }
        "##, 2; "if else block")]
    #[test_case(r##"
        fn make_add(x: int) -> fn[int](int) -> int {
            fn inner[x](y: int) -> int {
                return x + y;
            }
            return inner;
        }

        fn main() -> int {
            let add = make_add(1);
            return add(2);
        }
        "##, 3; "closure")]
    #[test_case(r##"
        fn make_add(x: int) -> fn[int](int) -> int {
            fn inner[x](y: int) -> int {
                return x + y;
            }
            return inner;
        }

        fn main() -> int {
            let add = make_add(1);
            let add2 = make_add(2);
            return add(2) + add2(3);
        }
        "##, 8; "closure_2")]
    #[test_case(r##"
        fn outer(x: int) ->  fn[int](int) -> (fn[int, int](int) -> int) {
            return lambda[x](y: int) -> fn[int, int](int) -> int {
                return lambda[x, y](z: int) -> int {
                    return x + y + z;
                };
            };
        }

        fn main() -> int {
            let inner = outer(1);
            let inner2 = inner(2);
            return inner2(3);
        }
        "##, 6; "closure_deep")]
    #[test_case(r##"
        fn outer(x: int) ->  fn[int](int) -> (fn[int, int](int) -> int) {
            return lambda[x](y: int) -> fn[int, int](int) -> int {
                if (x > 0) {
                    return lambda[x, y](z: int) -> int {
                        return x + y + z;
                    };
                } else {
                    return lambda[x, y](z: int) -> int {
                        return x - y - z;
                    };
                }
            };
        }

        fn main() -> int {
            let plus = outer(1)(2);
            let minus = outer(-1)(2);
            return plus(3) + minus(3);
        }
        "##, 0; "closure_deep_2")]
    #[test_case(r##"
        type c struct {
            x: int,
            y: int,
        };

        fn main() -> int {
            let s: struct c = struct c {x: 1, y: 2};
            return s.x + s.y;
        }
        "##, 3; "structs")]
    #[test_case(r##"
        type c struct {
            x: int,
        };

        fn (p: *struct c) inc() {
            p->x = p->x + 1;
        }

        fn main() -> int {
            let s: struct c = struct c {x: 1};
            s.inc();
            return s.x;
        }
        "##, 2; "method")]
    #[test_case(r##"
        fn forward(x: int) -> int;

        fn main() -> int {
            return forward(1);
        }

        fn forward(x: int) -> int {
            return x;
        }
        "##, 1; "forward")]
    #[test_case(r##"
    type adder interface {
        add: fn(int) -> int
    };

    type c struct {
        i : int
    } implements adder;

    fn (self: *struct c) add(j: int) -> int {
        let rself = cast(self, *struct c);
        let ret = rself->i = rself->i + j;
        return ret;
    }

    fn main() -> int {
        let c_conc = struct c{i: 0};
        let c_impl = c_impl_adder(&c_conc);

        c_conc.add(2);
        c_impl.add(2);
        return c_conc.i;
    }
    "##, 4; "interface")]
    #[test_case(r##"
    fn main() -> int {
        let s: *int = malloc(4);
        *s = 1;
        return *s;
    }
    "##, 1; "malloc")]
    #[test_case(r##"
    var arr: [[int; 4]; 4];
    fn main() -> int {
        arr[3][2] = 1;
        arr[2][3] = 2;
        return arr[3][2];
    }
    "##, 1; "twodarr")]
    #[test_case(r##"
    type c struct {
        x: int,
        y: int,
    };
    fn main() -> int {
        let s: struct c = struct c {x: 1, y: 2};
        let p = &s;
        return p->x + p->y;
    }
    "##, 3; "struct_literal")]
    #[test_case(r##"
    fn main() -> int {
        return 49 % 8;
    }
    "##, 1; "modulo")]
    #[test_case(r##"
    fn main() -> int {
        let x: int = 1;
        let y: float = 2.0;
        return x + #y;
    }
    "##, 3; "casting")]
    #[test_case(r##"
    type hold_ptr struct {
        x: *int,
    };
    fn main() -> int {
        let arr: [int; 4];
        arr[1] = 1;
        let h: struct hold_ptr = struct hold_ptr {x: arr};
        return h.x[1];
    }
    "##, 1; "ptr_struct")]
    #[test_case(r##"
    type s struct {
        x: int,
    };
    fn main() -> int {
        let arr: [struct s; 4];
        arr[1].x = 1;
        return arr[1].x;
    }
    "##, 1; "struct_arr")]
    #[test_case(r##"
    type generator interface {
        next: fn() -> int,
    };

    type nat_gen struct {
        i: int,
    } implements generator;

    fun (self : * struct nat_gen) next() -> int {
        let i = self->i;
        self->i = i + 1;
        return i;
    }

    fn main() -> int {
        let gen = struct nat_gen {i: 0};
        let g = nat_gen_impl_generator(&gen);
        return g.next() + g.next();
    }
    "##, 1; "structliteral_ptr")]
    #[test_case(r##"
    //pass array as pointer
    fn sum(arr: *int, len: int) -> int {
        let sum = 0;
        let i = 0;
        while (i < len) {
            sum = sum + arr[i];
            i = i + 1;
        }
        return sum;
    }

    fn main() -> int {
        let arr: [int; 4];
        arr[0] = 1;
        arr[1] = 2;
        arr[2] = 3;
        arr[3] = 4;
        return sum(arr, 4);
    }
    "##, 10; "array_ptr")]
    #[test_case(r##"
    //pass array of struct as pointer
    type s struct {
        x: int,
    };

    fn sum(arr: *struct s, len: int) -> int {
        let sum = 0;
        let i = 0;
        while (i < len) {
            sum = sum + arr[i].x;
            i = i + 1;
        }
        return sum;
    }

    fn main() -> int {
        let arr: [struct s; 4];
        arr[0].x = 1;
        arr[1].x = 2;
        arr[2].x = 3;
        arr[3].x = 4;
        return sum(arr, 4);
    }
    "##, 10; "struct_array_ptr")]
    #[test_case(r##"
    // nested if
    fn main() -> int {
        let x = 1;
        let y = 2;
        if (x == 1) {
            if (y == 2) {
                return 1;
            }
        }
        return 0;
    }
    "##, 1; "nested_if")]
    #[test_case(r##"
    // for loop
    fn main() -> int {
        let sum = 0;
        for (let i = 0; i < 10; i = i + 1) {
            sum = sum + i;
        }
        return sum;
    }
    "##, 45; "for_loop")]
    #[test_case(r##"
    // ternary
    fn main() -> int {
        let x = 1;
        return x == 1 ? 1 : 0;
    }
    "##, 1; "ternary")]
    #[test_case(r##"
    // ternary complicated
    fn main() -> int {
        let x = 1;
        let y = 2;
        return x == 1 ? (y == 2 ? 1 : 0) : 0;
    }
    "##, 1; "ternary_complicated")]
    #[test_case(r##"
    // generic struct
    type<T> s struct {
        x: T,
    };

    fn main() -> int {
        let s: struct s<int> = struct s<int> {x: 1};
        return s.x;
    }
    "##, 1; "generic_struct")]

    fn test_program_integer_return(prog: &str, expected: i64) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into()).unwrap();

        let ctx = Context::create();
        let ffi = crate::compiler::ffi_funcs::ffi_funcs();
        let t = crate::compiler::Typechecker::new(ast.custom_structs.clone(), ffi.as_ref());
        for n in ast.ast.roots.iter() {
            let r = t.compile(n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }
        let c = LLVMCompiler::new(&ctx, ast.custom_structs, ffi.as_ref());
        for n in ast.ast.roots {
            let r = c.compile(&n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }

        c.module
            .verify()
            .map_err(|e| anyhow::anyhow!("{:?}", e))
            .unwrap();

        let jit = c
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        unsafe {
            let func = jit
                .get_function::<unsafe extern "C" fn() -> i64>("main_fn")
                .unwrap();
            assert_eq!(func.call(), expected);
        };
    }
}
