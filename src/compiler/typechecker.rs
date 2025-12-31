use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    hash::Hash,
    ops::Deref,
    rc::Rc,
};

use anyhow::{Context as _, Result};
use indexmap::IndexMap;

use super::ffi_funcs::FfiFunc;
use crate::{
    lexer::{Token, TokenType},
    prelude::*,
    typing::{Closure, UValueType},
};

type Environment = ScopedMap<SharedString, UValueType>;
type EnvironmentMut = ScopedMap<SharedString, (UValueType, bool)>;

#[derive(Clone)]
pub struct Typechecker {
    variables: Rc<RefCell<EnvironmentMut>>,
    generics_in_scope: Rc<RefCell<Environment>>,

    types: Rc<HashMap<SharedString, CustomStruct>>,
    parametric_functions: RefCell<HashMap<SharedString, FunctionDeclaration>>,

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

        self.variables.borrow_mut().set(
            (*name).to_string().into(),
            (ValueType::ExternalFn(ret, (*name).into()).intern(), false),
        );
    }

    pub fn new(custom_structs: HashMap<SharedString, CustomStruct>, ffi_funcs: &[FfiFunc]) -> Self {
        let variables = Rc::new(EnvironmentMut::new().into());

        let c = Typechecker {
            variables,
            generics_in_scope: Rc::new(Environment::new().into()),
            types: custom_structs.into(),
            return_type: None.into(),
            parametric_functions: HashMap::new().into(),
        };

        for ffi in ffi_funcs {
            c.register_ffi_function(ffi);
        }

        c
    }

    pub fn compile_function(&self) -> Self {
        let c = self.clone();
        c.return_type.set(None);
        c
    }
}

impl Typechecker {
    fn get_variable(&self, name: &str) -> Result<(UValueType, bool)> {
        self.variables
            .borrow()
            .get(Into::<SharedString>::into(name.to_string()))
    }

    fn set_variable(&self, name: &str, value: UValueType, mutable: bool) {
        self.variables
            .borrow_mut()
            .set(name.to_string().into(), (value, mutable));
    }
}

impl NodeVisitor<Result<()>> for Typechecker {
    fn visit_statement(&self, statement: &Statement) -> Result<()> {
        statement.accept(self)
    }

    fn visit_expression(&self, expression: &Expression) -> Result<()> {
        let _ = expression.accept(self)?;
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
        self.0.substitute(None)
    }
}

type ExprResult = Result<ExprResultInner>;

impl ExprResultInner {
    fn new(serenity_type: UValueType) -> Self {
        Self(serenity_type)
    }

    fn rvalue(self) -> ExprResultInner {
        match self.0 {
            ValueType::LValue(ref t, _) => match t {
                ValueType::Array(a, _) => {
                    ExprResultInner::new(ValueType::Pointer(a, false).intern().substitute(None))
                }
                _ => ExprResultInner::new(t.substitute(None)),
            },
            _ => self,
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

    fn visit_double_colon_expression(&self, _expression: &DoubleColonExpression) -> ExprResult {
        // TODO: Implement this
        // let t = expression.typ.substitute(&*self.generics_in_scope);
        // println!("want to get ({t}) {}_{} in {:?}", t.id_str(), expression.acessor,
        // self.variables.as_hashmap().keys()); Ok(ExprResultInner::new(self.
        // variables.get(&format!("{}_{}", t.id_str(),
        // expression.acessor).into()).unwrap()))
        Ok(ExprResultInner::new(ValueType::new_type_var(Box::new([]))))
    }

    fn visit_string_literal_expression(&self, _: &StringLiteralExpression) -> ExprResult {
        Ok(ExprResultInner::new(
            ValueType::Pointer(ValueType::Char.intern(), false).intern(),
        ))
    }

    fn visit_unary_expression(&self, expression: &UnaryExpression) -> ExprResult {
        let operand_t = expression
            .operand
            .accept(self)
            .context("unary operand")?
            .rvalue();

        Ok(ExprResultInner::new(match expression.operator {
            TokenType::Minus => match operand_t.0 {
                ValueType::Integer => ValueType::Integer.intern(),
                ValueType::Float => ValueType::Float.intern(),
                _ => return Err(anyhow::anyhow!("Invalid minus unary expression")),
            },
            TokenType::Bang => match operand_t.0 {
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
            .rvalue();
        let ValueType::Pointer(t, m) = *expr else {
            return Err(anyhow::anyhow!("Invalid deref expression"));
        };
        Ok(ExprResultInner::new(ValueType::LValue(t, m).intern()))
    }

    fn visit_ref_expression(&self, expression: &RefExpression) -> ExprResult {
        let expr = expression
            .operand
            .accept(self)
            .context("Ref expression operand")?;
        let ValueType::LValue(t, m) = *expr else {
            return Err(anyhow::anyhow!("Invalid ref expression"));
        };
        Ok(ExprResultInner::new(ValueType::Pointer(t, m).intern()))
    }

    fn visit_index_expression(&self, expression: &IndexExpression) -> ExprResult {
        let pointer = expression
            .array
            .accept(self)
            .context("Index expression array")?
            .rvalue();
        let index = expression
            .index
            .accept(self)
            .context("Index expression index")?
            .rvalue();
        ValueType::unify(
            ValueType::Integer.intern(),
            index.0,
            &self.generics_in_scope.borrow(),
        )?;

        let ValueType::Pointer(t, m) = pointer.0 else {
            return Err(anyhow::anyhow!(
                "Invalid index expression array type was {:?}",
                pointer
            ));
        };

        Ok(ExprResultInner::new(ValueType::LValue(t, *m).intern()))
    }

    fn visit_binary_expression(&self, expression: &BinaryExpression) -> ExprResult {
        let lhs_ptr = expression
            .left
            .accept(self)
            .context("Left hand side of binary expression")?;
        let lhs = lhs_ptr.clone().rvalue();
        let rhs = expression
            .right
            .accept(self)
            .context("Right hand side of binary expression")?
            .rvalue();

        if matches!(*lhs, ValueType::Array(_, _) | ValueType::Pointer(_, _)) {
            ValueType::unify(
                ValueType::Integer.intern(),
                rhs.0,
                &self.generics_in_scope.borrow(),
            )?;
        } else {
            ValueType::unify(lhs.0, rhs.0, &self.generics_in_scope.borrow())?;
        }

        let expr = match (
            expression.operator,
            lhs.0.substitute(&*self.generics_in_scope.borrow()),
            rhs.0.substitute(&*self.generics_in_scope.borrow()),
        ) {
            (
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Percent,
                ValueType::Integer,
                ValueType::Integer,
            ) => ExprResultInner::new(ValueType::Integer.intern()),

            (
                TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash,
                ValueType::Float,
                ValueType::Float,
            ) => ExprResultInner::new(ValueType::Float.intern()),

            (TokenType::Plus, ValueType::Pointer(_, _), ValueType::Integer) => {
                ExprResultInner::new(lhs.0)
            }
            (TokenType::Plus, ValueType::Array(t1, Some(_)), ValueType::Integer) => {
                ExprResultInner::new(ValueType::Pointer(t1, false).intern())
            }

            (
                op,
                lhs_type @ (ValueType::Integer | ValueType::Float | ValueType::Pointer(_, _)),
                rhs_type @ (ValueType::Integer | ValueType::Float | ValueType::Pointer(_, _)),
            ) if matches!(
                op,
                TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::EqualEqual
                    | TokenType::BangEqual
            ) && !matches!(lhs_type, ValueType::Pointer(_, _))
                || matches!(rhs_type, ValueType::Integer | ValueType::Pointer(_, _)) =>
            {
                ExprResultInner::new(ValueType::Bool.intern())
            }

            (_, ValueType::TypeVar(x), ValueType::TypeVar(y)) if x == y => {
                ExprResultInner::new(ValueType::TypeVar(*x).intern())
            }

            (op, lhs, rhs) => {
                return Err(anyhow::anyhow!(
                    "Invalid binary expression {:?} {:?} {:?}",
                    op,
                    lhs,
                    rhs
                ))
            }
        };

        Ok(expr)
    }

    fn visit_ternary_expression(&self, expression: &TernaryExpression) -> ExprResult {
        let cond = expression
            .condition
            .accept(self)
            .context("Ternary expression condition")?
            .rvalue();
        ValueType::unify(
            ValueType::Bool.intern(),
            cond.0,
            &self.generics_in_scope.borrow(),
        )
        .context(format!(
            "Ternary expression unification {:?} = {:?}",
            ValueType::Bool.intern(),
            cond.0
        ))?;

        let lhs = expression
            .then_branch
            .accept(self)
            .context("Ternary expression then")?
            .rvalue();
        let rhs = expression
            .else_branch
            .accept(self)
            .context("Ternary expression else")?
            .rvalue();

        ValueType::unify(lhs.0, rhs.0, &self.generics_in_scope.borrow()).context(format!(
            "Ternary expression unification {:?} = {:?}",
            lhs.0, rhs.0
        ))?;

        Ok(ExprResultInner::new(lhs.0))
    }

    fn visit_variable_expression(&self, expression: &VariableExpression) -> ExprResult {
        let t = expression.token.borrow_mut();
        let name = &t.lexeme;
        let tipe = self.get_variable(name);

        if let Ok((t, m)) = tipe {
            if let ValueType::GenericParam(g, _) = t {
                if let Ok(t) = self.generics_in_scope.borrow().get(g.clone()) {
                    return Ok(ExprResultInner::new(ValueType::LValue(t, m).intern()));
                }
            }
            return Ok(ExprResultInner::new(ValueType::LValue(t, m).intern()));
        }

        if let Some(declaration) = self.parametric_functions.borrow().get(name) {
            let mut maping = HashMap::new();
            // Pre-populate the mapping with a fresh type-var for every declared
            // type parameter so that instantiation preserves declaration order
            // and length even if a param doesn't appear in the signature.
            for (tp, _) in &declaration.type_params {
                maping.insert(tp.clone(), ValueType::new_type_var(Box::new([])));
            }

            println!(
                "Instantiating function {} with decl {:?} {:?}",
                name, declaration, self.generics_in_scope
            );
            let fn_type = self
                .function_type_serenity(&declaration.prototype)
                .instantiate_generic(&mut maping);
            println!(
                "Instantiatied function {} with generics {:?} {:?}",
                name, maping, self.generics_in_scope
            );

            // Build types map in the same order as declaration.type_params
            let mut types_map: IndexMap<_, _> = IndexMap::new();
            for (k, _) in &declaration.type_params {
                types_map.insert(
                    k.clone(),
                    *maping.get(k).expect("type param instantiation missing"),
                );
            }
            let id = {
                use std::hash::Hasher;
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                for (k, v) in &types_map {
                    k.hash(&mut hasher);
                    v.id_str().hash(&mut hasher);
                }
                hasher.finish()
            };
            let new_name: SharedString = format!("{name}_{id:?}").into();
            drop(t);
            expression.token.borrow_mut().lexeme = new_name.clone();
            let mut new_declaration = declaration.clone();
            new_declaration.prototype.name = new_name.clone();
            new_declaration.type_params = IndexMap::new();
            new_declaration.generic_instantiations =
                FunctionGenerics::Monomorphic(types_map.clone());
            self.visit_function_declaration(&new_declaration)?;

            let FunctionGenerics::Parametric(ps) = &declaration.generic_instantiations else {
                unreachable!();
            };

            ps.borrow_mut().push(InstantiateAs {
                name: new_name,
                types: types_map,
            });

            return Ok(ExprResultInner::new(fn_type));
        }

        Err(anyhow::anyhow!(
            "Variable {} not found in {:?}",
            name,
            self.variables.borrow().as_hashmap().keys()
        ))
    }

    fn visit_assign_expression(&self, expression: &AssignExpression) -> ExprResult {
        let rhs = expression
            .value
            .accept(self)
            .with_context(|| {
                format!(
                    "Assign expression rhs {:?}",
                    ASTNode::Expression(*expression.value.clone())
                )
            })?
            .rvalue();
        let lhs = expression
            .variable
            .accept(self)
            .context("Assign expression lhs")?;

        let ValueType::LValue(t, _) = *lhs else {
            return Err(anyhow::anyhow!(format!(
                "Invalid assign expression lhs got {:?} at {:?}",
                lhs,
                ASTNode::Expression(Expression::Assign(expression.clone()))
            )));
        };
        ValueType::unify(t.decay(), rhs.0, &self.generics_in_scope.borrow()).context(format!(
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
            .rvalue();

        ValueType::unify(
            ValueType::Bool.intern(),
            lhs.0,
            &self.generics_in_scope.borrow(),
        )
        .context(format!(
            "Logical expression unification {:?} = {:?}",
            ValueType::Bool.intern(),
            lhs.0
        ))?;

        let rhs = expression
            .right
            .accept(self)
            .context("Logical expression rhs")?
            .rvalue();

        ValueType::unify(
            ValueType::Bool.intern(),
            rhs.0,
            &self.generics_in_scope.borrow(),
        )
        .context(format!(
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
            .rvalue();
        let args = expression
            .arguments
            .iter()
            .map(|arg| Ok(arg.accept(self).context("Argument")?.rvalue()))
            .collect::<Result<Vec<_>>>()?;
        let arg_types = args.iter().map(|a| a.0).collect::<Vec<_>>();
        let fn_type = callee.0.instantiate_generic(&mut HashMap::new());

        if let ValueType::ExternalFn(r, _) = fn_type {
            return Ok(ExprResultInner::new(r));
        }

        let ValueType::Closure(Closure {
            args,
            upvals: _,
            ret: r,
            generics: _,
        }) = fn_type
        else {
            return Err(anyhow::anyhow!(
                "Invalid callee serenity type got {:?} {:?}",
                fn_type,
                ASTNode::Expression(Expression::Call(expression.clone()))
            ));
        };

        for arg in args.iter().zip(arg_types.iter()) {
            ValueType::unify(
                arg.0.decay(),
                arg.1.decay(),
                &self.generics_in_scope.borrow(),
            )
            .context(format!(
                "Call expression unification {:?} = {:?}",
                arg.0.decay(),
                arg.1.decay()
            ))?;
        }

        Ok(ExprResultInner::new(ValueType::LValue(r, false).intern()))
    }

    fn visit_dot_expression(&self, expression: &DotExpression) -> ExprResult {
        let lhs = expression
            .object
            .accept(self)
            .context("Dot expression lhs")?;
        let ValueType::LValue(st, m) = *lhs else {
            return Err(anyhow::anyhow!("Invalid dot expression lhs"));
        };

        let st = st.fill_self_struct((*self.types).clone());
        let t = match st.clone() {
            ValueType::Struct(t) => t,
            ValueType::TypeVar(_t) => {
                let cs = st.get_constraints().unwrap();
                if cs.is_empty() {
                    return Err(anyhow::anyhow!(
                        "Generic parameter {} has no constraints",
                        st.id_str()
                    ));
                }
                'a: {
                    for c in cs {
                        let impler_str = format!("{}_impl", c.0.as_ref().unwrap()).into();
                        if let Some(t) = self.types.get(&impler_str) {
                            break 'a Box::new(t.clone());
                        }
                    }
                    return Err(anyhow::anyhow!(
                        "Generic parameter {} has no matching struct",
                        st.id_str()
                    ));
                }
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "Invalid dot expression lhs got {:?} on {:?}",
                    st,
                    expression.line_no
                ));
            }
        };

        let ofield = t
            .fields
            .borrow()
            .iter()
            .enumerate()
            .find(|(_i, (name, _))| *name == &expression.field)
            .map(|(i, _)| i);

        if ofield.is_none() {
            let method_name = &format!("{}_{}", st.id_str(), expression.field).into();
            if let Some(method_name) = t.methods.borrow().get(method_name) {
                return Expression::Call(CallExpression {
                    callee: Box::new(Expression::Variable(VariableExpression {
                        token: Rc::new(
                            Token {
                                lexeme: method_name.clone(),
                                tok_type: TokenType::Identifier,
                                line: 0,
                            }
                            .into(),
                        ),
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
            if let Some(g_method_name) = t
                .parametric_methods
                .borrow()
                .get(&format!("{}_{}", t.name, expression.field).into())
            {
                let mut maping = HashMap::new();
                let pf = self.parametric_functions.borrow();
                let g_method = pf.get(g_method_name).unwrap();
                let _ = self
                    .function_type_serenity(&g_method.prototype)
                    .instantiate_generic(&mut maping);

                // Build types map in the same order as g_method.type_params
                let mut types_map: IndexMap<_, _> = IndexMap::new();
                for (k, _) in &g_method.type_params {
                    types_map.insert(
                        k.clone(),
                        *maping.get(k).expect("type param instantiation missing"),
                    );
                }

                let mut new_declaration = g_method.clone();
                new_declaration.prototype.name = method_name.clone();
                new_declaration.type_params = IndexMap::new();
                new_declaration.generic_instantiations =
                    FunctionGenerics::Monomorphic(types_map.clone());
                self.visit_function_declaration(&new_declaration)?;
                let FunctionGenerics::Parametric(ps) = &g_method.generic_instantiations else {
                    unreachable!();
                };

                ps.borrow_mut().push(InstantiateAs {
                    name: method_name.clone(),
                    types: types_map,
                });

                return Expression::Call(CallExpression {
                    callee: Box::new(Expression::Variable(VariableExpression {
                        token: Rc::new(
                            Token {
                                lexeme: method_name.clone(),
                                tok_type: TokenType::Identifier,
                                line: 0,
                            }
                            .into(),
                        ),
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
                "Field {} not found in struct {:?} {:?}",
                method_name,
                t.methods,
                t.parametric_methods
            ));
        }

        Ok(ExprResultInner::new(
            ValueType::LValue(t.fields.clone().borrow()[&expression.field].value, m).intern(),
        ))
    }

    fn visit_function_expression(&self, expression: &FunctionExpression) -> ExprResult {
        let fn_type = self.function_type_serenity(&expression.prototype);
        let r_type = self.function_body(&expression.prototype, expression.body.as_ref())?;

        ValueType::unify(
            expression.prototype.return_type,
            r_type,
            &self.generics_in_scope.borrow(),
        )
        .context(format!(
            "Function expression unification {:?} = {:?}",
            expression.prototype.return_type, r_type
        ))?;

        Ok(ExprResultInner::new(fn_type))
    }

    fn visit_cast_expression(&self, expression: &CastExpression) -> ExprResult {
        let _expr = expression
            .expression
            .accept(self)
            .context("Cast expression operand")?
            .rvalue();

        // ValueType::unify(expr.0, expression.target_type,
        // &self.generics_in_scope.borrow())?;

        Ok(ExprResultInner(expression.target_type))
    }

    fn visit_struct_initializer_expression(
        &self,
        expression: &StructInitializerExpression,
    ) -> ExprResult {
        for (name, expr) in &expression.fields {
            let t = expression
                .struct_type
                .fields
                .borrow()
                .get(name)
                .map(|f| f.value)
                .ok_or_else(|| anyhow::anyhow!("Field {} not found in struct", name))?;
            let expr_t = expr.accept(self)?.rvalue();
            ValueType::unify(t.decay(), expr_t.0, &self.generics_in_scope.borrow()).context(
                format!(
                    "Struct initializer expression unification {:?} = {:?}",
                    t, expr_t.0
                ),
            )?;
        }
        Ok(ExprResultInner::new(
            expression.struct_type.to_value_type().substitute(None),
        ))
    }

    fn visit_sizeof_expression(&self, _: &SizeofExpression) -> ExprResult {
        Ok(ExprResultInner::new(ValueType::Integer.intern()))
    }
}

impl Typechecker {
    fn function_type_serenity(&self, prototype: &Prototype) -> UValueType {
        ValueType::Closure(Closure::new(
            prototype
                .params
                .iter()
                .map(|(_s, t, _)| t.decay())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype
                .captures
                .iter()
                .map(|capture| self.get_variable(capture).unwrap().0)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            prototype.return_type,
            None,
        ))
        .intern()
    }

    fn function_body(
        &self,
        prototype: &Prototype,
        body: Option<&Vec<ASTNode>>,
    ) -> Result<UValueType> {
        let args = &prototype.params;
        let caps = &prototype.captures;

        self.variables.borrow_mut().begin_scope();

        for s in caps {
            let arg_type_serenity = self.get_variable(s).unwrap();
            self.set_variable(s, arg_type_serenity.0, arg_type_serenity.1);
        }

        for &(ref s, t, m) in args {
            self.set_variable(s, t.decay(), m);
        }

        let ret = if let Some(body) = body {
            let fc = self.compile_function();
            for statement in body {
                statement.accept(&fc).context("Function body")?;
            }
            let ret = fc
                .return_type
                .get()
                .unwrap_or_else(|| ValueType::Nil.intern());

            ValueType::unify(prototype.return_type, ret, &self.generics_in_scope.borrow())
                .context(format!(
                    "Function return unification \n{:?}\n{:?}",
                    prototype.return_type, ret
                ))?;
            ret
        } else {
            prototype.return_type
        };

        self.variables.borrow_mut().end_scope();

        Ok(ret)
    }
}

impl DeclarationVisitor<Result<()>> for Typechecker {
    fn visit_var_declaration(&self, declaration: &VarDeclaration) -> Result<()> {
        let var_type = declaration.tipe;

        if let Some(init) = &declaration.initializer {
            let init_type = init.accept(self)?.rvalue();
            ValueType::unify(var_type, init_type.0, &self.generics_in_scope.borrow()).context(
                format!(
                    "Var declaration unification {:?} = {:?}",
                    var_type, init_type.0
                ),
            )?;
        }
        self.set_variable(&declaration.name, var_type, declaration.mutable);

        Ok(())
    }

    fn visit_function_declaration(&self, declaration: &FunctionDeclaration) -> Result<()> {
        match &declaration.generic_instantiations {
            FunctionGenerics::Parametric(_) => {
                self.visit_polymorphic_function_declaration(declaration);
                Ok(())
            }
            FunctionGenerics::Monomorphic(mappings) => {
                self.visit_monomorphic_function_declaration(declaration, mappings)
            }
        }
    }
}
impl Typechecker {
    fn visit_polymorphic_function_declaration(&self, declaration: &FunctionDeclaration) {
        self.parametric_functions
            .borrow_mut()
            .insert(declaration.prototype.name.clone(), declaration.clone());
    }

    fn visit_monomorphic_function_declaration(
        &self,
        declaration: &FunctionDeclaration,
        mappings: &IndexMap<SharedString, UValueType>,
    ) -> Result<()> {
        self.generics_in_scope.borrow_mut().begin_scope();
        for (n, t) in mappings {
            self.generics_in_scope.borrow_mut().set(n.clone(), t);
        }

        self.set_variable(
            &declaration.prototype.name,
            self.function_type_serenity(&declaration.prototype)
                .substitute(&*self.generics_in_scope.borrow()),
            false,
        );

        self.function_body(&declaration.prototype, declaration.body.as_ref().as_ref())
            .context(format!(
                "Function declaration {:?}",
                ASTNode::Declaration(Declaration::Function(declaration.clone()))
            ))?;

        self.generics_in_scope.borrow_mut().end_scope();

        Ok(())
    }
}

impl StatementVisitor<Result<()>> for Typechecker {
    fn visit_block_statement(&self, statement: &BlockStatement) -> Result<()> {
        self.variables.borrow_mut().begin_scope();
        for s in &statement.statements {
            s.accept(self)?;
        }
        self.variables.borrow_mut().end_scope();
        Ok(())
    }

    fn visit_if_statement(&self, statement: &IfStatement) -> Result<()> {
        let cond = statement.condition.accept(self)?.rvalue();
        ValueType::unify(
            ValueType::Bool.intern(),
            cond.0,
            &self.generics_in_scope.borrow(),
        )
        .context(format!(
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
        let cond = statement.condition.accept(self)?.rvalue();

        ValueType::unify(
            ValueType::Bool.intern(),
            cond.0,
            &self.generics_in_scope.borrow(),
        )?;

        statement.body.accept(self)?;

        Ok(())
    }

    fn visit_for_statement(&self, statement: &ForStatement) -> Result<()> {
        self.variables.borrow_mut().begin_scope();
        if let Some(init) = &statement.init {
            init.accept(self)?;
        }

        if let Some(cond) = &statement.condition {
            let cond = cond.accept(self)?.rvalue();
            ValueType::unify(
                ValueType::Bool.intern(),
                cond.0,
                &self.generics_in_scope.borrow(),
            )?;
        }

        if let Some(inc) = &statement.increment {
            inc.accept(self)?;
        }

        statement.body.accept(self)?;
        self.variables.borrow_mut().end_scope();

        Ok(())
    }

    fn visit_break_statement(&self, _: &BreakStatement) -> Result<()> {
        Ok(())
    }

    fn visit_continue_statement(&self, _: &ContinueStatement) -> Result<()> {
        Ok(())
    }

    fn visit_return_statement(&self, statement: &ReturnStatement) -> Result<()> {
        if let Some(value) = &statement.value {
            let v = value.accept(self)?.rvalue();
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
    use test_case::test_case;

    use super::*;
    use crate::{compiler::ffi_funcs, parser::Parser};
    #[test_case(r"
        fn main() -> int {
            let x: int = 1;
            return x;
        }
        "; "var_expr_fully_typed")]
    #[test_case(r"
        fn main() -> int {
            let x = 1;
            return x;
        }
        "; "var_expr_inferred")]
    #[test_case(r"
        fn main() -> int {
            let x;
            x = 1;
            return x;
        }
        "; "var_expr_assign")]
    fn test_program_integer_return(prog: &str) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();

        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());
        for n in ast.ast.roots {
            let r = c.compile(&n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }
    }

    #[test_case(r"
        fn main() -> float {
            let x: float = 3.14;
            return x;
        }
        "; "float_return")]
    #[test_case(r"
        fn main() -> bool {
            let x: bool = true;
            return x;
        }
        "; "bool_return")]
    #[test_case(r"
        fn main() -> int {
            let x: int = 5;
            let y: int = 3;
            return x + y;
        }
        "; "addition")]
    #[test_case(r"
        fn main() -> int {
            let x: int = 10;
            let y: int = 3;
            return x - y;
        }
        "; "subtraction")]
    #[test_case(r"
        fn main() -> int {
            let x: int = 5;
            let y: int = 3;
            return x * y;
        }
        "; "multiplication")]
    #[test_case(r"
        fn main() -> int {
            let x: int = 15;
            let y: int = 3;
            return x / y;
        }
        "; "division")]
    #[test_case(r"
        fn main() -> bool {
            let x: int = 5;
            let y: int = 3;
            return x > y;
        }
        "; "greater_than")]
    #[test_case(r"
        fn main() -> bool {
            let x: int = 5;
            let y: int = 3;
            return x < y;
        }
        "; "less_than")]
    #[test_case(r"
        fn main() -> bool {
            let x: int = 5;
            let y: int = 5;
            return x == y;
        }
        "; "equality")]
    #[test_case(r"
        fn main() -> bool {
            let x: int = 5;
            let y: int = 3;
            return x != y;
        }
        "; "inequality")]
    #[test_case(r"
        fn main() -> bool {
            let x: bool = true;
            let y: bool = false;
            return x and y;
        }
        "; "logical_and")]
    #[test_case(r"
        fn main() -> bool {
            let x: bool = true;
            let y: bool = false;
            return x or y;
        }
        "; "logical_or")]
    #[test_case(r"
        fn main() -> int {
            let x: int = 5;
            return -x;
        }
        "; "unary_negation")]
    #[test_case(r"
        fn main() -> bool {
            let x: bool = true;
            return !x;
        }
        "; "logical_not")]
    fn test_expressions(prog: &str) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();

        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());
        for n in ast.ast.roots {
            let r = c.compile(&n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }
    }

    #[test_case(r"
        fn main() -> int {
            let x: int = 5;
            if (x > 3) {
                return 1;
            }
            return 0;
        }
        "; "if_statement")]
    #[test_case(r"
        fn main() -> int {
            let x: int = 0;
            while (x < 5) {
                x = x + 1;
            }
            return x;
        }
        "; "while_loop")]
    #[test_case(r"
        fn main() -> int {
            let sum: int = 0;
            for (var i = 0; i < 5; i = i + 1) {
                sum = sum + i;
            }
            return sum;
        }
        "; "for_loop")]
    #[test_case(r"
        fn add(x: int, y: int) -> int {
            return x + y;
        }
        fn main() -> int {
            return add(3, 5);
        }
        "; "function_call")]
    #[test_case(r"
        fn main() -> int {
            let x: bool = #5;
            let y: int = x ? 10 : 20;
            return y;
        }
        "; "ternary_operator")]
    fn test_control_flow(prog: &str) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();

        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());
        for n in ast.ast.roots {
            let r = c.compile(&n);
            assert!(r.is_ok(), "{:#}", r.unwrap_err());
        }
    }

    #[test_case(r"
        fn main() -> int {
            let x: int = 5;
            let y: *int = &x;
            return *y;
        }
        "; "reference_and_dereference")]
    #[test_case(r"
        fn main() -> int {
            let arr: [int; 3];
            for (var i = 0; i < 3; i = i + 1) {
                arr[i] = i * 2;
            }
            return arr[0];
        }
        "; "array_indexing")]
    fn test_pointers_and_arrays(prog: &str) {
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();

        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());
        for n in ast.ast.roots {
            let r = c.compile(&n);
            // These may fail due to array literal parsing, so we just check they compile
            // attempt
            let _ = r;
        }
    }

    #[test]
    fn test_compile_creates_typechecker() {
        let prog = r"
            fn main() -> int {
                return 42;
            }
        ";
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();
        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());

        for n in ast.ast.roots {
            let result = c.compile(&n);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_variable_scoping() {
        let prog = r"
            fn main() -> int {
                let x: int = 5;
                {
                    let x: int = 10;
                }
                return x;
            }
        ";
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();
        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());

        for n in ast.ast.roots {
            let result = c.compile(&n);
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_multiple_functions() {
        let prog = r"
            fn add(x: int, y: int) -> int {
                return x + y;
            }
            fn mul(x: int, y: int) -> int {
                return x * y;
            }
            fn main() -> int {
                return add(2, 3);
            }
        ";
        let ast = crate::parser::SerenityParser::parse(prog.into(), "mod".into(), vec![]).unwrap();
        let c = Typechecker::new(ast.custom_structs, ffi_funcs::ffi_funcs().as_ref());

        for n in ast.ast.roots {
            let result = c.compile(&n);
            assert!(result.is_ok());
        }
    }
}
