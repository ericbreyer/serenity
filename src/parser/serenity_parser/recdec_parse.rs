use std::{
    cell::RefCell,
    collections::{HashMap, HashSet}, rc::Rc,
};

use indexmap::IndexMap;
use tracing::debug;

use crate::{
    lexer::TokenType,
    prelude::*,
    typing::{Closure, UValueType},
};

use super::{Precedence, SerenityParser};

impl SerenityParser {
    //--------------------------------------
    // Recursive Decent
    //--------------------------------------

    pub(super) fn declaration(&mut self) -> Vec<ASTNode> {
        let ret;
        if self.match_token(TokenType::Var) {
            ret = self.var_declaration(false);
        } else if self.match_token(TokenType::Const) {
            ret = self.var_declaration(true);
        } else if self.match_token(TokenType::Fun) {
            ret = self.fun_declaration();
        } else if self.match_token(TokenType::Type) {
            return self.type_declaration();
        } else if self.match_token(TokenType::Include) {
            self.consume(TokenType::String, "Expect SharedString after include.");
            let mut path = self.previous.lexeme.clone();
            path = path.trim_matches('"').into();
            self.consume(TokenType::Semicolon, "Expect ';' after include.");
            let source = std::fs::read_to_string(path.as_ref()).expect("Failed to read file");
            let mparse_result = Self::parse_helper(source.into(), path, false, HashMap::default());
            let Ok(parse_result) = mparse_result else {
                self.had_error.set(Some(mparse_result.err().unwrap()));
                return vec![ASTNode::Empty];
            };
            self.custom_types.extend(parse_result.custom_structs);
            return parse_result.ast.roots;
        } else {
            ret = ASTNode::Statement(self.statement());
        }
        if self.panic_mode.get() {
            self.synchronize();
        }
        if let ASTNode::Empty = &ret {
            vec![]
        } else {
            vec![ret]
        }
    }

    fn statement(&mut self) -> Statement {
        let line_no = self.previous.line;
        if self.match_token(TokenType::LeftBrace) {
            let statements = self.block();
            Statement::Block(BlockStatement {
                statements,
                line_no,
            })
        } else if self.match_token(TokenType::If) {
            return self.if_statement();
        } else if self.match_token(TokenType::While) {
            return self.while_statement();
        } else if self.match_token(TokenType::For) {
            return self.for_statement();
        } else if self.match_token(TokenType::Semicolon) {
            return Statement::Expression(ExpressionStatement {
                expr: Expression::Empty.into(),
                line_no,
            });
        } else if self.match_token(TokenType::Break) {
            self.consume(TokenType::Semicolon, "Expect ';' after break.");
            return Statement::Break(BreakStatement { line_no });
        } else if self.match_token(TokenType::Continue) {
            self.consume(TokenType::Semicolon, "Expect ';' after continue.");
            return Statement::Continue(ContinueStatement { line_no });
        } else if self.match_token(TokenType::Return) {
            let mut return_node = None;
            if self.match_token(TokenType::Semicolon) {
            } else {
                let e_node = self.expression();
                return_node = e_node.into();
                self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            }
            return Statement::Return(ReturnStatement {
                value: return_node.map(Box::new),
                line_no,
            });
        } else {
            return self.expression_statement();
        }
    }

    pub(super) fn expression(&mut self) -> Expression {
        self.parse_precedence(Precedence::Assignment)
    }

    //-----Statements------//

    pub(super) fn block(&mut self) -> Vec<ASTNode> {
        let mut statements = Vec::new();
        while self.current.token_type != TokenType::RightBrace
            && self.current.token_type != TokenType::Eof
        {
            statements.extend(self.declaration());
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
        statements
    }

    fn expression_statement(&mut self) -> Statement {
        let node = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        let line_no = self.previous.line;
        Statement::Expression(ExpressionStatement {
            expr: Box::new(node),
            line_no,
        })
    }

    fn if_statement(&mut self) -> Statement {
        let line_no = self.previous.line;
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        let cond_node = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let conseq = self.statement();

        let mut alt = None;
        if self.match_token(TokenType::Else) {
            alt = Some(self.statement());
        }
        Statement::If(IfStatement {
            condition: Box::new(cond_node),
            then_branch: Box::new(conseq),
            else_branch: alt.map(Box::new),
            line_no,
        })
    }

    fn while_statement(&mut self) -> Statement {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        let cond_node = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let line_no = self.previous.line;

        let body = self.statement();
        Statement::While(WhileStatement {
            condition: Box::new(cond_node),
            body: Box::new(body),
            line_no,
        })
    }

    fn for_statement(&mut self) -> Statement {
        let _ = self.previous.line;
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        let line_no = self.previous.line;

        let initializer: Option<ASTNode>;
        if self.match_token(TokenType::Semicolon) {
            initializer = ASTNode::Expression(Expression::Empty).into();
        } else if self.match_token(TokenType::Var) {
            initializer = self.var_declaration(false).into();
        } else {
            initializer = ASTNode::Statement(self.expression_statement()).into();
        }

        let mut cond_node = None;
        if !self.match_token(TokenType::Semicolon) {
            let n = self.expression();
            cond_node = Some(n);
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");
        }
        let mut increment_node = None;
        if !self.match_token(TokenType::RightParen) {
            let n = self.expression();
            increment_node = n.into();
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");
        }

        let body = self.statement();

        Statement::For(ForStatement {
            init: initializer.map(Box::new),
            condition: cond_node.map(Box::new),
            increment: increment_node.map(Box::new),
            body: Box::new(body),
            line_no,
        })
    }

    //----Declarations----//

    fn type_declaration(&mut self) -> Vec<ASTNode> {
        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            while self.current.token_type != TokenType::Greater {
                self.consume(TokenType::Identifier, "Expect type parameter name.");
                type_params.push(self.previous.lexeme.clone());
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
            self.consume(TokenType::Greater, "Expect '>' after type parameters.");
        }

        self.consume(TokenType::Identifier, "Expect type name.");
        let name = self.previous.lexeme.clone();

        if self.match_token(TokenType::Struct) {
            self.struct_declaration(name, type_params)
        } else if self.match_token(TokenType::Interface) {
            self.interface_definition(name)
        } else {
            self.error("Expect struct or interface in type definition");
            vec![ASTNode::Empty]
        }
    }

    fn struct_declaration(
        &mut self,
        name: SharedString,
        type_params: Vec<SharedString>,
    ) -> Vec<ASTNode> {
        let _ = self.previous.line;

        if self.custom_types.contains_key(&name) {
            self.error("Struct with that name already exists.");
        }

        let mut fields = IndexMap::new();
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let offset = 0;
        self.custom_types.insert(
            name.clone(),
            CustomStruct {
                name: name.clone(),
                fields: fields.clone().into(),
                embed: None,
                methods: HashSet::new().into(),
                type_vars: type_params.clone().into(),
            },
        );
        let mut embed = None;
        while self.current.token_type != TokenType::RightBrace {
            if !self.match_token(TokenType::Identifier) {
                if !offset == 0 {
                    self.error("Expect field name.");
                    continue;
                }
                if !self.match_token(TokenType::Struct) {
                    self.error("Can only embed structs.");
                    continue;
                }
                self.consume(TokenType::Identifier, "Expect struct name.");
                let s_name = self.previous.lexeme.clone();
                let Some(s) = self.custom_types.get(&s_name) else {
                    self.error("Struct not found.");
                    continue;
                };
                for (field_name, field) in s.fields.borrow().iter() {
                    fields.insert(
                        field_name.clone(),
                        StructEntry {
                            value: field.value.decay(),
                            offset,
                        },
                    );
                }
                embed = Some(s_name);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
                continue;
            }

            let field_name = self.previous.clone();
            self.consume(TokenType::Colon, "Expect ':' after field name.");
            let field_type = self.parse_type(Some(&name), Some(&type_params));

            // if the field is this struct throw an error
            if let ValueType::Struct(s) = field_type {
                if s.name == name {
                    self.error("Struct cannot contain itself.");
                }
            }

            fields.insert(
                field_name.lexeme.clone(),
                StructEntry {
                    value: field_type.decay(),
                    offset,
                },
            );
            if !self.match_token(TokenType::Comma) {
                break;
            }
        }
        self.consume(
            TokenType::RightBrace,
            "Expect '}' after struct declaration.",
        );
        // debug!("Struct: {:?}", self.custom_types.get(&name.lexeme));
        // debug!("New fields: {:?}", fields);
        self.custom_types
            .get(&name)
            .unwrap()
            .fields
            .borrow_mut()
            .clone_from(&fields);
        self.custom_types.get_mut(&name).unwrap().embed = embed.clone();
        debug!("after Struct: {:?}", self.custom_types.get(&name).unwrap());

        let mut impl_nodes = vec![];

        if self.match_token(TokenType::Implements) {
            while self.match_token(TokenType::Identifier) {
                let interface = &self.previous.lexeme;
                let interfacev = format!("{}_vtable", self.previous.lexeme).into();
                let Some(interface_vtab) = self.custom_types.get(&interfacev) else {
                    return vec![ASTNode::Empty];
                };
                let mut s = String::new();
                for f in interface_vtab.fields.borrow().iter().map(|f| f.0) {
                    s = format!("fn (self: *struct {name}) {f}(");
                    let iv = interface_vtab.fields.borrow();
                    let ValueType::Closure(Closure{ args: _, upvals: _, ret, generics: _ }) = iv.get(f).unwrap().value else {
                        self.error("interfaces may only contain methods");
                        continue;
                    };
                    let ValueType::Closure(Closure{ args: atypes, upvals: _, ret, generics: _ }) = ret else {
                        self.error("interfaces may only contain methods");
                        continue;
                    };
                    for (i, t) in atypes.iter().enumerate() {
                        s = format!("{s} a{i} : {}, ", t.to_string());
                    }
                    s = format!("{s}) -> {};", ret.to_string());
                }
                s = format!("{s}let {name}_{interface}_vtable : struct {interfacev};");

                s = format!(
                    "{s}\n\n\
                    fn {name}_impl_{interface}(self: *struct {name}) -> impl {interface} {{ \n"
                );
                for f in interface_vtab.fields.borrow().iter().map(|f| f.0) {
                    s = format!("{s} {name}_{interface}_vtable.{f} = struct {name}::{f};\n");
                }
                s = format!("{s} \n\
                        \treturn struct {interface}_impl{{vtable: &{name}_{interface}_vtable, self: self}}; \n\
                    }}
                    "
                );
                impl_nodes.extend(
                    Self::parse_helper(
                        s.into(),
                        format!("{name}_{interface}_impl").into(),
                        false,
                        self.custom_types.clone(),
                    )
                    .unwrap()
                    .ast
                    .roots,
                );
            }
        }

        self.consume(TokenType::Semicolon, "Expect ';' after struct declaration.");

        impl_nodes
    }

    fn interface_definition(&mut self, name: SharedString) -> Vec<ASTNode> {
        let vtable_name: SharedString = format!("{name}_vtable").into();
        let _ = self.previous.line;

        debug!("before Interface: {}", vtable_name);

        if self.custom_types.contains_key(&vtable_name) {
            self.error("Struct with that name already exists.");
        }

        let mut fields = IndexMap::new();
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let offset = 0;
        self.custom_types.insert(
            vtable_name.clone(),
            CustomStruct {
                name: vtable_name.clone(),
                fields: fields.clone().into(),
                embed: None,
                methods: HashSet::new().into(),
                type_vars: vec![].into(),
            },
        );
        let mut methods: Vec<(SharedString, SharedString)> = vec![];

        while self.current.token_type != TokenType::RightBrace {
            if !self.match_token(TokenType::Identifier) {
                self.error("Expect field name.");
                continue;
            }

            let field_name = self.previous.clone();
            self.consume(TokenType::Colon, "Expect ':' after field name.");
            let field_type = self.parse_type(Some(&vtable_name), None);

            // if the field is this struct throw an error
            let ValueType::Closure(Closure{args, upvals, ret, generics: _}) = &field_type else {
                self.error("interfaces may only contain functions");
                continue;
            };
            assert!(upvals.is_empty());

            let new_field_type = ValueType::Closure(Closure::new(
                vec![ValueType::Pointer(ValueType::Nil.intern(), false).intern()]
                    .into_boxed_slice(),
                Box::new([]),
                ValueType::Closure(Closure::new(
                    args.clone(),
                    vec![ValueType::Pointer(ValueType::Nil.intern(), false).intern()]
                        .as_slice()
                        .into(),
                    ret,
                    None,
                ))
                .intern(),
                None,
            ));

            fields.insert(
                field_name.lexeme.clone(),
                StructEntry {
                    value: new_field_type.intern(),
                    offset,
                },
            );

            let method_string = {
                let mut s = "fn".to_string();
                s = format!("{s} (self: *struct {name}_impl)");
                s = format!("{s} {}(", field_name.lexeme.clone());
                for (i, t) in args.iter().enumerate() {
                    s = format!("{s} a{i} : {}, ", t.to_string());
                }
                s = format!("{s}) -> {}{{\n", ret.to_string());
                s = format!(
                    "{s} return (self->vtable->{})(self->self)(",
                    field_name.lexeme.clone()
                );
                for (i, _) in args.iter().enumerate() {
                    s = format!("{s} a{i}, ");
                }
                s = format!("{s});\n}}");
                s
            };

            methods.push((field_name.lexeme.clone(), method_string.into()));

            if !self.match_token(TokenType::Comma) {
                break;
            }
        }
        self.consume(
            TokenType::RightBrace,
            "Expect '}' after struct declaration.",
        );
        self.consume(TokenType::Semicolon, "Expect ';' after struct declaration.");
        // debug!("Struct: {:?}", self.custom_types.get(&name.lexeme));
        // debug!("New fields: {:?}", fields);
        self.custom_types
            .get(&vtable_name)
            .unwrap()
            .fields
            .borrow_mut()
            .clone_from(&fields);
        debug!(
            "after Struct: {:?}",
            self.custom_types.get(&vtable_name).unwrap()
        );

        let impler_name: SharedString = format!("{name}_impl").into();
        let impler = CustomStruct {
            name: impler_name.clone(),
            fields: IndexMap::from([
                (
                    "self".into(),
                    StructEntry {
                        value: ValueType::Pointer(ValueType::Nil.intern(), false).intern(),
                        offset: 0,
                    },
                ),
                (
                    "vtable".into(),
                    StructEntry {
                        value: ValueType::Pointer(
                            ValueType::Struct(self.custom_types.get(&vtable_name).unwrap().clone())
                                .intern(),
                            false,
                        )
                        .intern(),
                        offset: 1,
                    },
                ),
            ])
            .into(),
            embed: None,
            methods: RefCell::new(methods.iter().map(|(name, _)| name.clone()).collect()),
            type_vars: vec![].into(),
        };

        self.custom_types.insert(impler_name, impler);

        methods
            .iter()
            .flat_map(|(methname, node)| {
                Self::parse_helper(
                    node.clone(),
                    format!("{name}_{}_meth_mod", methname).into(),
                    false,
                    self.custom_types.clone(),
                )
                .unwrap()
                .ast
                .roots
            })
            .collect()
    }

    fn var_declaration(&mut self, is_const: bool) -> ASTNode {
        let line_no = self.previous.line;

        self.consume(TokenType::Identifier, "Expect variable name.");
        let name = self.previous.lexeme.clone();

        let mut var_type = None;
        if self.match_token(TokenType::Colon) {
            var_type = self.parse_type(None, None).into();
        }
        let mut initializer = None;
        if self.match_token(TokenType::Equal) {
            let node = self.expression();
            initializer = Some(node);
        }

        self.consume(
            TokenType::Semicolon,
            format!(
                "Expect {} after variable name.",
                match (var_type.is_some(), initializer.is_some()) {
                    (true, true) => "';'",
                    (true, false) => "';' or '='",
                    (false, true) => "';'",
                    (false, false) => "';' or ':' or '='",
                }
            )
            .as_str(),
        );

        if is_const {
            let value = initializer.as_ref().and_then(|x| x.eval_constexpr());
            let Some(value) = value else {
                self.error("Const variable must be initialized with a constant value.");
                return ASTNode::Empty;
            };
            self.constants.insert(name.clone(), value);
            return ASTNode::Empty;
        }

        ASTNode::Declaration(Declaration::Var(VarDeclaration {
            name,
            tipe: var_type.unwrap_or(ValueType::new_type_var()),
            initializer: initializer.map(Box::new),
            mutable: true,
            line_no,
        }))
    }

    pub(super) fn struct_initializer(&mut self, t: UValueType) -> Expression {
        let line_no = self.previous.line;
        let ValueType::Struct(s) = t else {
            self.error("Expect struct type.");
            return Expression::Empty;
        };
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let mut fields = IndexMap::new();
        while self.current.token_type != TokenType::RightBrace {
            self.consume(TokenType::Identifier, "Expect field name.");
            let name = self.previous.clone();
            self.consume(TokenType::Colon, "Expect ':' after field name.");
            let value = self.expression();
            fields.insert(name.lexeme.clone(), value);
            if !self.match_token(TokenType::Comma) {
                break;
            }
        }
        self.consume(
            TokenType::RightBrace,
            "Expect '}' after struct initializer.",
        );
        Expression::StructInitializer(StructInitializerExpression {
            struct_type: s.clone(),
            fields,
            line_no,
        })
    }

    fn fun_declaration(&mut self) -> ASTNode {
        let line_no = self.previous.line;

        if self.match_token(TokenType::LeftParen) {
            self.consume(TokenType::Identifier, "Expect receiver name.");
            let receiver = self.previous.lexeme.clone();
            self.consume(TokenType::Colon, "Expect ':' after receiver  name.");
            let receiver_type = self.parse_type(None, None);
            self.consume(TokenType::RightParen, "Expect ')' after receiver.");
            let ValueType::Pointer(s, _) = receiver_type else {
                self.error("Receiver must be a struct pointer.");
                return ASTNode::Empty;
            };

            let ValueType::Struct(cs) = s else {
                self.error("Receiver must be a struct pointer.");
                return ASTNode::Empty;
            };

            self.consume(TokenType::Identifier, "Expect function name.");
            let name = self.previous.lexeme.clone();

            let new_name: SharedString = format!("{}_{}", s.id_str(), name).into();

            cs.methods.borrow_mut().insert(new_name.clone());

            let inner_name = format!("{}_inner", new_name);
            let mut node = self.function(&inner_name, vec![]);

            node.prototype.captures.push(receiver.clone());

            let mut node_type = vec![];
            for (_name, t, _) in node.prototype.params.iter() {
                node_type.push(*t);
            }
            let node_return = node.prototype.return_type;

            let decl = ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
                prototype: Prototype {
                    params: vec![(receiver, receiver_type, false)],
                    return_type: ValueType::Closure(Closure::new(
                        node_type.into_boxed_slice(),
                        vec![receiver_type].into_boxed_slice(),
                        node_return,
                        None,
                    ))
                    .intern(),
                    name: new_name,
                    captures: vec![],
                    line_no,
                },
                body: Rc::new(vec![ASTNode::Statement(Statement::Return(ReturnStatement {
                    value: Some(Box::new(Expression::Function(node))),
                    line_no,
                }))]
                .into()),

                line_no,
                type_params: vec![],
                generic_instantiations: FunctionGenerics::Monomorphic(IndexMap::new()),
            }));
            return decl;
        }

        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            loop {
                self.consume(TokenType::Identifier, "Expect capture name.");
                type_params.push(self.previous.lexeme.clone());
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
            self.consume(TokenType::Greater, "Expect '>' after captures.");
        }

        self.consume(TokenType::Identifier, "Expect function name.");
        let name = self.previous.lexeme.clone();
        let node = self.function(&name, type_params.clone());

        let mono = type_params.is_empty();

        ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
            prototype: node.prototype,
            body: node.body.into(),
            line_no,
            type_params,
            generic_instantiations: if mono {
                FunctionGenerics::Monomorphic(IndexMap::new())
            } else {
                FunctionGenerics::Parametric(Rc::new(Vec::new().into()))
            },
        }))
    }
}
