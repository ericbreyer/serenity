use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use tracing::debug;

use crate::{lexer::TokenType, prelude::*};

use super::{Precedence, SerenityParser};

impl SerenityParser {
    //--------------------------------------
    // Recursive Decent
    //--------------------------------------

    pub(super) fn declaration(&mut self) -> Vec<ASTNode> {
        let ret;
        if self.match_token(TokenType::Var) {
            ret = self.var_declaration(true);
        } else if self.match_token(TokenType::Const) {
            ret = self.var_declaration(false);
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
            return parse_result.ast;
        } else {
            ret = ASTNode::Statement(self.statement());
        }
        if self.panic_mode.get() {
            self.synchronize();
        }
        vec![ret]
    }

    fn statement(&mut self) -> Statement {
        let line_no = self.previous.line;
        if self.match_token(TokenType::Print) {
            self.print_statement()
        } else if self.match_token(TokenType::LeftBrace) {
            let statements = self.block();
            return Statement::Block(BlockStatement {
                statements,
                line_no,
            });
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

    fn print_statement(&mut self) -> Statement {
        let line_no = self.previous.line;
        let node = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        Statement::Print(PrintStatement {
            expr: Box::new(node),
            line_no,
        })
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
        let _line = self.previous.line;
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        let line_no = self.previous.line;

        let initializer: Option<ASTNode>;
        if self.match_token(TokenType::Semicolon) {
            initializer = ASTNode::Expression(Expression::Empty).into();
        } else if self.match_token(TokenType::Var) {
            initializer = self.var_declaration(true).into();
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
        self.consume(TokenType::Identifier, "Expect type name.");
        let name = self.previous.lexeme.clone();

        if self.match_token(TokenType::Struct) {
            self.struct_declaration(name)
        } else if self.match_token(TokenType::Interface) {
            self.interface_definition(name)
        } else {
            self.error("Expect struct or interface in type definition");
            vec![ASTNode::Empty]
        }
    }

    fn struct_declaration(&mut self, name: SharedString) -> Vec<ASTNode> {
        let _line = self.previous.line;

        debug!("before Struct: {}", name);

        if self.custom_types.contains_key(&name) {
            self.error("Struct with that name already exists.");
        }

        let mut fields = HashMap::new();
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let mut offset = 0;
        self.custom_types.insert(
            name.clone(),
            CustomStruct {
                name: name.clone(),
                fields: fields.clone().into(),
                embed: None,
                methods: HashSet::new().into(),
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
                            value: field.value.clone(),
                            offset: offset,
                        },
                    );
                    offset += field.value.num_words();
                }
                embed = Some(s_name);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
                continue;
            }

            let field_name = self.previous.clone();
            self.consume(TokenType::Colon, "Expect ':' after field name.");
            let mut field_type = self.parse_complex_type(&Some(name.clone()));

            // if the field is this struct throw an error
            if let ValueType::Struct(s) = field_type.as_ref() {
                if s.name == name {
                    self.error("Struct cannot contain itself.");
                }
            }

            // if it is a pointer to this struct it is a pointer to selfstruct
            if let ValueType::Pointer(s, _) = field_type.as_ref() {
                if let ValueType::Struct(s) = s.as_ref() {
                    if s.name == name {
                        field_type = ValueType::Pointer(
                            ValueType::SelfStruct(s.name.clone()).intern(),
                            true,
                        )
                        .intern();
                    }
                }
            }

            fields.insert(
                field_name.lexeme.clone(),
                StructEntry {
                    value: field_type,
                    offset,
                },
            );
            offset += field_type.num_words();
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
                    let ValueType::Closure(ftype, _) = iv.get(&f).unwrap().value.as_ref() else {
                        self.error("interfaces may only contain functions");
                        continue;
                    };
                    for (i, t) in ftype[0..ftype.len() - 1].iter().enumerate() {
                        s = format!("{s} a{i} : {}, ", t.to_string());
                    }
                    s = format!("{s}) -> {};", ftype.last().unwrap().to_string());
                }
                s = format!("{s}const {name}_{interface}_vtable = struct {interfacev}{{");
                for f in interface_vtab.fields.borrow().iter().map(|f| f.0) {
                    s = format!("{s} {f}: struct {name}::{f}");
                }
                s = format!("{s}}};");
                s = format!(
                    "{s}\n\n\
                    fn {name}_impl_{interface}(self: *struct {name}) -> impl {interface} {{ \n\
                        \treturn struct {interface}_impl{{vtable: &{name}_{interface}_vtable, self: self}}; \n\
                    }}
                    "
                );
                println!("{s}");
                impl_nodes.extend(
                    Self::parse_helper(
                        s.into(),
                        format!("{name}_{interface}_impl").into(),
                        false,
                        self.custom_types.clone(),
                    )
                    .unwrap()
                    .ast,
                );
            }
        }

        self.consume(TokenType::Semicolon, "Expect ';' after struct declaration.");

        impl_nodes
    }

    fn interface_definition(&mut self, name: SharedString) -> Vec<ASTNode> {
        let vtable_name: SharedString = format!("{name}_vtable").into();
        let _line = self.previous.line;

        debug!("before Interface: {}", vtable_name);

        if self.custom_types.contains_key(&vtable_name) {
            self.error("Struct with that name already exists.");
        }

        let mut fields = HashMap::new();
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let mut offset = 0;
        self.custom_types.insert(
            vtable_name.clone(),
            CustomStruct {
                name: vtable_name.clone(),
                fields: fields.clone().into(),
                embed: None,
                methods: HashSet::new().into(),
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
            let field_type = self.parse_complex_type(&Some(vtable_name.clone()));

            // if the field is this struct throw an error
            let ValueType::Closure(params, 0) = &mut field_type.as_ref() else {
                self.error("interfaces may only contain functions");
                continue;
            };

            let new_field_type = ValueType::Closure(
                vec![
                    ValueType::Pointer(ValueType::Nil.intern(), false).intern(),
                    ValueType::Closure(params.clone(), 1).intern(),
                ]
                .into_boxed_slice(),
                0,
            );

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
                for (i, t) in params[0..params.len() - 1].iter().enumerate() {
                    s = format!("{s} a{i} : {}, ", t.to_string());
                }
                s = format!("{s}) -> {}{{\n", params.last().unwrap().to_string());
                s = format!(
                    "{s} return (self->vtable->{})(self->self)(",
                    field_name.lexeme.clone()
                );
                for (i, _) in params[0..params.len() - 1].iter().enumerate() {
                    s = format!("{s} a{i}, ");
                }
                s = format!("{s});\n}}");
                s
            };

            methods.push((field_name.lexeme.clone(), method_string.into()));

            offset += new_field_type.num_words();
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
            fields: HashMap::from([
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
            })
            .collect()
    }

    fn array_declaration(&mut self, var_name: SharedString, var_type: UValueType) -> ASTNode {
        let mut init = Vec::new();
        let line_no = self.previous.line;
        match self.parse_literal_index() {
            Ok(n) => {
                self.consume(TokenType::RightBracket, "brace");

                init = self.define_array(Some(n));
            }
            Err(e) => self.error(&e.to_string()),
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );
        ASTNode::Declaration(Declaration::Array(ArrayDeclaration {
            elements: init,
            name: var_name,
            elem_tipe: var_type.into(),

            line_no,
        }))
    }

    fn define_array(&mut self, size: Option<u32>) -> Vec<Expression> {
        let mut init = Vec::new();
        if self.match_token(TokenType::Equal) || self.current.token_type == TokenType::LeftBrace {
            self.consume(TokenType::LeftBrace, "Expect '{' after array declaration.");
            if self.current.token_type != TokenType::RightBrace {
                loop {
                    let node = self.expression();
                    init.push(node);
                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBrace, "Expect '}' after array declaration.");

            if let Some(n) = size {
                if (n as usize) != init.len() {
                    self.error(&format!(
                        "Array size mismatch, expected {} got {}",
                        n,
                        init.len()
                    ));
                }
            }
        } else if let Some(n) = size {
            init = vec![Expression::Empty; n as usize];
        }

        init
    }

    fn var_declaration(&mut self, mutable: bool) -> ASTNode {
        let line_no = self.previous.line;

        self.consume(TokenType::Identifier, "Expect variable name.");
        let name = self.previous.lexeme.clone();

        let mut var_type = None;
        if self.match_token(TokenType::Colon) {
            var_type = self.parse_complex_type(&None).into();
            if self.match_token(TokenType::LeftBracket) {
                return self.array_declaration(name.clone(), var_type.unwrap());
            }
        }
        let mut initializer = None;
        if self.match_token(TokenType::Equal) {
            if self.current.token_type == TokenType::LeftBrace {
                let elems = self.define_array(None);
                return ASTNode::Declaration(Declaration::Array(ArrayDeclaration {
                    elements: elems,
                    name,
                    elem_tipe: var_type,
                    line_no,
                }));
            }
            if self.current.token_type == TokenType::Struct {
                let t = self.parse_complex_type(&None);
                let s = self.struct_initializer(t);
                initializer = Some(s);
            } else {
                let node = self.expression();
                initializer = Some(node);
            }
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

        ASTNode::Declaration(Declaration::Var(VarDeclaration {
            name,
            tipe: var_type,
            initializer: initializer.map(Box::new),
            mutable,
            line_no,
        }))
    }

    pub(super) fn struct_initializer(&mut self, t: UValueType) -> Expression {
        let line_no = self.previous.line;
        let ValueType::Struct(s) = t.as_ref() else {
            self.error("Expect struct type.");
            return Expression::Empty;
        };
        self.consume(TokenType::LeftBrace, "Expect '{' after struct name.");
        let mut fields = HashMap::new();
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
            let receiver_type = self.parse_complex_type(&None);
            self.consume(TokenType::RightParen, "Expect ')' after receiver.");
            let ValueType::Pointer(s, _) = receiver_type.as_ref() else {
                self.error("Receiver must be a struct pointer.");
                return ASTNode::Empty;
            };

            let ValueType::Struct(cs) = s.as_ref() else {
                self.error("Receiver must be a struct pointer.");
                return ASTNode::Empty;
            };

            self.consume(TokenType::Identifier, "Expect function name.");
            let name = self.previous.lexeme.clone();

            let new_name: SharedString = format!("{}_{}", s.unique_string(), name).into();

            cs.methods.borrow_mut().insert(new_name.clone());

            let inner_name = format!("{}_inner", new_name);
            let mut node = self.function(&inner_name);
            node.prototype.captures.push(receiver.clone());

            let mut node_type = vec![];
            for (_name, t, _) in node.prototype.params.iter() {
                node_type.push(t.clone());
            }
            node_type.push(node.prototype.return_type.clone());

            return ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
                prototype: Prototype {
                    params: vec![(receiver, receiver_type, false)],
                    return_type: ValueType::Closure(node_type.into_boxed_slice(), 1).intern(),
                    name: new_name,
                    captures: vec![],
                    line_no,
                },
                body: vec![ASTNode::Statement(Statement::Return(ReturnStatement {
                    value: Some(Box::new(Expression::Function(node.clone()))),
                    line_no,
                }))]
                .into(),

                line_no,
            }));
        }

        self.consume(TokenType::Identifier, "Expect function name.");
        let name = self.previous.lexeme.clone();
        let node = self.function(&name);

        ASTNode::Declaration(Declaration::Function(FunctionDeclaration {
            prototype: node.prototype,
            body: node.body.into(),
            line_no,
        }))
    }
}
