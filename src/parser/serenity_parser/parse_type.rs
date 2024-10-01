use crate::typing::{Closure, UValueType};

use super::*;

impl SerenityParser {
    pub fn parse_type(
        &mut self,
        struct_name: Option<&SharedString>,
        type_params: Option<&Vec<SharedString>>,
    ) -> UValueType {
        let leading_star = self.match_token(TokenType::Star);

        let parsed_type = if leading_star {
            self.parse_type(struct_name, type_params)
        } else {
            self.parse_complex_type(struct_name, type_params)
        };

        let trailing_star = self.match_token(TokenType::Star);

        if trailing_star || leading_star {
            if trailing_star == leading_star {
                self.error("Cannot have both leading and trailing '*'.");
                return ValueType::Err.intern();
            }

            return ValueType::Pointer(parsed_type, true).intern();
        }

        parsed_type
    }

    fn parse_complex_type(
        &mut self,
        struct_name: Option<&SharedString>,
        type_params: Option<&Vec<SharedString>>,
    ) -> UValueType {
        if self.match_token(TokenType::LeftParen) {
            let t = self.parse_complex_type(struct_name, type_params);
            self.consume(TokenType::RightParen, "Expect ')' after type.");
            return t;
        }

        if self.match_token(TokenType::LeftBracket) {
            let t = self.parse_complex_type(struct_name, type_params);
            let mut index = None;
            if self.match_token(TokenType::Semicolon) {
                let sindex = self.expression().eval_constexpr();
                index = Some(match sindex {
                    Some(Value::UInteger(index)) => index,
                    Some(Value::Integer(index)) if index >= 0 => index as u64,
                    _ => {
                        self.error("Expect constant unsigned index.");
                        return ValueType::Err.intern();
                    }
                } as usize);
            }

            self.consume(TokenType::RightBracket, "Expect ']' after index.");
            return ValueType::Array(t, index).intern();
        }

        if self.match_token(TokenType::Identifier) {
            let name = self.previous.lexeme.clone();
            if let Some(s) = type_params.and_then(|tp| tp.contains(&name).then(|| name.clone())) {
                return ValueType::GenericParam(s.clone()).intern();
            }
        }

        if self.match_token(TokenType::SimpleType) {
            return ValueType::from(self.previous.lexeme.clone()).intern();
        }

        if self.match_token(TokenType::Fun) {
            return self.parse_function_type(struct_name, type_params);
        }

        if self.match_token(TokenType::Struct) {
            return self.parse_struct_type(struct_name, type_params);
        }

        if self.match_token(TokenType::Impl) {
            self.consume(TokenType::Identifier, "Expect interface name.");
            let name: SharedString = format!("{}_impl", self.previous.lexeme).into();

            if struct_name.is_some_and(|s| *s == name) {
                return ValueType::SelfStruct(name, vec![]).intern();
            }
            if let Some(s) = self.custom_types.get(&name) {
                return ValueType::Struct(s.clone()).intern();
            }
        }

        self.error("Expect type.");
        ValueType::Err.intern()
    }

    fn parse_struct_type(
        &mut self,
        struct_name: Option<&SharedString>,
        type_params: Option<&Vec<SharedString>>,
    ) -> UValueType {
        self.consume(TokenType::Identifier, "Expect struct name.");
        let name = self.previous.lexeme.clone();

        // if this struct is the same as the current struct, return a self type
        if struct_name.is_some_and(|s| *s == name) {
            let tps = if self.match_token(TokenType::Less) {
                // if the struct has type parameters, parse them
                let mut params = Vec::new();
                loop {
                    let p_type = self.parse_type(struct_name, type_params);
                    params.push(p_type);
                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                }
                self.consume(TokenType::Greater, "Expect '>' after type parameters.");
                params
            } else {
                // if the struct has no type parameters, return an empty vec as the type parameters
                Vec::new()
            };

            return ValueType::SelfStruct(name, tps).intern();
        }

        // if the struct is a valid custom struct, return the struct type
        if let Some(s) = self.custom_types.get(&name).cloned() {
            // if the struct has type parameter bindings, parse them
            let mut tps = if self.match_token(TokenType::Less) {
                let mut params = Vec::new();
                loop {
                    let p_type = self.parse_type(struct_name, type_params);
                    params.push(p_type);
                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                }
                self.consume(TokenType::Greater, "Expect '>' after type parameters.");
                if params.len() != s.type_vars.borrow().len() {
                    self.error("Expect same number of type parameters.");
                    return ValueType::Err.intern();
                }
                let mut type_params = HashMap::new();
                for (name, p_type) in s.type_vars.borrow().iter().zip(params) {
                    type_params.insert(name.clone(), p_type);
                }
                type_params
            } else {
                // if the struct has no type parameters, return an empty hashmap as the type parameters
                HashMap::new()
            };
            return ValueType::Struct(s.clone()).instantiate_generic(&mut tps);
        }
        self.error("Unknown struct type.");
        ValueType::Err.intern()
    }

    fn parse_function_type(
        &mut self,
        struct_name: Option<&SharedString>,
        type_params: Option<&Vec<SharedString>>,
    ) -> UValueType {
        let mut param_types = Vec::new();

        let mut captures = Vec::new();
        if self.match_token(TokenType::LeftBracket) {
            loop {
                let p_type = self.parse_type(struct_name, type_params);
                captures.push(p_type);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
            self.consume(TokenType::RightBracket, "Expect ']' after captures.");
        }

        self.consume(TokenType::LeftParen, "Expect '(' after 'fun'.");
        if self.current.token_type != TokenType::RightParen {
            loop {
                let p_type = self.parse_type(struct_name, type_params);
                param_types.push(p_type);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        let mut return_type = ValueType::Nil.intern();
        if self.match_token(TokenType::RightArrow) {
            return_type = self.parse_type(struct_name, type_params);
        }

        return ValueType::Closure(Closure::new(
            param_types.as_slice().into(),
            captures.as_slice().into(),
            return_type,
            None,
        ))
        .intern()
        .instantiate_generic(&mut HashMap::new());
    }
}
