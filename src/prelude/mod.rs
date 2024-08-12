mod ast;
mod shared_strings;

pub use crate::{
    prelude::{ast::*, shared_strings::SharedString},
    typing::{CustomStruct, StructEntry, UValueType, ValueType},
    value::Value,
};
use std::fmt::Debug;

#[derive(Clone)]
pub struct ParseResult {
    pub ast: Vec<ast::ASTNode>,
    pub custom_structs: std::collections::HashMap<SharedString, CustomStruct>,
}

impl Default for ParseResult {
    fn default() -> Self {
        ParseResult {
            ast: vec![ast::ASTNode::Empty],
            custom_structs: std::collections::HashMap::new(),
        }
    }
}

impl Debug for ParseResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in &self.ast {
            write!(f, "{:?}", node)?;
        }
        Ok(())
    }
}
