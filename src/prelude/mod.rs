mod ast;
mod scoped_map;
mod shared_strings;

pub use crate::{
    prelude::{ast::*, scoped_map::*, shared_strings::SharedString},
    typing::{CustomStruct, StructEntry, ValueType},
    value_literals::Value,
};
use std::fmt::Debug;

#[derive(Clone)]
pub struct ParseResult {
    pub ast: Ast,
    pub custom_structs: std::collections::HashMap<SharedString, CustomStruct>,
}

impl Default for ParseResult {
    fn default() -> Self {
        ParseResult {
            ast: Ast {
                roots: vec![ast::ASTNode::Empty],
            },
            custom_structs: std::collections::HashMap::new(),
        }
    }
}

impl Debug for ParseResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in &self.ast.roots {
            writeln!(f, "{:?}", node)?;
        }
        Ok(())
    }
}
