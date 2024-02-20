use crate::typing::CustomStruct;

pub mod ast;
pub mod dyn_tracing;
pub mod runnable;


pub struct ParseResult {
        pub ast: ast::ASTNode,
        pub custom_structs: std::collections::HashMap<String, CustomStruct>,
        pub had_errors: bool,
}

impl Default for ParseResult {
    fn default() -> Self {
        ParseResult {
            ast: ast::ASTNode::Err,
            custom_structs: std::collections::HashMap::new(),
            had_errors: false,
        }
    }
}

pub struct CompileResult {
    pub function: Option<runnable::Function>,
    pub static_data_segment: Vec<crate::value::Word>,
    pub function_segment: Vec<runnable::Runnable>,
}