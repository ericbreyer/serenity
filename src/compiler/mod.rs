mod emit_pass;
mod type_check_pass;
mod optimization_pass;

use std::collections::HashMap;

use emit_pass::EmitWalker;

use crate::{ common::{ CompileResult, ParseResult }, typing::ValueType };

use self::type_check_pass::TypeCheckWalker;

#[macro_export]
macro_rules! error {
    ($self:ident, $message:expr) => {
        $self.error($message, file!(), line!());
    };
}

pub fn compile(
    mut parsed: ParseResult,
    native_functions: &HashMap<String, (usize, ValueType)>
) -> CompileResult {
    parsed = TypeCheckWalker::type_check(parsed, native_functions);
    parsed = optimization_pass::OptimizationWalker::optimize(parsed, native_functions);
    return EmitWalker::emit(parsed, native_functions);
}
