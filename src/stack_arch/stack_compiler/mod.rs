mod emit_pass;
mod type_check_pass;
// mod optimization_pass;

use std::collections::HashMap;

use emit_pass::EmitWalker;

use crate::{ common::{ CompileResult, ParseResult }, typing::UValueType };

use self::type_check_pass::TypeCheckWalker;

/// A macro to log an error message with the file and line number.
#[macro_export]
macro_rules! error {
    ($self:ident, $message:expr) => {
        $self.error($message, file!(), line!());
    };
}

pub fn compile(
    parsed: ParseResult,
    native_functions: &HashMap<SharedString, (usize, UValueType)>
) -> CompileResult {
    let type_checked = TypeCheckWalker::type_check(parsed, native_functions);
    // parsed = OptimizationWalker::optimize(parsed, native_functions);
    EmitWalker::emit(type_checked, native_functions)
}
