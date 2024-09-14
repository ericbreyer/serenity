use anyhow::Result;
use inkwell::{context::Context, module::Module};
use llvm_compiler::LLVMCompiler;
use tracing::info;
pub use typechecker::Typechecker;

use crate::prelude::ParseResult;

mod llvm_compiler;
pub mod typechecker;

mod ffi_funcs {
    use crate::typing::{UValueType, ValueType};

    pub struct FfiFunc<'a> {
        pub name: &'a str,
        pub ret: UValueType,
        pub args: Box<[UValueType]>,
        pub va: bool,
    }

    pub fn ffi_funcs() -> Box<[FfiFunc<'static>]> {
        Box::new([
            FfiFunc {
                name: "printf",
                ret: ValueType::Nil.intern(),
                args: Box::new([ValueType::Pointer(ValueType::Char.intern(), false).intern()]),
                va: true,
            },
            FfiFunc {
                name: "malloc",
                ret: ValueType::Pointer(ValueType::Nil.intern(), false).intern(),
                args: Box::new([ValueType::Integer.intern()]),
                va: false,
            },
            FfiFunc {
                name: "free",
                ret: ValueType::Nil.intern(),
                args: Box::new([ValueType::Pointer(ValueType::Nil.intern(), false).intern()]),
                va: false,
            },
        ])
    }
}

pub fn compile(context: &Context, pr: ParseResult) -> Result<Module<'_>> {
    let ffi = ffi_funcs::ffi_funcs();
    let typechecker = Typechecker::new(pr.custom_structs.clone(), ffi.as_ref());
    for ast in &pr.ast.roots {
        typechecker.compile(ast)?;
    }
    
    info!("Typechecking complete");

    let compiler = LLVMCompiler::new(context, pr.custom_structs, ffi.as_ref());
    for ast in pr.ast.roots {
        compiler.compile(&ast)?;
    }

    info!("Compilation complete");
    
    Ok(compiler.module())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Parser, SerenityParser};

    use inkwell::context::Context;
    use test_case::test_case;

    #[test_case(r##"
    fn main() -> int {
        let x;
        x = 2;
        return x;
    }"##, 2; "infer")]
    fn test_compile(input: &str, expected: usize) {
        let context = Context::create();

        let pr = SerenityParser::parse(input.into(), "test_compile".into()).unwrap();

        let result = compile(&context, pr);
        if result.is_err() {
            println!("{:?}", result);
            panic!("Failed to compile {:?}", result.err().unwrap());
        }

        unsafe {
            let jit = result
                .unwrap()
                .create_jit_execution_engine(inkwell::OptimizationLevel::None)
                .unwrap();
            let main = jit
                .get_function::<unsafe extern "C" fn() -> i64>("main_fn")
                .unwrap();
            let out = main.call();
            assert_eq!(out, expected as i64);
        }
    }
}
