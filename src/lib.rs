//! Serenity is a toy programming language written from scratch by Eric Breyer
//!
//! It is heavily inspired by C, with some QOL features like closures,
//! and struct methods. I originally followed the [`Crafting Interpreters`] book
//! by Bob Nystrom, but have since diverged from the book's implementation.
//!
//! Serenity has seperate lexing, parsing, and compiling phases, and currently
//! uses a two pass compiler to statically type check the program.
//!
//! Serenity currently runs on a stack based virtual machine, but I plan to
//! implement an LLVM backend in the future.
//!
//! [`Crafting Interpreters`]: https://craftinginterpreters.com/

// #![warn(clippy::too_many_lines)]

use anyhow::{Context, Result};
use tracing::{info, level_filters::LevelFilter, subscriber::DefaultGuard};
use tracing_appender::non_blocking::WorkerGuard;
use tracing_subscriber::{
    self,
    filter::FilterFn,
    fmt::format::FmtSpan,
    layer::{Filter, SubscriberExt},
    Layer,
};

mod lexer;
use lexer::Lexer;
mod parser;
use parser::{Parser, SerenityParser};
mod compiler;
mod prelude;
mod typing;
mod value_literals;

pub use typing::ValueType;

enum LevelOrFn {
    Level(LevelFilter),
    Fn(FilterFn),
}

impl<S> Filter<S> for LevelOrFn {
    fn enabled(
        &self,
        meta: &tracing::Metadata<'_>,
        cx: &tracing_subscriber::layer::Context<'_, S>,
    ) -> bool {
        match self {
            LevelOrFn::Level(l) => tracing_subscriber::layer::Filter::enabled(l, meta, cx),
            LevelOrFn::Fn(f) => tracing_subscriber::layer::Filter::enabled(f, meta, cx),
        }
    }
}

pub fn set_log_verbosity(verbose: usize) -> Result<(DefaultGuard, WorkerGuard)> {
    let outfile =
        std::fs::File::create("output.ansi").context("While creating debug output file")?;

    let (non_blocking, g) = tracing_appender::non_blocking(outfile);

    let file_trace = tracing_subscriber::fmt::layer()
        .with_span_events(FmtSpan::ACTIVE)
        // .without_time()
        // .with_ansi(false)
        .with_writer(non_blocking)
        .with_filter(match verbose {
            1 => LevelOrFn::Level(tracing_subscriber::filter::LevelFilter::DEBUG),
            n if n > 1 => LevelOrFn::Level(tracing_subscriber::filter::LevelFilter::TRACE),
            _ => LevelOrFn::Fn(tracing_subscriber::filter::filter_fn(|_| false)),
        });

    let err_trace = tracing_subscriber::fmt::layer()
        // .without_time()
        .with_writer(std::io::stderr)
        .with_filter(match verbose {
            n if n > 1 => tracing_subscriber::filter::LevelFilter::DEBUG,
            _ => tracing_subscriber::filter::LevelFilter::INFO,
        });

    let subscriber = tracing_subscriber::registry()
        .with(file_trace)
        .with(err_trace);

    Ok((tracing::subscriber::set_default(subscriber), g))
}

pub fn scan(path: &str) -> Result<Vec<String>> {
    let source = std::fs::read_to_string(path).context("Failed to read file")?;
    let mut lexer = Lexer::new(source.into());
    let mut tokens = Vec::new();
    loop {
        let token = lexer.scan_token();
        let eof = token.is_eof();
        tokens.push(token);
        if eof {
            break;
        }
    }
    Ok(tokens.into_iter().map(|t| format!("{:?}", t)).collect())
}

pub fn parse(path: &str, include_paths: Vec<String>) -> Result<String> {
    let source = std::fs::read_to_string(path).context("Failed to read file")?;
    let parser = SerenityParser::parse(source.into(), path.into(), include_paths)
        .context("Failed to parse file")?;
    let ast = parser.ast;
    Ok(format!("{:?}", ast))
}

pub fn compile(path: &str, include_paths: Vec<String>) -> Result<String> {
    let source = std::fs::read_to_string(path).context("Failed to read file")?;
    let parsed = parser::SerenityParser::parse(source.into(), path.into(), include_paths)
        .context("Failed to parse file")?;
    let context = inkwell::context::Context::create();
    let module = compiler::compile(&context, parsed)?;
    module
        .verify()
        .map_err(|s| anyhow::Error::msg(s.to_string()))
        .context("Failed to verify module")?;
    Ok(module.print_to_string().to_string())
}

pub fn typecheck(path: &str, include_paths: Vec<String>) -> Result<String> {
    let source = std::fs::read_to_string(path).context("Failed to read file")?;
    let parsed = parser::SerenityParser::parse(source.into(), path.into(), include_paths)
        .context("Failed to parse file")?;
    println!("{:?}", parsed.ast);
    let ast = compiler::typecheck(parsed)?;

    Ok(format!("{:?}", ast))
}

pub fn run_file(
    path: &str,
    include_paths: Vec<String>,
    output: &mut impl std::io::Write,
) -> Result<i64> {
    let source = std::fs::read_to_string(path).context("Failed to read file")?;

    let parsed = parser::SerenityParser::parse(source.into(), path.into(), include_paths)
        .context("Failed to parse file")?;

    let context = inkwell::context::Context::create();
    let module = compiler::compile(&context, parsed).context("Failed to compile")?;

    module
        .verify()
        .map_err(|s| anyhow::Error::msg(s.to_string()))
        .context("Failed to verify module")?;

    let engine = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .map_err(|e| anyhow::Error::msg(e.to_string()))
        .context("Failed to create execution engine")?;

    info!("Execution engine created");

    let out = unsafe {
        let main = engine
            .get_function::<unsafe extern "C" fn() -> i64>("main_i64")
            .context("Failed to get main function")?;

        info!("Calling main function");

        let out = main.call();

        info!("Exited with code {}", out);
        writeln!(output, "Exited with code {}", out)?;
        out
    };

    Ok(out as i64)
}

#[cfg(test)]
mod tests {
    use std::{
        fs,
        io::Cursor,
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    };

    use super::*;

    fn temp_file(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        path.push(format!("serenity_{}_{}.ser", name, nanos));
        path
    }

    fn write_program(contents: &str) -> PathBuf {
        let path = temp_file("prog");
        fs::write(&path, contents).unwrap();
        path
    }

    #[test]
    fn test_set_log_verbosity_levels() {
        // Ensure all verbosity levels initialize without panic
        let _ = set_log_verbosity(0).unwrap();
        let _ = set_log_verbosity(1).unwrap();
        let _ = set_log_verbosity(2).unwrap();
    }

    #[test]
    fn test_scan_and_parse_roundtrip() {
        let program = r#"
            fn main() -> int {
                return 1 + 2;
            }
        "#;
        let path = write_program(program);

        let tokens = scan(path.to_str().unwrap()).unwrap();
        assert!(tokens.len() > 2);

        let parsed = parse(path.to_str().unwrap(), vec![]).unwrap();
        assert!(!parsed.is_empty());
    }

    #[test]
    fn test_compile_and_run_file() {
        let program = r#"
            fn main() -> int {
                return 42;
            }
        "#;
        let path = write_program(program);

        let module_ir = compile(path.to_str().unwrap(), vec![]).unwrap();
        assert!(module_ir.contains("main"));

        let mut output = Cursor::new(Vec::new());
        let result = run_file(path.to_str().unwrap(), vec![], &mut output).unwrap();
        assert_eq!(result, 42);
        assert!(String::from_utf8(output.into_inner())
            .unwrap()
            .contains("Exited with code 42"));
    }

    #[test]
    fn test_typecheck_smoke() {
        let program = r#"
            fn id(x: int) -> int { return x; }
            fn main() -> int { return id(7); }
        "#;
        let path = write_program(program);

        let typed = typecheck(path.to_str().unwrap(), vec![]).unwrap();
        assert!(!typed.is_empty());
        assert!(typed.contains("Function"));
    }
}
