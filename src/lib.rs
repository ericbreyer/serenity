// //! Serenity is a toy programming language written from scratch by Eric Breyer
// //!
// //! It is heavily inspired by C, with some QOL features like closures,
// //! and struct methods. I originally followed the [`Crafting Interpreters`] book
// //! by Bob Nystrom, but have since diverged from the book's implementation.
// //!
// //! Serenity has seperate lexing, parsing, and compiling phases, and currently
// //! uses a two pass compiler to statically type check the program.
// //!
// //! Serenity currently runs on a stack based virtual machine, but I plan to
// //! implement an LLVM backend in the future.
// //!
// //! [`Crafting Interpreters`]: https://craftinginterpreters.com/
// //!

// #![warn(clippy::pedantic)]
// #![warn(missing_docs)]
// #![warn(clippy::missing_docs_in_private_items)]

// // extern crate getopts;

use parser::Parser;
// use reg_vm::ExitReason;
use tracing::{level_filters::LevelFilter, subscriber::DefaultGuard};
use tracing_appender::non_blocking::WorkerGuard;
use tracing_subscriber::{
    self,
    filter::FilterFn,
    fmt::format::FmtSpan,
    layer::{Filter, SubscriberExt},
    Layer,
};
use anyhow::{Context, Result};

mod lexer;
mod prelude;
mod parser;
// // mod reg_compiler;
mod value;
// // mod reg_vm;
mod typing;
mod llvm_compiler;

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

    let outfile = std::fs::File::create("output.ansi").context("While creating debug output file")?;

    let (non_blocking, _guard) = tracing_appender::non_blocking(outfile);

    let file_trace = tracing_subscriber::fmt::layer()
        .with_span_events(FmtSpan::ACTIVE)
        .without_time()
        // .with_ansi(false)
        .with_writer(non_blocking)
        .with_filter(match verbose {
            1 => LevelOrFn::Level(tracing_subscriber::filter::LevelFilter::DEBUG),
            n if n > 1 => LevelOrFn::Level(tracing_subscriber::filter::LevelFilter::TRACE),
            _ => LevelOrFn::Fn(tracing_subscriber::filter::filter_fn(|_| false)),
        });

    let err_trace = tracing_subscriber::fmt::layer()
        .without_time()
        .with_writer(std::io::stderr)
        .with_filter(match verbose {
            n if n > 1 => tracing_subscriber::filter::LevelFilter::DEBUG,
            _ => tracing_subscriber::filter::LevelFilter::INFO,
        });

    let subscriber = tracing_subscriber::registry()
        .with(file_trace)
        .with(err_trace);

    Ok((tracing::subscriber::set_default(subscriber), _guard))
}

pub fn run_file(path: &str, _output: &mut impl std::io::Write) -> Result<(), std::io::Error> {

    let source = std::fs::read_to_string(path).expect("Failed to read file");

    let parsed = parser::SerenityParser::parse(source.into(), path.into());

    let context = inkwell::context::Context::create();
    let module = llvm_compiler::compile(&context, parsed.expect("Failed to parse file")).map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))?;

    module.print_to_file("output.ll").expect("Failed to write to file");

    Ok(())
}
