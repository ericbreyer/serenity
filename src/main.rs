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
//!

#![warn(clippy::pedantic)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use std::env::args;
// extern crate getopts;
use getopts::Options;

use parser::Parser;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{
  self,
  filter::FilterFn,
  fmt::format::FmtSpan,
  layer::{ Filter, SubscriberExt },
  Layer,
};



mod chunk;
mod common;
mod compiler;
#[cfg(test)]
mod end_to_end_tests;
mod parser;
mod lexer;
mod value;
mod vm;
mod typing;

fn run_file(mut vm: vm::VM, path: String) -> Result<(), std::io::Error> {
  let source = std::fs::read_to_string(path.clone()).expect("Failed to read file");
  let parsed = parser::SerenityParser::parse(source, path);
  let result = vm.interpret(parsed);
  match result {
    vm::InterpretResult::Ok => Ok(()),
    vm::InterpretResult::CompileError =>
      Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Compile error")),
    vm::InterpretResult::RuntimeError =>
      Err(std::io::Error::new(std::io::ErrorKind::Interrupted, "Runtime error")),
  }
}

enum LevelOrFn {
  Level(LevelFilter),
  Fn(FilterFn),
}

impl<S> Filter<S> for LevelOrFn {
  fn enabled(
    &self,
    meta: &tracing::Metadata<'_>,
    cx: &tracing_subscriber::layer::Context<'_, S>
  ) -> bool {
    match self {
      LevelOrFn::Level(l) => tracing_subscriber::layer::Filter::enabled(l, meta, cx),
      LevelOrFn::Fn(f) => tracing_subscriber::layer::Filter::enabled(f, meta, cx),
    }
  }
}

fn usage(prog: &str, opts: &Options) {
  let req = format!("{prog} path");
  let brief = opts.short_usage(&req);
  print!("{}", opts.usage(&brief));
}
fn main() -> std::io::Result<()> {
  // usage
  // serenity [path] - run file
  // optional -v - verbose

  let args: Vec<String> = args().collect();
  let prog = args[0].clone();

  let mut opts = Options::new();
  opts.optopt("v", "verbose", "enable verbose output", "LEVEL");
  opts.optflag("h", "help", "print this help menu");

  let matches = match opts.parse(&args[1..]) {
    Ok(m) => m,
    Err(f) => {
      usage(&prog, &opts);
      panic!("{}", f.to_string())
    }
  };

  if matches.opt_present("h") {
    usage(&prog, &opts);
    return Ok(());
  }

  let verbose = matches.opt_get_default("v", 0).unwrap_or(0);

  let outfile = std::fs::File::create("output.ansi")?;

  let (non_blocking, _guard) = tracing_appender::non_blocking(outfile);

  let file_trace = tracing_subscriber::fmt
    ::layer()
    .with_span_events(FmtSpan::ACTIVE)
    .without_time()
    .with_ansi(false)
    .with_writer(non_blocking)
    .with_filter(match verbose {
      1 => LevelOrFn::Level(tracing_subscriber::filter::LevelFilter::DEBUG),
      n if n > 1 => LevelOrFn::Level(tracing_subscriber::filter::LevelFilter::TRACE),
      _ => LevelOrFn::Fn(tracing_subscriber::filter::filter_fn(|_| false)),
    });

  let err_trace = tracing_subscriber::fmt
    ::layer()
    .without_time()
    .with_writer(std::io::stderr)
    .with_filter(tracing_subscriber::filter::LevelFilter::INFO);

  let subscriber = tracing_subscriber::registry().with(file_trace).with(err_trace);

  // subscriber.downcast_ref().unwrap()

  tracing::subscriber::set_global_default(subscriber).expect("Failed to set subscriber");

  let vm = vm::VM::new();
  if matches.free.len() == 1 {
    run_file(vm, matches.free[0].clone())
  } else {
    println!("Usage: {prog} path [--v[v]]");
    Err(
      std::io::Error::new(std::io::ErrorKind::InvalidInput, format!("Usage: {prog} path [--v[v]]"))
    )
  }
}
