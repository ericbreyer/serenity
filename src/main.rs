use std::{ env::args, io::Write };
// extern crate getopts;
use getopts::Options;
use tracing;
use tracing_subscriber::{ self, fmt::format::FmtSpan, layer::SubscriberExt, Layer };

use crate::parser::SerenityParser;

pub mod chunk;
pub mod common;
pub mod compiler;
#[cfg(test)]
mod end_to_end_tests;
pub mod parser;
pub mod lexer;
pub mod value;
pub mod vm;
pub mod typing;

fn repl(mut vm: vm::VM) {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).expect("Failed to read line");
        vm.interpret::<SerenityParser>(line);
    }
}

fn run_file(mut vm: vm::VM, path: String) -> Result<(), std::io::Error>{
    let source = std::fs::read_to_string(path).expect("Failed to read file");
    let result = vm.interpret::<SerenityParser>(source);
    match result {
        vm::InterpretResult::Ok => Ok(()),
        vm::InterpretResult::CompileError => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Compile error")),
        vm::InterpretResult::RuntimeError => Err(std::io::Error::new(std::io::ErrorKind::Interrupted, "Runtime error"))
    }
}

fn main() -> std::io::Result<()> {
    // usage
    // serenity - REPL
    // serenity [path] - run file
    // optional -v - verbose

    let args: Vec<String> = args().collect();
    let prog = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("v", "verbose", "print extra info");
    opts.optflag("", "vv", "print even more info");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!("{}", f.to_string()),
    };

    let verbose = matches.opt_present("v");
    let super_verbose = matches.opt_present("vv");

    let outfile = std::fs::File::create("output.ansi")?;

    let (non_blocking, _guard) = tracing_appender::non_blocking(outfile);

    let file_trace = tracing_subscriber::fmt
        ::layer()
        .with_span_events(FmtSpan::ACTIVE)
        .without_time().with_ansi(false)
        .with_writer(non_blocking)
        .with_filter(match (verbose, super_verbose) {
            (true, _) => tracing_subscriber::filter::LevelFilter::DEBUG,
            (_, true) => tracing_subscriber::filter::LevelFilter::TRACE,
            _ => tracing_subscriber::filter::LevelFilter::INFO,
        });

    let err_trace = tracing_subscriber::fmt
        ::layer()
        // .with_span_events(FmtSpan::ACTIVE)
        .without_time()
        .with_writer(std::io::stderr)
        .with_filter(tracing_subscriber::filter::LevelFilter::INFO);

    let subscriber = tracing_subscriber::registry().with(file_trace).with(err_trace);

    // subscriber.downcast_ref().unwrap()

    tracing::subscriber::set_global_default(subscriber).expect("Failed to set subscriber");

    let vm = vm::VM::new();
    if matches.free.len() == 0 {
        repl(vm);
        Ok(())
    } else if matches.free.len() == 1 {
        return run_file(vm, matches.free[0].clone());
    } else {
        // panic!("Usage: {} [path]", prog);
        Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Usage: serenity [path]"))
    }
}
