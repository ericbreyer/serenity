use std::{env::args, io::Write};
// extern crate getopts;
use getopts::Options;
use tracing;
use tracing_subscriber;

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
        std::io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        vm.interpret::<SerenityParser>(line);
    }
}

fn run_file(mut vm: vm::VM, path: String) {
    let source = std::fs::read_to_string(path).expect("Failed to read file");
    vm.interpret::<SerenityParser>(source);
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

    let outfile = std::fs::File::create("output.txt")?;

    let (non_blocking, _guard) = tracing_appender::non_blocking(outfile);

    let subscriber = tracing_subscriber::fmt()
        .with_max_level(match (verbose, super_verbose) {
            (true, _) => tracing::Level::DEBUG,
            (_, true) => tracing::Level::TRACE,
            _ => tracing::Level::INFO,
        })
        
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::ACTIVE)
        .without_time()
        .with_writer(non_blocking)
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let vm = vm::VM::new();
    if matches.free.len() == 0 {
        repl(vm);
    } else if matches.free.len() == 1 {
        run_file(vm, matches.free[0].clone());
    } else {
        panic!("Usage: {} [path]", prog);
    }
    Ok(())
}
