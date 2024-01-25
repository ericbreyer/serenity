use std::{env::args, io::Write};

pub mod common;
pub mod chunk;
pub mod value;
pub mod vm;
pub mod compiler;
pub mod scanner;
#[cfg(test)]
mod end_to_end_tests;

fn repl(mut vm: vm::VM) {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        std::io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        vm.interpret(line);
    }
}

fn run_file(mut vm: vm::VM, path: String) {
    let source = std::fs::read_to_string(path).expect("Failed to read file");
    vm.interpret(source);
}

fn main() {
    let vm = vm::VM::new(false);
    if args().len()  ==1 {
        repl(vm);
    } else if args().len() == 2 {
        run_file(vm, args().nth(1).unwrap());
    } else {
        panic!("Usage: rlox [path]");
    }
}
