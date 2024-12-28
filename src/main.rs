use std::io::{self};
use std::{env, fs};

// Modules within this package
mod chunk;
mod vm;
mod scanner;
mod compiler;
mod opcodes;
mod token;


// Top-level struct to tie our pipeline together
// Scanner->Compiler->VM
// Note that the scanner is part of the compiler
struct Interpreter {
    compiler: compiler::Compiler,
    vm: vm::VM,
}

impl Interpreter {

    fn new() -> Self {
        Self {
            compiler: compiler::Compiler::new(),
            vm: vm::VM::new(),
        }
    }

    fn interpret(&mut self, source: String) -> vm::InterpretResult {
        self.compiler.compile(source);
        return vm::InterpretResult::OK;
    }

    fn repl(&mut self) {
        loop {
            print!("> ");
    
            let mut line = String::new();
            io::stdin().read_line(&mut line).unwrap();
    
            self.interpret(line);
        }
    }
    
    fn run_file(&mut self, filepath: &String) {
        let source = fs::read_to_string(filepath).expect("Error reading file");
        self.interpret(source);
    }


}

fn main() {

    let mut interpreter = Interpreter::new();

    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        // No filepath given, start the repl
        interpreter.repl();
    } else if args.len() == 2 {
        // Assume we have a filepath to process
        let filepath = &args[1];
        interpreter.run_file(filepath);
    } else {
        println!("Usage: rlox [path]")
    }
}
