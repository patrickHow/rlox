use crate::scanner;
use crate::token;

pub struct Compiler {
    
}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, source: String) {

        let mut scanner = scanner::Scanner::new(source);

        // Dummy code for now 
        let mut line: u32 = 0;
        loop {
            let token = scanner.scan_token().unwrap();

            // Update line if scanner has advanced
            if token.line != line {
                print!("{} ", token.line);
                line = token.line;
            } else {
                print!("  | ");
            }

            println!("{:?} {}", token.token_type, token.lexeme);

            if token.token_type == token::TokenType::Eof {
                break;
            }
        }
    }
}