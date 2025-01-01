use crate::chunk::Chunk;
use crate::opcodes;
use crate::scanner::{ScanError, Scanner};
use crate::token::{TokenType, Token};

struct Parser{
    source: String,  // Parser explicitly owns the source string
    scanner: Scanner,
    current: Token, 
    previous: Token,
    has_error: bool,
    panic_mode: bool,
}

impl Parser {
    fn new(source: String) -> Self {
        Self {
            source,
            scanner: Scanner::new(),
            current: Token {
                token_type: TokenType::Error,
                lexeme: "".to_string(),
                line: 0,
            },
            previous: Token {
                token_type: TokenType::Error,
                lexeme: "".to_string(),
                line: 0,
            },
            has_error: false, 
            panic_mode: false,
        }
    }

    fn advance(&mut self) {
        // TODO find a better way to do this 
        self.previous = self.current.clone();

        match self.scanner.scan_token(&self.source) {
            Ok(token) => {
                self.current = token;
            }
            Err(e) => {
                if !self.panic_mode {
                    self.panic_mode = true;
                    self.has_error = true;
                    e.print_error();
                }
            }
        }
    }

    // Attempt to consume a token of a specific type, emitting an error
    // message if a different token is encountered
    fn consume(&mut self, token_type: TokenType, msg: String) {
        if self.current.token_type == token_type {
            self.advance();
        } else {
            self.error_on_current(msg);
        }
    }

    fn error_on_prev(&mut self, msg: String) {
        if self.panic_mode {
            return
        }
        self.panic_mode = true;
        self.has_error = true; 

        self.previous.error(msg);
    }

    fn error_on_current(&mut self, msg: String) {
        if self.panic_mode {
            return
        }
        self.panic_mode = true;
        self.has_error = true; 

        self.current.error(msg);
    }
    
}

pub struct Compiler{

}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, source: String) -> Option<Chunk> {
        let mut parser = Parser::new(source);
        let mut chunk = Chunk::new();

        parser.advance();
        parser.consume(TokenType::Eof, "Expected end of expression".to_string());
        


        
        if parser.has_error {
            return None
        } else {
            return Some(chunk)
        }
    }

    fn emit_byte(&self, byte: u8, chunk: &mut Chunk, line: u32) {
        chunk.write(byte, line);
    }

    // Utility function for the common use case of writing an instruction
    // followed by a 1-byte operand
    fn emit_bytes(&self, byte1: u8, byte2: u8, chunk: &mut Chunk, line: u32) {
        chunk.write(byte1, line);
        chunk.write(byte2, line);
    }

    fn end(&self, chunk: &mut Chunk, line: u32) {
        self.emit_byte(opcodes::OP_RETURN, chunk, line);
    }
}