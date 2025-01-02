use crate::chunk::Chunk;
use crate::opcodes::{self};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType, TOKEN_COUNT};
use crate::value::Value;
use core::panic;
use std::ops::Add;

const MAX_EXPRESSION_NESTING: u32 = 50;

// Operator precedence table, ordered from lowest to highest
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    None = 0,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Add<u8> for Precedence {
    type Output = Precedence;

    fn add(self, rhs: u8) -> Precedence {
        let result = (self as u8) + rhs;
        // Safety: ensure result maps to valid variant
        match result {
            x if x <= (Precedence::Primary as u8) => unsafe { std::mem::transmute(result) },
            _ => Precedence::Primary, // Or handle error case
        }
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Compiler, &mut Chunk, &mut Parser)>,
    infix: Option<fn(&mut Compiler, &mut Chunk, &mut Parser)>,
    precedence: Precedence,
}

impl ParseRule {
    const fn new(
        prefix: Option<fn(&mut Compiler, &mut Chunk, &mut Parser)>,
        infix: Option<fn(&mut Compiler, &mut Chunk, &mut Parser)>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

struct Parser {
    source: String, // Parser explicitly owns the source string
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
            return;
        }
        self.panic_mode = true;
        self.has_error = true;

        self.previous.error(msg);
    }

    fn error_on_current(&mut self, msg: String) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        self.has_error = true;

        self.current.error(msg);
    }
}

pub struct Compiler {
    expression_depth: u32, // Track the nesting depth of the expression being compiled
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            expression_depth: 0,
        }
    }

    pub fn compile(&mut self, source: String) -> Option<Chunk> {
        let mut parser = Parser::new(source);
        let mut chunk = Chunk::new();

        // Advance the parser to the first token
        parser.advance();
        self.expression(&mut parser, &mut chunk);
        parser.consume(TokenType::Eof, "Expected end of expression".to_string());

        self.end(&mut chunk, parser.previous.line);
        if parser.has_error {
            return None;
        } else {
            return Some(chunk);
        }
    }

    // Core function for compiling expressions - can be called recursively
    fn expression(&mut self, parser: &mut Parser, chunk: &mut Chunk) {
        self.expression_depth += 1;
        if self.expression_depth > MAX_EXPRESSION_NESTING {
            panic!("Expressions exceed maximum nesting, aborting compilation");
        }

        self.parse_precedence(parser, chunk, Precedence::Assignment);

        self.expression_depth -= 1;
    }

    // Starts at current token and parses any expression at the given precedence or higher
    fn parse_precedence(&mut self, parser: &mut Parser, chunk: &mut Chunk, precedence: Precedence) {
        // Read the next token and get the parse rule
        parser.advance();
        let rule = get_rule(parser.previous.token_type);

        // Since this is the first token, not having a prefix rule should throw a syntax error
        // Otherwise, call the function to parse the token
        match rule.prefix {
            None => {
                parser.previous.error("Expected expression".to_string());
                return;
            }
            Some(parse_fn) => {
                parse_fn(self, chunk, parser);
            }
        }

        // Now parse infix expressions
        while precedence <= get_rule(parser.current.token_type).precedence {
            parser.advance();
            let infix_rule = get_rule(parser.previous.token_type);
            match infix_rule.infix {
                None => {
                    parser.previous.error("No rule to parse token".to_string());
                    return;
                }
                Some(parse_fn) => {
                    parse_fn(self, chunk, parser);
                }
            }
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

    fn emit_constant(&self, value: Value, chunk: &mut Chunk, line: u32) {
        let ind = chunk.add_constant(value);
        if ind > u8::MAX {
            panic!("Error, too many constants in one chunk");
        }

        // Add the constant opcode and index to the chunk
        self.emit_bytes(opcodes::OP_CONSTANT, ind, chunk, line);
    }

    // Compiles a number literal
    // Assumes the token in previous contains the literal
    fn number(&mut self, chunk: &mut Chunk, parser: &mut Parser) {
        let value = Value::Double(parser.previous.lexeme.parse().unwrap()); // TODO add a better error
        self.emit_constant(value, chunk, parser.previous.line);
    }

    // Recursively compile a grouping within parentheses
    fn grouping(&mut self, chunk: &mut Chunk, parser: &mut Parser) {
        self.expression(parser, chunk);
        parser.consume(
            TokenType::RightParen,
            "Expected ')' after expression".to_string(),
        );
    }

    // Compiles a unary operation, such as negation
    fn unary(&mut self, chunk: &mut Chunk, parser: &mut Parser) {
        let tok_type = parser.previous.token_type;

        // Compile the expression
        self.parse_precedence(parser, chunk, Precedence::Unary);

        // Emit the instruction - will need to be extended for new unary ops
        // Note when we compile an expression like "!val" we need to put val on the stack FIRST
        // hence the call to expression() before we emit the OP_NEGATE
        self.emit_byte(opcodes::OP_NEGATE, chunk, parser.previous.line);
    }

    fn binary(&mut self, chunk: &mut Chunk, parser: &mut Parser) {
        let op_type = parser.previous.token_type;
        let rule = get_rule(op_type);
        // Parse everything to the right of the token with precedence higher than the current op
        self.parse_precedence(parser, chunk, rule.precedence + 1);

        // Emit the correct byte based on the op
        match op_type {
            TokenType::Plus => self.emit_byte(opcodes::OP_ADD, chunk, parser.previous.line),
            TokenType::Minus => self.emit_byte(opcodes::OP_SUBTRACT, chunk, parser.previous.line),
            TokenType::Star => self.emit_byte(opcodes::OP_MULTIPLY, chunk, parser.previous.line),
            TokenType::FSlash => self.emit_byte(opcodes::OP_DIVIDE, chunk, parser.previous.line),
            _ => panic!("Invalid binary op type"),
        }
    }
}

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    // Static array is initialized at compile time
    static RULES: [ParseRule; TOKEN_COUNT] = [
        // Single-character tokens
        ParseRule::new(Some(Compiler::grouping), None, Precedence::None), // LeftParen
        ParseRule::new(None, None, Precedence::None),                     // RightParen
        ParseRule::new(None, None, Precedence::None),                     // LeftBrace
        ParseRule::new(None, None, Precedence::None),                     // RightBrace
        ParseRule::new(None, None, Precedence::None),                     // Comma
        ParseRule::new(None, None, Precedence::None),                     // Dot
        ParseRule::new(
            Some(Compiler::unary),
            Some(Compiler::binary),
            Precedence::Term,
        ), // Minus
        ParseRule::new(None, Some(Compiler::binary), Precedence::Term),   // Plus
        ParseRule::new(None, None, Precedence::None),                     // Semicolon
        ParseRule::new(None, Some(Compiler::binary), Precedence::Factor), // FSlash
        ParseRule::new(None, Some(Compiler::binary), Precedence::Factor), // Star
        // One or two character tokens
        ParseRule::new(None, None, Precedence::None), // Bang
        ParseRule::new(None, None, Precedence::None), // BangEqual
        ParseRule::new(None, None, Precedence::None), // Equal
        ParseRule::new(None, None, Precedence::None), // EqualEqual
        ParseRule::new(None, None, Precedence::None), // Greater
        ParseRule::new(None, None, Precedence::None), // GreaterEqual
        ParseRule::new(None, None, Precedence::None), // Less
        ParseRule::new(None, None, Precedence::None), // LessEqual
        // Literals
        ParseRule::new(None, None, Precedence::None), // Identifier
        ParseRule::new(None, None, Precedence::None), // String
        ParseRule::new(Some(Compiler::number), None, Precedence::None), // Number
        // Keywords
        ParseRule::new(None, None, Precedence::None), // And
        ParseRule::new(None, None, Precedence::None), // Class
        ParseRule::new(None, None, Precedence::None), // Else
        ParseRule::new(None, None, Precedence::None), // False
        ParseRule::new(None, None, Precedence::None), // For
        ParseRule::new(None, None, Precedence::None), // Fun
        ParseRule::new(None, None, Precedence::None), // If
        ParseRule::new(None, None, Precedence::None), // Nil
        ParseRule::new(None, None, Precedence::None), // Or
        ParseRule::new(None, None, Precedence::None), // Print
        ParseRule::new(None, None, Precedence::None), // Return
        ParseRule::new(None, None, Precedence::None), // Super
        ParseRule::new(None, None, Precedence::None), // This
        ParseRule::new(None, None, Precedence::None), // True
        ParseRule::new(None, None, Precedence::None), // Var
        ParseRule::new(None, None, Precedence::None), // While
        // Special tokens
        ParseRule::new(None, None, Precedence::None), // Error
        ParseRule::new(None, None, Precedence::None), // Eof
    ];
    &RULES[token_type as usize]
}
