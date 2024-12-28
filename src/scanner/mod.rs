use std::iter::Scan;

use crate::token::{TokenType, Token};

pub struct Scanner {
    source: String,
    start: usize, // Start of current token
    current: usize, // Current character being examined within token
    line: u32,  // Current line number
}

// Result<> error values for scanning
#[derive(Debug)]
pub enum ScanError {
    InvalidCharacter(char), 
    UnterminatedString, 
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    // Scan and return the next token from the source
    pub fn scan_token(&mut self) -> Result<Token, ScanError> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof));
        }

        // Scan the next character
        let c = self.advance();
        match c {
            // Single character tokens first
            '(' => Ok(self.make_token(TokenType::LeftParen)),
            ')' => Ok(self.make_token(TokenType::RightParen)),
            '{' => Ok(self.make_token(TokenType::LeftBrace)),
            '}' => Ok(self.make_token(TokenType::RightBrace)),
            ',' => Ok(self.make_token(TokenType::Comma)),
            ';' => Ok(self.make_token(TokenType::Semicolon)),
            '.' => Ok(self.make_token(TokenType::Dot)),
            '+' => Ok(self.make_token(TokenType::Plus)),
            '-' => Ok(self.make_token(TokenType::Minus)),
            '*' => Ok(self.make_token(TokenType::Star)),
            '/' => Ok(self.make_token(TokenType::FSlash)),

            // Characters that MAY be two characters
            '!' => {
                if self.match_next('=') {
                    Ok(self.make_token(TokenType::BangEqual))
                } else {
                    Ok(self.make_token(TokenType::Bang))
                }
            },

            '=' => {
                if self.match_next('=') {
                    Ok(self.make_token(TokenType::EqualEqual))
                } else {
                    Ok(self.make_token(TokenType::Equal))
                }
            },

            '<' => {
                if self.match_next('=') {
                    Ok(self.make_token(TokenType::LessEqual))
                } else {
                    Ok(self.make_token(TokenType::Less))
                }
            },

            '>' => {
                if self.match_next('=') {
                    Ok(self.make_token(TokenType::GreaterEqual))
                } else {
                    Ok(self.make_token(TokenType::Greater))
                }
            },
            // A double quote should indicate the start of a string literal
            '"' => self.string(),
            // Any numeric characters are a number literal
            '0'..='9' => self.number(),

            // Identifiers must start with a letter or an _
            'a' ..='z' | 'A' ..='Z' | '_' => self.identifier(),


            _ => Err(ScanError::InvalidCharacter(c)),
        }
    }

    fn string(&mut self) -> Result<Token, ScanError> {
        // Consume characters until we reach a closing quote
        while self.peek().unwrap() != '"' && self.is_at_end() == false {
            // Support newlines within string literals
            if self.peek().unwrap() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(ScanError::UnterminatedString)
        }
        // Advance past the final terminating quote 
        self.advance();
        return Ok(self.make_token(TokenType::String))
    }

    fn number(&mut self) -> Result<Token, ScanError> {
        while self.peek().unwrap().is_digit(10) {
            self.advance();
        }

        // See if we have a fractional part
        if self.peek().unwrap() == '.' && self.peek_next().unwrap().is_digit(10) {
            // Consume the '.'
            self.advance();
            // Consume the rest of the number 
            while self.peek().unwrap().is_digit(10) {
                self.advance();
            }       
        }

        return Ok(self.make_token(TokenType::Number))
    }

    fn identifier(&mut self) -> Result<Token, ScanError> {
        // Scan out the rest of the identifier - we allow
        // numeric characters as well as digits 
        while self.peek().unwrap().is_ascii_alphanumeric() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type = TokenType::from_keyword(text).unwrap_or(TokenType::Identifier);
        Ok(self.make_token(token_type))
    }

    // Make a token struct from the current state of the scanner
    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: self.source[self.start..self.current].to_string(),
            line: self.line,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    // Look at the current character without consuming it
    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    // Look at the NEXT character without consuming it 
    fn peek_next(&self) -> Option<char> {
        if self.is_at_end() {
            return Some('\0')
        }
        self.source.chars().nth(self.current + 1)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false
        }

        if self.source.chars().nth(self.current).unwrap() != expected {
            return false
        }

        // We found the matching character
        self.current += 1;
        return true; 
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    // Two '/' in a row indicates a comment line
                    if self.peek_next().unwrap() == '/' {
                        while self.peek().unwrap() != '\n' && self.is_at_end() == false {
                            self.advance();
                        }
                    } else {
                        // Not part of a comment, return without advancing so this char can be tokenized
                        return;
                    }
                }
                _ => return,
            }
        }
    }
    

}