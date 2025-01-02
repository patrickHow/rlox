use crate::token::{Token, TokenType};

pub struct Scanner {
    start: usize,   // Start of current token
    current: usize, // Current character being examined within token
    line: u32,      // Current line number
}

// Result<> error values for scanning
#[derive(Debug)]
pub struct ScanError {
    msg: String,
    text: String,
    line: u32,
}

impl ScanError {
    pub fn print_error(&self) {
        println!("{} Error: {} at text: {}", self.line, self.msg, self.text);
    }
}

impl Scanner {
    pub fn new() -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
        }
    }

    // Scan and return the next token from the source
    pub fn scan_token(&mut self, source: &str) -> Result<Token, ScanError> {
        self.skip_whitespace(&source);
        self.start = self.current;

        if self.is_at_end(&source) {
            return Ok(self.make_token(&source, TokenType::Eof));
        }

        // Scan the next character
        let c = self.advance(&source);
        match c {
            // Single character tokens first
            '(' => Ok(self.make_token(&source, TokenType::LeftParen)),
            ')' => Ok(self.make_token(&source, TokenType::RightParen)),
            '{' => Ok(self.make_token(&source, TokenType::LeftBrace)),
            '}' => Ok(self.make_token(&source, TokenType::RightBrace)),
            ',' => Ok(self.make_token(&source, TokenType::Comma)),
            ';' => Ok(self.make_token(&source, TokenType::Semicolon)),
            '.' => Ok(self.make_token(&source, TokenType::Dot)),
            '+' => Ok(self.make_token(&source, TokenType::Plus)),
            '-' => Ok(self.make_token(&source, TokenType::Minus)),
            '*' => Ok(self.make_token(&source, TokenType::Star)),
            '/' => Ok(self.make_token(&source, TokenType::FSlash)),

            // Characters that MAY be two characters
            '!' => {
                if self.match_next(&source, '=') {
                    Ok(self.make_token(&source, TokenType::BangEqual))
                } else {
                    Ok(self.make_token(&source, TokenType::Bang))
                }
            }

            '=' => {
                if self.match_next(&source, '=') {
                    Ok(self.make_token(&source, TokenType::EqualEqual))
                } else {
                    Ok(self.make_token(&source, TokenType::Equal))
                }
            }

            '<' => {
                if self.match_next(&source, '=') {
                    Ok(self.make_token(&source, TokenType::LessEqual))
                } else {
                    Ok(self.make_token(&source, TokenType::Less))
                }
            }

            '>' => {
                if self.match_next(&source, '=') {
                    Ok(self.make_token(&source, TokenType::GreaterEqual))
                } else {
                    Ok(self.make_token(&source, TokenType::Greater))
                }
            }
            // A double quote should indicate the start of a string literal
            '"' => self.string(&source),
            // Any numeric characters are a number literal
            '0'..='9' => self.number(&source),

            // Identifiers must start with a letter or an _
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(&source),

            _ => Err(ScanError {
                msg: "Invalid character".to_string(),
                text: source[self.start..self.current].to_string(),
                line: self.line,
            }),
        }
    }

    fn string(&mut self, source: &str) -> Result<Token, ScanError> {
        // Consume characters until we reach a closing quote
        while self.peek(source).unwrap() != '"' && self.is_at_end(&source) == false {
            // Support newlines within string literals
            if self.peek(&source).unwrap() == '\n' {
                self.line += 1;
            }
            self.advance(&source);
        }

        if self.is_at_end(&source) {
            return Err(ScanError {
                msg: "Unterminated string".to_string(),
                text: source[self.start..self.current].to_string(),
                line: self.line,
            });
        }
        // Advance past the final terminating quote
        self.advance(&source);
        return Ok(self.make_token(&source, TokenType::String));
    }

    fn number(&mut self, source: &str) -> Result<Token, ScanError> {
        while self.peek(&source).unwrap().is_digit(10) {
            self.advance(&source);
        }

        // See if we have a fractional part
        if self.peek(&source).unwrap() == '.' && self.peek_next(&source).unwrap().is_digit(10) {
            // Consume the '.'
            self.advance(&source);
            // Consume the rest of the number
            while self.peek(&source).unwrap().is_digit(10) {
                self.advance(&source);
            }
        }

        return Ok(self.make_token(&source, TokenType::Number));
    }

    fn identifier(&mut self, source: &str) -> Result<Token, ScanError> {
        // Scan out the rest of the identifier - we allow
        // numeric characters as well as digits
        while self.peek(&source).unwrap().is_ascii_alphanumeric() {
            self.advance(&source);
        }

        let text = &source[self.start..self.current];
        let token_type = TokenType::from_keyword(text).unwrap_or(TokenType::Identifier);
        Ok(self.make_token(&source, token_type))
    }

    // Make a token struct from the current state of the scanner
    fn make_token(&self, source: &str, token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: source[self.start..self.current].to_string(),
            line: self.line,
        }
    }

    fn advance(&mut self, source: &str) -> char {
        self.current += 1;
        source.chars().nth(self.current - 1).unwrap()
    }

    // Look at the current character without consuming it
    fn peek(&self, source: &str) -> Option<char> {
        source.chars().nth(self.current)
    }

    // Look at the NEXT character without consuming it
    fn peek_next(&self, source: &str) -> Option<char> {
        if self.is_at_end(&source) {
            return Some('\0');
        }
        source.chars().nth(self.current + 1)
    }

    fn is_at_end(&self, source: &str) -> bool {
        self.current >= source.len()
    }

    fn match_next(&mut self, source: &str, expected: char) -> bool {
        if self.is_at_end(source) {
            return false;
        }

        if source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        // We found the matching character
        self.current += 1;
        return true;
    }

    fn skip_whitespace(&mut self, source: &str) {
        while let Some(c) = self.peek(&source) {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance(&source);
                }
                '\n' => {
                    self.line += 1;
                    self.advance(&source);
                }
                '/' => {
                    // Two '/' in a row indicates a comment line
                    if self.peek_next(source).unwrap() == '/' {
                        while self.peek(&source).unwrap() != '\n'
                            && self.is_at_end(&source) == false
                        {
                            self.advance(&source);
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
