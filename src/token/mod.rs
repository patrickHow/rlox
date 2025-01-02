// Define token types possible in Lox
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen = 0,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    FSlash,
    Star,

    // One- or two-character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Special tokens
    Error,
    Eof,
}

// One day this will be stabilized, for now we manually maintain
// pub const TOKEN_COUNT: usize = variant_count::<TokenType>();
pub const TOKEN_COUNT: usize = 40;

impl TokenType {
    pub fn from_keyword(keyword: &str) -> Option<TokenType> {
        match keyword {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String, // This might not be as efficient as &str but holy hell the lifetimes
    pub line: u32,
}

impl Token {
    pub fn error(&self, msg: String) {
        print!("[Line {}] Error", self.line);
        if self.token_type == TokenType::Eof {
            print!(" at end");
        } else if self.token_type == TokenType::Error {
            // nothing to print
        } else {
            print!(" at {}", self.lexeme);
        }

        println!(": {}", msg);
    }
}
