// Enum wrapping the types that a Lox variable can have
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Double(f64),
    String(String),
}

impl Value {
    pub fn print(&self) {
        match self {
            Value::Nil => print!("nil"),
            Value::Bool(v) => print!("{v}"),
            Value::Double(v) => print!("{v}"),
            Value::String(s) => print!("{s}"),
        }
    }

    // Return the "falsiness" of the value
    // Yes, I dislike this word as well
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(v) => !v,
            Value::Double(_) => false,
            Value::String(_) => false,
        }
    }
}
