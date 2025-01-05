pub mod function;
use function::Function;

// Enum wrapping the types that a Lox variable can have
#[derive(Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Double(f64),
    String(String),
    Func(Function)
}

impl Value {
    pub fn print(&self) {
        match self {
            Value::Nil => print!("nil"),
            Value::Bool(v) => print!("{v}"),
            Value::Double(v) => print!("{v}"),
            Value::String(s) => print!("{s}"),
            Value::Func(f) => print!("{}", f.name),
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
            Value::Func(_) => false, // Please do not the function
        }
    }
}
