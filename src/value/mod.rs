
// Enum wrapping the types that a Lox variable can have
#[derive(Debug, Clone, Copy)]
pub enum Value {
    Nil,
    Bool(bool),
    Double(f64),
}

impl Value {
    pub fn print(&self) {
        match self {
            Value::Nil => print!("nil"),
            Value::Bool(v) => print!("{v}"),
            Value::Double(v) => print!("{v}"),
        }
    }
}
