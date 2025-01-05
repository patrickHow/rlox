use crate::chunk::Chunk;

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub arity: u32,
    pub chunk: Chunk,
}

impl PartialEq for Function {
    // Treat functions as "equal" if they have the same name and arity
    // So add(a, b) is not equivalent to add(a, b, c)
    fn eq(&self, other: &Function) -> bool {
        if (self.name == other.name) && (self.arity == other.arity) {
            return true;
        }
        false
    }
}
