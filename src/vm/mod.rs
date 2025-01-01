use crate::chunk;
use crate::opcodes;

const DEFAULT_VM_STACK_SIZE: usize = 256;

// Const functions for doing binary operations
const fn add(a: f64, b: f64) -> f64 {a + b}
const fn sub(a: f64, b: f64) -> f64 {a - b}
const fn mul(a: f64, b: f64) -> f64 {a * b}
const fn div(a: f64, b: f64) -> f64 {a / b}

// TODO this needs to be moved to the interpreter
// and we need to figure out how to propagate results 
pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

pub struct VM {
    ip: usize, // Instruction pointer - an index into the code array in the vector
    stack: Vec<f64>, // A stack for values in the VM 
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(DEFAULT_VM_STACK_SIZE),
        }
    }

    fn reset(&mut self) {
        self.ip = 0;
        self.stack.clear();
    }

    fn binary_op(&mut self, op: fn(f64, f64) -> f64) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        self.stack.push(op(a, b));
    }

    pub fn run(&mut self, chunk: chunk::Chunk) -> InterpretResult {

        self.reset();

        loop {
            if cfg!(debug_assertions) {
                // Dump the stack trace
                for val in &self.stack {
                    print!("[{val}]");
                }
                println!();

                // Print instruction about to be executed
                chunk.disassemble_instruction(self.ip);
            }
            let instruction = chunk.code[self.ip];
            self.ip += 1;

            match instruction {
                opcodes::OP_CONSTANT => {
                    let ind = chunk.code[self.ip] as usize;
                    self.ip += 1;
                    let val = chunk.constants[ind];
                    self.stack.push(val);
                }
                opcodes::OP_RETURN => {
                    let val = self.stack.pop().unwrap();
                    println!("{val}");
                    return InterpretResult::OK
                }
                opcodes::OP_ADD => self.binary_op(add),
                opcodes::OP_SUBTRACT => self.binary_op(sub),
                opcodes::OP_MULTIPLY => self.binary_op(mul),
                opcodes::OP_DIVIDE => self.binary_op(div),
                opcodes::OP_NEGATE => {
                    let top_idx = self.stack.len() - 1;
                    self.stack[top_idx] = -self.stack[top_idx];
                }
                _ => return InterpretResult::RuntimeError,
            }
        }
    }
}
