use crate::chunk;
use crate::opcodes;
use crate::value::Value;

const DEFAULT_VM_STACK_SIZE: usize = 256;

// Const functions for doing binary operations
const fn add(a: f64, b: f64) -> f64 {
    a + b
}
const fn sub(a: f64, b: f64) -> f64 {
    a - b
}
const fn mul(a: f64, b: f64) -> f64 {
    a * b
}
const fn div(a: f64, b: f64) -> f64 {
    a / b
}

// TODO this needs to be moved to the interpreter
// and we need to figure out how to propagate results
pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

pub struct VM {
    ip: usize,         // Instruction pointer - an index into the code array in the vector
    stack: Vec<Value>, // A stack for values in the VM
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

    fn binary_op(&mut self, line: u32, op: fn(f64, f64) -> f64) -> bool {
        match (self.stack.get(self.stack.len() - 2), self.stack.last()) {
            (Some(&Value::Double(a)), Some(&Value::Double(b))) => {
                self.stack.truncate(self.stack.len() - 2);
                self.stack.push(Value::Double(op(a, b)));
                true
            }
            _ => {
                self.runtime_error("Operands must be numbers".to_string(), line);
                false
            }
        }
    }

    pub fn run(&mut self, chunk: chunk::Chunk) -> InterpretResult {
        self.reset();

        loop {
            if cfg!(debug_assertions) {
                // Dump the stack trace
                for val in &self.stack {
                    print!("[");
                    val.print();
                    print!("]");
                }
                println!();

                // Print instruction about to be executed
                chunk.disassemble_instruction(self.ip);
            }
            let instruction = chunk.code[self.ip];
            // Line number is useful to have around for reporting errors
            let line = chunk.lines[self.ip];
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
                    val.print();
                    println!();
                    return InterpretResult::OK;
                }
                opcodes::OP_ADD => {
                    if !self.binary_op(line, add) {
                        return InterpretResult::RuntimeError;
                    }
                }
                opcodes::OP_SUBTRACT => {
                    if !self.binary_op(line, sub) {
                        return InterpretResult::RuntimeError;
                    }
                }
                opcodes::OP_MULTIPLY => {
                    if !self.binary_op(line, mul) {
                        return InterpretResult::RuntimeError;
                    }
                }
                opcodes::OP_DIVIDE => {
                    if !self.binary_op(line, div) {
                        return InterpretResult::RuntimeError;
                    }
                }
                opcodes::OP_NEGATE => match self.stack.last().unwrap() {
                    Value::Double(v) => {
                        *self.stack.last_mut().unwrap() = Value::Double(-v);
                    }
                    _ => {
                        self.runtime_error("Operand must be a number".to_string(), line);
                        return InterpretResult::RuntimeError;
                    }
                },
                _ => return InterpretResult::RuntimeError,
            }
        }
    }

    fn runtime_error(&mut self, msg: String, line: u32) {
        println!("[line {line}]: runtime error: {msg}");
    }
}
