use crate::chunk;
use crate::opcodes;
use crate::opcodes::OP_SET_LOCAL;
use crate::value::Value;
use std::collections::HashMap;

const DEFAULT_VM_STACK_SIZE: usize = 256;

// Const functions for doing binary operations
const fn add(a: f64, b: f64) -> Value {
    Value::Double(a + b)
}
const fn sub(a: f64, b: f64) -> Value {
    Value::Double(a - b)
}
const fn mul(a: f64, b: f64) -> Value {
    Value::Double(a * b)
}
const fn div(a: f64, b: f64) -> Value {
    Value::Double(a / b)
}

const fn greater(a: f64, b: f64) -> Value {
    Value::Bool(a > b)
}

const fn less(a: f64, b: f64) -> Value {
    Value::Bool(a < b)
}

// TODO this needs to be moved to the interpreter
// and we need to figure out how to propagate results
pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

// Utility function for value equality
// Very simple thanks to Rust's PartialEq
fn values_equal(a: Value, b: Value) -> bool {
    a == b
}

pub struct VM {
    ip: usize,         // Instruction pointer - an index into the code array in the vector
    stack: Vec<Value>, // A stack for values in the VM
    globals: HashMap<String, Value>, // Hash table for global variable storage
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: 0,
            stack: Vec::with_capacity(DEFAULT_VM_STACK_SIZE),
            globals: HashMap::new(),
        }
    }

    fn reset(&mut self) {
        self.ip = 0;
        self.stack.clear();
    }

    fn binary_op(&mut self, line: u32, op: fn(f64, f64) -> Value) -> bool {
        match (self.stack.get(self.stack.len() - 2), self.stack.last()) {
            (Some(&Value::Double(a)), Some(&Value::Double(b))) => {
                self.stack.truncate(self.stack.len() - 2);
                self.stack.push(op(a, b));
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

        println!("\n== VM Starting ==\n");

        loop {
            if cfg!(debug_assertions) {
                // Print the instruction pointer
                println!("ip: {}", self.ip);
                // Dump the stack trace
                print!("stack: ");
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
                    let val = chunk.constants[ind].clone();
                    self.stack.push(val);
                }
                opcodes::OP_RETURN => {
                    // Exit VM
                    return InterpretResult::OK;
                }
                opcodes::OP_ADD => {
                    // Special case for add - string concatenation
                    match (self.stack.get(self.stack.len() - 2), self.stack.last()) {
                        (Some(&Value::Double(a)), Some(&Value::Double(b))) => {
                            self.stack.truncate(self.stack.len() - 2);
                            self.stack.push(Value::Double(a + b));
                        }
                        (Some(Value::String(a)), Some(Value::String(b))) => {
                            // Concat first then push - otherwise we try to truncate the vec while a and b are borrowing from it
                            let concat = Value::String(a.to_owned() + b);
                            self.stack.truncate(self.stack.len() - 2);
                            self.stack.push(concat);
                        }
                        _ => {
                            self.runtime_error(
                                "Operands must be both numbers or both strings".to_string(),
                                line,
                            );
                            return InterpretResult::RuntimeError;
                        }
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
                opcodes::OP_NIL => self.stack.push(Value::Nil),
                opcodes::OP_FALSE => self.stack.push(Value::Bool(false)),
                opcodes::OP_TRUE => self.stack.push(Value::Bool(true)),
                opcodes::OP_NOT => {
                    // Logical negate applies to a Value based on its type
                    // See Value::is_falsey for details
                    let val = Value::Bool(self.stack.pop().unwrap().is_falsey());
                    self.stack.push(val)
                }
                opcodes::OP_EQUAL => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(values_equal(a, b)));
                }
                opcodes::OP_GREATER => {
                    if !self.binary_op(line, greater) {
                        return InterpretResult::RuntimeError;
                    }
                }
                opcodes::OP_LESS => {
                    if !self.binary_op(line, less) {
                        return InterpretResult::RuntimeError;
                    }
                }
                opcodes::OP_PRINT => {
                    let value = self.stack.pop().unwrap();
                    value.print();
                    println!();
                }
                opcodes::OP_POP => {
                    // TODO should we throw an error if this gets inserted and pops nothing?
                    self.stack.pop();
                }
                opcodes::OP_DEFINE_GLOBAL => {
                    let ind = chunk.code[self.ip] as usize;
                    self.ip += 1;
                    // The value itself will be on the stack
                    let val = self.stack.pop().unwrap();
                    if let Value::String(key) = &chunk.constants[ind] {
                        self.globals.insert(key.to_owned(), val);
                        self.stack.pop();
                    } else {
                        self.runtime_error("Non-string value for variable name".to_string(), line);
                    }
                }
                opcodes::OP_GET_GLOBAL => {
                    let ind = chunk.code[self.ip] as usize;
                    self.ip += 1;
                    if let Value::String(name) = &chunk.constants[ind] {
                        if let Some(value) = self.globals.get(name) {
                            self.stack.push(value.clone());
                        } else {
                            self.runtime_error(format!("Undefined variable: {}", name), line);
                        }
                    } else {
                        self.runtime_error(
                            "Non-string constant fetched for variable name".to_string(),
                            line,
                        );
                    }
                }
                opcodes::OP_SET_GLOBAL => {
                    let ind = chunk.code[self.ip] as usize;
                    self.ip += 1;
                    // Pop the new value
                    let val = self.stack.pop().unwrap();
                    if let Value::String(key) = &chunk.constants[ind] {
                        // Valid variable name, does it exist?
                        match self.globals.get_mut(key) {
                            None => {
                                self.runtime_error(format!("Undefined variable {}", key), line);
                            }
                            Some(v) => *v = val.clone(),
                        }
                    } else {
                        self.runtime_error("Non-string value for variable name".to_string(), line);
                    }
                }
                opcodes::OP_GET_LOCAL => {
                    // Get the stack slot where the local variable lives
                    let slot = chunk.code[self.ip] as usize;
                    self.ip += 1;
                    // Put it on the stack
                    self.stack.push(self.stack[slot].clone());
                }
                opcodes::OP_SET_LOCAL => {
                    // Get the value from the top of the stack and update the variable's slot
                    let slot = chunk.code[self.ip] as usize;
                    self.ip += 1;
                    self.stack[slot] = self.stack.last().unwrap().clone();
                    // Note we don't pop the stack - assignment is an expression and therefore produces a value
                }
                opcodes::OP_JUMP_IF_FALSE => {
                    let offset: usize = (((chunk.code[self.ip] as u16) << 8)
                        | (chunk.code[self.ip + 1] as u16))
                        as usize;
                    self.ip += 2;
                    if self.stack.last().unwrap().is_falsey() {
                        self.ip += offset;
                    }
                }
                opcodes::OP_JUMP => {
                    let offset: usize = (((chunk.code[self.ip] as u16) << 8)
                        | (chunk.code[self.ip + 1] as u16))
                        as usize;
                    self.ip += 2;
                    self.ip += offset;
                }
                _ => return InterpretResult::RuntimeError,
            }
        }
    }

    fn runtime_error(&mut self, msg: String, line: u32) {
        println!("[line {line}]: runtime error: {msg}");
    }
}
