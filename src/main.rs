// mod vm;

// TODO this all needs to be split to individual files

const DEFAULT_CHUNK_LEN: usize = 16;

// The core "value" type that operations work on
type Value = f64; 

// OPCODES - defining as u8 instead of enum so we can do direct comparisons
// without all kinds of nasty casting 

const OP_CONSTANT: u8 = 0; // [opcode][constant index] - pushes a constant to the stack
const OP_RETURN: u8 = 1; // [opcode] - pops a constant from the stack and prints it
const OP_ADD: u8 = 2; // [opcode] - adds the last two values on the stack
const OP_SUBTRACT: u8 = 3; // [opcode] - subtracts the last value on the stack from the second-from-last value on the stack
const OP_MULTIPLY: u8 = 4; // [opcode] - multiplies the last two values on the stack
const OP_DIVIDE: u8 = 5; // [opcode] - divides the last value on the stack by the second-from-last value on the stack
const OP_NEGATE: u8 = 6; // [opcode] - negates the value at the top of the stack

// Const functions for doing binary operations
const fn add(a: f64, b: f64) -> f64 {a + b}
const fn sub(a: f64, b: f64) -> f64 {a - b}
const fn mul(a: f64, b: f64) -> f64 {a * b}
const fn div(a: f64, b: f64) -> f64 {a / b}

fn simple_instruction(name: String, offset: usize) -> usize {
    println!("{name}");
    return offset + 1
}

struct Chunk {
    code: Vec<u8>,
    lines: Vec<u32>,
    constants: Vec<Value>,
}

impl Chunk {

    fn new() -> Chunk {
        Self {
            code: Vec::with_capacity(DEFAULT_CHUNK_LEN),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    // Debug function for disassembling a whole chunk 
    fn disassemble(&self, name: String) {
        // Print a header for convenience
        println!("== {} ==", name);

        let mut offset = 0;

        // Disassemble each instruction in the chunk 
        // Note that we skip the offset forward in the return to account for
        // differing byte lengths of instructions
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset)
        }
    }

    // Disassemble an individual instruction
    fn disassemble_instruction(&self, offset: usize) -> usize {
        
        // Print the offset in the chunk as well as the line number
        print!("{offset:>4} ");
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:>4} ", self.lines[offset]);
        }
        
        let instruction = self.code[offset];
        match instruction {
            OP_RETURN => simple_instruction("OP_RETURN".to_string(), offset),
            OP_CONSTANT => self.constant_instruction("OP_CONSTANT".to_string(), offset),
            OP_ADD => simple_instruction("OP_ADD".to_string(), offset),
            OP_SUBTRACT => simple_instruction("OP_SUBTRACT".to_string(), offset),
            OP_MULTIPLY => simple_instruction("OP_MULTIPLY".to_string(), offset),
            OP_DIVIDE => simple_instruction("OP_DIVIDE".to_string(), offset),

            OP_NEGATE => simple_instruction("OP_NEGATE".to_string(), offset),
            _ => {
                println!("Invalid opcode {instruction}");
                offset + 1 // Advance past the bad instruction
            },
        }
    }

    // Disassemble helper for constant instruction 
    fn constant_instruction(&self, name: String, offset: usize) -> usize {
        // Second byte of the instruction will the index of the constant
        // in the chunk's constant array 
        let ind = self.code[offset + 1] as usize;
        let value = self.constants[ind];
        println!("{name} {value}");
        return offset + 2;
    }

    // Write bytecode to the chunk
    fn write(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    // Write a constant value to the chunk
    // Returns the index of the added value for later use
    // TODO fix if we need more than 256 constants per chunk (but...why)
    fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        return (self.constants.len() - 1) as u8
    }
}

struct VM {
    chunk: Chunk, // The current chunk being executed
    ip: usize, // Instruction pointer - an index into the code array in the vector
    stack: Vec<Value>, // A stack for values in the VM 
}

enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

impl VM {
    fn new(initial: Chunk) -> VM {
        Self {
            chunk: initial,
            ip: 0,
            stack: Vec::new(),
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn binary_op(&mut self, op: fn(f64, f64) -> f64) {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();
        self.stack.push(op(a, b));
    }

    fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if cfg!(debug_assertions) {
                // Dump the stack trace
                for val in &self.stack {
                    print!("[{val}]");
                }
                println!();

                // Print instruction about to be executed
                self.chunk.disassemble_instruction(self.ip);
            }
            let instruction = self.chunk.code[self.ip];
            self.ip += 1;

            match instruction {
                OP_CONSTANT => {
                    let ind = self.chunk.code[self.ip] as usize;
                    self.ip += 1;
                    let val = self.chunk.constants[ind];
                    self.stack.push(val);
                }
                OP_RETURN => {
                    let val = self.stack.pop().unwrap();
                    println!("{val}");
                    return InterpretResult::OK
                }
                OP_ADD => self.binary_op(add),
                OP_SUBTRACT => self.binary_op(sub),
                OP_MULTIPLY => self.binary_op(mul),
                OP_DIVIDE => self.binary_op(div),
                OP_NEGATE => {
                    let top_idx = self.stack.len() - 1;
                    self.stack[top_idx] = -self.stack[top_idx];
                }
                _ => return InterpretResult::RuntimeError,
            }
        }
    }
}


fn main() {
    let dummy_chunk = Chunk::new();
    let mut chunk = Chunk::new();
    // Basic hand-spun code to test the chunk type
    // Arbitrary line number for now
    let ind = chunk.add_constant(1.2);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(ind, 123);
    
    let ind = chunk.add_constant(3.4);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(ind, 123);

    chunk.write(OP_ADD, 123);

    let ind = chunk.add_constant(5.6);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(ind, 123);

    chunk.write(OP_DIVIDE, 123);

    chunk.write(OP_NEGATE, 123);
    chunk.write(OP_RETURN, 123);

    println!("Code execution starting...");

    let mut vm = VM::new(dummy_chunk);
    vm.interpret(chunk);
}
