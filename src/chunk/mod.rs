use crate::opcodes;
use crate::value::Value;

const DEFAULT_CHUNK_LEN: usize = 16;

pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<u32>,
    pub constants: Vec<Value>,
}

fn simple_instruction(name: String, offset: usize) -> usize {
    println!("{name}");
    return offset + 1
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::with_capacity(DEFAULT_CHUNK_LEN),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    // Debug function for disassembling a whole chunk 
    pub fn disassemble(&self, name: String) {
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
    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        
        // Print the offset in the chunk as well as the line number
        print!("{offset:>4} ");
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:>4} ", self.lines[offset]);
        }
        
        let instruction = self.code[offset];
        match instruction {
            opcodes::OP_RETURN => simple_instruction("OP_RETURN".to_string(), offset),
            opcodes::OP_CONSTANT => self.constant_instruction("OP_CONSTANT".to_string(), offset),
            opcodes::OP_ADD => simple_instruction("OP_ADD".to_string(), offset),
            opcodes::OP_SUBTRACT => simple_instruction("OP_SUBTRACT".to_string(), offset),
            opcodes::OP_MULTIPLY => simple_instruction("OP_MULTIPLY".to_string(), offset),
            opcodes::OP_DIVIDE => simple_instruction("OP_DIVIDE".to_string(), offset),

            opcodes::OP_NEGATE => simple_instruction("OP_NEGATE".to_string(), offset),
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
        let value = &self.constants[ind];
        print!("{name}: ");
        value.print();
        println!();
        return offset + 2;
    }

    // Write bytecode to the chunk
    pub fn write(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    // Write a constant value to the chunk
    // Returns the index of the added value for later use
    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        return (self.constants.len() - 1) as u8
    }
}
