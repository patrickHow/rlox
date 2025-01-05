use crate::opcodes;
use crate::value::Value;

const DEFAULT_CHUNK_LEN: usize = 16;

pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<u32>,
    pub constants: Vec<Value>,
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    return offset + 1;
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
    pub fn disassemble(&self, name: &str) {
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
            opcodes::OP_RETURN => simple_instruction("OP_RETURN", offset),
            opcodes::OP_CONSTANT => self.constant_instruction("OP_CONSTANT", offset),
            opcodes::OP_ADD => simple_instruction("OP_ADD", offset),
            opcodes::OP_SUBTRACT => simple_instruction("OP_SUBTRACT", offset),
            opcodes::OP_MULTIPLY => simple_instruction("OP_MULTIPLY", offset),
            opcodes::OP_DIVIDE => simple_instruction("OP_DIVIDE", offset),
            opcodes::OP_NEGATE => simple_instruction("OP_NEGATE", offset),
            opcodes::OP_NIL => simple_instruction("OP_NIL", offset),
            opcodes::OP_FALSE => simple_instruction("OP_FALSE", offset),
            opcodes::OP_TRUE => simple_instruction("OP_TRUE", offset),
            opcodes::OP_NOT => simple_instruction("OP_NOT", offset),
            opcodes::OP_EQUAL => simple_instruction("OP_EQUAL", offset),
            opcodes::OP_GREATER => simple_instruction("OP_GREATER", offset),
            opcodes::OP_LESS => simple_instruction("OP_LESS", offset),
            opcodes::OP_PRINT => simple_instruction("OP_PRINT", offset),
            opcodes::OP_POP => simple_instruction("OP_POP", offset),
            opcodes::OP_DEFINE_GLOBAL => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            opcodes::OP_GET_GLOBAL => self.constant_instruction("OP_GET_GLOBAL", offset),
            opcodes::OP_SET_GLOBAL => self.constant_instruction("OP_SET_GLOBAL", offset),
            opcodes::OP_SET_LOCAL => self.byte_instruction("OP_SET_LOCAL", offset),
            opcodes::OP_GET_LOCAL => self.byte_instruction("OP_GET_LOCAL", offset),
            opcodes::OP_JUMP => self.jump_instruction("OP_JUMP", 1, offset),
            opcodes::OP_JUMP_IF_FALSE => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            opcodes::OP_LOOP => self.jump_instruction("OP_LOOP", -1, offset),
            _ => {
                println!("Invalid opcode {instruction}");
                offset + 1 // Advance past the bad instruction
            }
        }
    }

    // Disassemble helper for constant instruction
    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        // Second byte of the instruction will the index of the constant
        // in the chunk's constant array
        let ind = self.code[offset + 1] as usize;
        let value = &self.constants[ind];
        print!("{name}: ");
        value.print();
        println!();
        return offset + 2;
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        print!("{name}: {}", slot);

        println!();
        return offset + 2;
    }

    fn jump_instruction(&self, name: &str, sign: i16, offset: usize) -> usize {
        let jump = (((self.code[offset + 1] as u16) << 8) + (self.code[offset + 2] as u16)) as i16;

        println!("{name}: {}->{}", offset, (offset as i16 + 3 + sign * jump));

        return offset + 3;
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
        return (self.constants.len() - 1) as u8;
    }
}
