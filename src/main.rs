use std::string;

// TODO this all needs to be split to individual files

const DEFAULT_CHUNK_LEN: usize = 16;

type ConstantType = f64; 

// OPCODES - defining as u8 instead of enum so we can do direct comparisons
// without all kinds of nasty casting 

const OP_CONSTANT: u8 = 0; // [opcode][constant index]
const OP_RETURN: u8 = 1; // [opcode]


fn simple_instruction(name: String, offset: usize) -> usize {
    println!("{name}");
    return offset + 1
}

struct Chunk {
    code: Vec<u8>,
    lines: Vec<u32>,
    constants: Vec<ConstantType>,
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
    fn add_constant(&mut self, value: ConstantType) -> u8 {
        self.constants.push(value);
        return (self.constants.len() - 1) as u8
    }
}


fn main() {
    let mut chunk = Chunk::new();
    // Basic hand-spun code to test the chunk type
    // Arbitrary line number for now
    let ind = chunk.add_constant(1.2);
    chunk.write(OP_CONSTANT, 123);
    chunk.write(ind, 123);
    chunk.write(OP_RETURN, 123);

    chunk.disassemble("test chunk".to_string());
}
