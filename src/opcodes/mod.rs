pub const OP_CONSTANT: u8 = 0; // [opcode][constant index] - pushes a constant to the stack
pub const OP_RETURN: u8 = 1; // [opcode] - pops a constant from the stack and prints it
pub const OP_ADD: u8 = 2; // [opcode] - adds the last two values on the stack
pub const OP_SUBTRACT: u8 = 3; // [opcode] - subtracts the last value on the stack from the second-from-last value on the stack
pub const OP_MULTIPLY: u8 = 4; // [opcode] - multiplies the last two values on the stack
pub const OP_DIVIDE: u8 = 5; // [opcode] - divides the last value on the stack by the second-from-last value on the stack
pub const OP_NEGATE: u8 = 6; // [opcode] - negates the value at the top of the stack
