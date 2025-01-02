pub const OP_CONSTANT: u8 = 0; // [opcode][constant index] - pushes a numeric constant to the stack
pub const OP_RETURN: u8 = 1; // [opcode] - pops a constant from the stack and prints it
pub const OP_ADD: u8 = 2; // [opcode] - adds the last two values on the stack
pub const OP_SUBTRACT: u8 = 3; // [opcode] - subtracts the last value on the stack from the second-from-last value on the stack
pub const OP_MULTIPLY: u8 = 4; // [opcode] - multiplies the last two values on the stack
pub const OP_DIVIDE: u8 = 5; // [opcode] - divides the last value on the stack by the second-from-last value on the stack
pub const OP_NEGATE: u8 = 6; // [opcode] - negates the value at the top of the stack
pub const OP_NIL: u8 = 7; // [opcode] - pushes a nil value to the stack
pub const OP_TRUE: u8 = 8; // [opcode] - pushes a true bool value to the stack
pub const OP_FALSE: u8 = 9; // [opcode] - pushes a false bool value to the stack
pub const OP_NOT: u8 = 10; // [opcode] - applies logical not to the value at the top of the stack
pub const OP_EQUAL: u8 = 11; // [opcode] - checks the equality of the last two values on the stack
pub const OP_GREATER: u8 = 12; // [opcode] - checks if the second-from-last value on the stack is greater than the last value on the stack
pub const OP_LESS: u8 = 13; // [opcode] - checks if the second-from-last value on the stack is less than the last value on the stack
