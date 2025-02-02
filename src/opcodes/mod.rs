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
pub const OP_PRINT: u8 = 14; // [opcode] - prints the last value on the stack
pub const OP_POP: u8 = 15; // [opcode] - discard the last item on the stack
pub const OP_DEFINE_GLOBAL: u8 = 16; // [opcode][ind] - add a variable to the VM's global variable table. The name is at [ind] in the chunk constant table, the value is on the stack
pub const OP_GET_GLOBAL: u8 = 17; // [opcode][ind] - load the value in the variable whose name is stored at the index in the constant table
pub const OP_SET_GLOBAL: u8 = 18; // [opcode][ind] - set the value in the variable whose name is stored at the index in the constant table to the value on the stack
pub const OP_GET_LOCAL: u8 = 19; // [opcode][ind] - load the value in the variable whose name is stored at the index in the constant table
pub const OP_SET_LOCAL: u8 = 20; // [opcode][ind] - set the value in the variable whose name is stored at the index in the constant table to the value on the stack
pub const OP_JUMP: u8 = 21; // [opcode][operand_hi][operand_lo] - offset the instruction pointer by the 16-bit operand
pub const OP_JUMP_IF_FALSE: u8 = 22; // [opcode][operand_hi][operand_lo] - if the value on the top of the stack is false(y), offset the instruction pointer by the 16-bit operand
pub const OP_LOOP: u8 = 23; // [opcode][operand_hi][operand_lo] - unconditionally subtract the 16-bit value in the operand from the instruction pointer
