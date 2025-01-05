# rlox 

rlox is a rust implementation of the Lox programming language, following the excellent book ["Crafting Interpreters"](https://craftinginterpreters.com/) by Robert Nystrom. 
Specifically, rlox is my attempt at a rust implementation of the third part of the book, "A Bytecode Virtual Machine". 

rlox is, therefore, a bytecode virtual machine, scanner, and compiler. rlox is still very much a work in progress and is not yet a complete Lox implementation. 
This project is primarily for my own learning, so I am building it piece by piece. 

# Progress
Progress on Part 3 of the book is going well:
- [x] Ch.14 Support for a bytecode `Chunk` type
- [x] Ch.15 A stack-based virtual machine to execute bytecode `Chunk`
- [x] Ch.16 An on-demand scanner for parsing source text into `Token`
- [x] Ch.17 A compiler and parser to turn scanned `Token`s into a `Chunk` of executable bytecode
- [x] Ch.18 A `Value` type to represent supported Lox types, plus some basic operations on constants
- [x] Ch.19 Support for `String` literals - using Rust's `String` type
- [x] Ch.20 Hash tables - largely skipped by using Rust's `HashMap`
- [x] Ch.21 Support for global variables 
- [x] Ch.22 Support for local variables and block scopes
- [x] Ch.23 Support for `if`, `else`, `while`, and `for`
- [ ] Ch.24 Functions and function calls
- [ ] Ch.25 Closures
- [ ] Ch.26 Garbage collection 
- [ ] Ch.27 Classes and Instances
- [ ] Ch.28 Methods and initializers
- [ ] Ch.29 Superclasses
- [ ] Ch.30 Optimization

# Usage
rlox requires rust, and currently has no other crate dependencies. Building and running is as simple as:
`cargo run`

This will drop you into a REPL that will compile and output the results of chunks of Lox code. 
Alternatively, you can pass a .lox file to rlox to be compiled and run through the VM:
`cargo run filename.lox`
