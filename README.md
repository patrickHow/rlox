# rlox 

rlox is a rust implementation of the Lox programming language, following the excellent book ["Crafting Interpreters"](https://craftinginterpreters.com/) by Robert Nystrom. 
Specifically, rlox is my attempt at a rust implementation of the third part of the book, "A Bytecode Virtual Machine". 

rlox is, therefore, a bytecode virtual machine, scanner, and compiler. rlox is still very much a work in progress and is not yet a complete Lox implementation. 
This project is primarily for my own learning, so I am building it piece by piece. 

# Current State
Rlox has currently implemented up to and including chapter **21** of Crafting Interpreters, with support for global variable declaration and assignment. 

# Usage
rlox requires rust, and currently has no other crate dependencies. Building and running is as simple as:
`cargo run`

This will drop you into a REPL that will compile and output the results of chunks of Lox code. 
Alternatively, you can pass a .lox file to rlox to be compiled and run through the VM:
`cargo run filename.lox`
