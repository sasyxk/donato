# Donato Language

**Donato** is a custom programming language developed as part of my master's thesis, which I completed at the University of Twente in the Netherlands and presented at the University of Parma, where I am currently enrolled.

This language has been built completely **from scratch** using C++. The core components include a parser and an Abstract Syntax Tree (AST). Each node of the AST class implements specific code generation functions that produce LLVM intermediate representation (IR), enabling the language to be **compiled** into native executable code via the LLVM infrastructure.

By leveraging LLVM, Donato benefits from a modern and powerful backend, allowing for efficient code generation and optimization. This compilation pipeline ensures that source code written in Donato can be transformed into highly optimized machine code.

---

## Getting Started (Linux)

To build and run the Donato compiler on a Linux system, follow these steps:

1. Clone the repository:
   ```bash
   git clone https://github.com/sasyxk/donato.git
   cd donato

2. Install dependencies
    ```bash
    sudo apt update
    sudo apt install clang llvm-dev

3. Create a build directory and compile the compiler:
    ```bash
    mkdir build
    cd build
    cmake ..
    make

## Using the Compiler

To compile a .donato source file:

1. Place your source file (e.g., example.donato) inside the build directory or anywhere you prefer.

2. Run the compiler with:
    ```bash
    ./dtc example.donato

3. The compiler will generate a default executable named *output*.
    ```bash
    ./output

**Note**: In future versions, you will be able to specify the output executable's name via command-line options.

## About the Project
Donato is designed as a compiled language to explore modern compiler design techniques. Writing the compiler from scratch allowed me to deeply understand parsing, AST construction, and code generation using LLVM. This project serves both as a research and educational tool for compiler technology.