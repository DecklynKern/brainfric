# brainfric
Compiler for a custom scripting language into brainf***.

# Overview
This is made largely for myself as a learning experience about writing my own programming language.
Instead of doing something normal like compiling to machine code or having an interpreter, brainfric compiles to BF (brainf***) code.
The goal is to create a language that is simple but has enough features to write sufficiently large programs and not have to think about the monstrosity of BF instructions it will turn into.

# Language description
Each statement is placed on a new line with no semicolons required.

## Variables
Variables are declared with a `[type] [name]` statement as in `byte a` or `bool x`.
Currently there are only two types (more are planned):
* `byte` represents a single byte
* `bool` represents a boolean `true` or `false`

## Assignment
Variables are assigned values using a `[name] <- [expression]` statement.
Here are some examples:
* `a <- 2`
* `a <- b`
* `a <- b + c`
* `x <- false`
* `x <- ?(#x + b)`

## Expressions
The following operations are allowed in expressions:
* `+` `-` `*` with their usual interpretations on byte variables
* `(` `)` with their usual interpretations
* `&` `|` `!` with their usual interpretations as AND OR and NOT on bool variables
* `#` which converts a bool to a byte (either 0 or 1)
* `?` which converts a byte to a bool (0 becomes false and anything else becomes true)

## I/O
The `read [name]` statement reads a character from the input into the specified byte variable.
The `write [expression]` statement writes the byte values of the result of the expression into the output.

## Control flow
Currently there are only two control flow patterns allowed: `while [expression]` and `if [expression]`.
These expressions must be of bool type, and then followed by the statements inside, and finally an `end` statement to close the block.

## Example program
This is a program in brainfric that prints out the byte values of the Fibonacci sequence (mod 256 because of overflow) until it loops back to 0:
```
byte a
byte b
byte c

a <- 0
b <- 1

while ?a
  write a
  c <- a + b
  a <- b
  b <- c
end
```

# How does this work?

## Basic BF lesson (skip if you know already)
BF code runs on a linear tape of cells all initialized to zero.
There is a read/write head initially pointing to the first cell which is used to perform all computation.

BF code then has 8 instructions to manipulate the tape and data head to produce programs:
* **+** increments the cell at the data head
* **-** decriments the cell at the data head
* **>** moves the data head one cell to the right
* **<** moves the data head one cell to the left
* **,** reads a character from the input (can be stdin, file input, or other)
* **.** writes a character to the output (can be stdout, file output, or other)
* **[** jumps to the instruction after the matching **]** if the cell at the data head is 0
* **]** jumps to the matching **[** instruction (backwards jump)

This instruction set is obviously quite limited, but provably Turing-complete.
This (at least for our purposes) means it has the same computational power as any other programming language.

brainfric assumes the following about the interpreter running its code (these are fairly standard among implementations):
* the tape is at least as large as needed to fit the compiled output
* each cell is a single byte in size
* the cells wrap (incremening a cell with 255 will leave it at 0 and decrimenting it will leave it at 255 again)
* the I/O operations use single bytes to transfer characters (like chars in C) in whatever encoding is used (some ASCII extension)

## brainfric IR
TODO
