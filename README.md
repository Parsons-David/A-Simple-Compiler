# A Simple Compiler

### David Parsons - Rutgers University
### Compilers - Spring 2018
### NetID: dwp35

## Description

A Simple Compiler written with Yacc for CS 415 S2018.

## Compilation
Simply run `make` to build the `codegen` executable.

## Usage
Example call of compiler:
```code
./codegen < testcases/demo1
```

### Optimization
The `-O` flag will optimize common sub-expressions in the iLoc code.

Example
```code
./codegen -O < testcases/demo1
```
