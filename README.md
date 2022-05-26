# CS320 INTERPRETER PROJECT #
This is the official repository for my Interpreter project. 

## Table of Contents ##
1) Introduction to Ocaml
2) Brief description of program
3) Commands

## Introduction to Ocaml ##

Ocaml is a pretty unique programming language with the following features:
* Garbage Collection for automatic memory management.
* First-class functions (a function that is treated like a variable).
* Static type-checking. Variables are checked during compile-time.
* Parametric polymorphism. Enables construction of abstractions that work across data types.
* Immutable programming.
* Type inference. Variable types don't need to be declared, but have to remain the same. 
* Pattern matching. 

Ocaml used to be commonly used for parsers, compilers, and interpreters but now, along with wide support for the language, it is used for more things too. 

## Brief description of program ##

The order in which the interpreter operates is as follows:
1) Accepts a string of commands (eg. "Push 1 Push 2 Add 2")
2) The string of commands is then broken up into an array of commands. Each element of type command. 
3) The array is passed into a evaluator function that performs the commands on a stack. 
4) An array of results is returned from the evaluator function.

[Demonstration video.](https://youtu.be/EiNxiaXhf8w)

## Commands ##

Refer to [project_instructions](https://github.com/TayzaShwe/cs320_interpreter_project/blob/main/project_instructions.pdf).


