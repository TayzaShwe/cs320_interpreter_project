# CS320 INTERPRETER PROJECT #
This is the official repository for my Interpreter project. 

## Table of Contents ##
1) Introduction to Ocaml
2) Brief description of program
3) Commands
4) How the program was built

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


## How the program was built ##

The first version I created used the terminal for user interface. I then switched to PyQt5. 
1) Download PyQt5
```
pip install pyqt5
pip install pyqt5-tools
```
2) Design the windows in designer. The app can be found inside the site-packages folder inside your python folder. This is PyQt5's drag-and-drop style application that allows you to build the interface. 
3) Save the design as a .ui file.
4) Convert the .ui file into a python file with the following command:
```
pyuic5 -x filename.ui -o filename.py
```
5) Edit the python file to add functionality. 
6) Download PyInstaller
```
pip install pyinstaller
```
7) Convert the python file into an executable file. Not that this converts to an executable file only compatible with your operating system. I used linux, so the executable file in this repository is only compatible with linux. Also note that the file size may be large since it adds all of PyQt5 into the executable file. There is no way around this issue at the moment.
```
pyinstaller --onefile filename.py
```
Now will you have the executable file! 

## TECHNOLOGIES INVOLVED ##

* Python 
* [PyQt5](https://pypi.org/project/PyQt5/)
* [PyInstaller](https://pyinstaller.org/en/stable/) 

