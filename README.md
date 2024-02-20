# ALPS: an Array and List Processing System

ALPS is a simple lisp interpreter that incorporates most of the array
processing primitives from APL\360. The lisp dialect is primarily that
of the original LISP1.5. Alps stands for "An Array and List Processing
System" or "A Lisp Personal System". It is designed for use as an
interactive personal computing environment for engineers and
scientists that encourages experimentation with code and data. It
derives its simplicity from using only original lisp syntax without
syntactic sugar. Nevertheles it is extremely powerful thanks to the
mostly happy marriage of the functional nature of lisp with the array
processing and manipulation primitives of APL. Programmes can be coded
very concisely affording economy of typing and navigation. Alps is a
pure interpreter that directly interprets its internal list
representaion without any intermediate code. This facilitates
debugging and creating code that generates code for
execution. Performance should be adequate for most personal needs on
modern equipment.

License: [0BSD](LICENSE)


## Prerequisites

gcc

make

The optional graphics capability is based on Maorong Zou's
[EZWGL](https://github.com/dpenkler/EZWGL) library which must be
installed before building alps.

The optional instrument control capability is based on
[linux-gpib](https://sourceforge.net/projects/linux-gpib/files/latest/download) 
## Installation

Installing alps creates a directory "alps" in your current directory
and copies the files to it.

For Un*x systems:
1) Obtain a source
```
$ git clone https://github.com/dpenkler/ALPS ALPS
```
2) Change to the directory into which you cloned ALPS
```
$ cd ALPS
```
3) Building the executable:

There ar a number of capabilities that can be activated by setting the capability identifier to 1 on the  **make** command line
```
GRAF    enable graphics integration with EZWGL
INSTCON enable gpib instrument control
SOUND   support for play & record 
SOCKET  enable networking support
DEBUG   enable internal debug output (developers only)
STATS   internal profiling support (developers only)
```
Other capabilities are controlled by the **FAST** identifier
```
If FAST==1 then
PREEMPT task preemption        is turned off
RANGE   range checking         is turned off
CKARGS  argument checking      is turned off
TDEBUG  lisp trace & debug     is turned off
TAIL    tail recursion removal is turned on
By default FAST==0 and
PREEMPT task preemption        is turned on
RANGE   range checking         is turned on
CKARGS  argument checking      is turned on
TDEBUG  lisp trace & debug     is turned on
TAIL    tail recursion removal is turned off
```
To otherwise modify the capabilities requires modifying the #defines in alps.c

By default it builds an executable for a X86-64 linux system HOST=LINUX64

You can specify other targets such as an x86 linux system: HOST=LINUX32 if you have a 32 bit build environment.
See [alps.h](src/alps.h) for other targets. The Makefile may need to be modified.
```
$ make [GRAF=1] [SOUND=1] [etc]
```
5) Put the alps directory in your execution path or link the
alps executable into a directory in your current execution path.
6) Copy the file lisp/dotalps to the alps directory as .alps and modify
it to your needs. .alps is read by alps at startup

## Running alps

```
$ cd <path to alps directory>
$ ./alps
./alps Linux 64bit Interpreter V8.34 128MB
loading .alps
loading prims.al
loading dll.al
loading qt.al
alps: 

```

## Using alps
alps is an interpreter that evaluates lisp expressions. By default it
enters a prompt-read-eval-print loop. At the "alps:" prompt enter any
valid expression to evaluate it and print the result.  A semicolon and
all characters following it on a line are ignored. The semicolon is
used to introduce comments so we use it here to annotate the examples.
A lisp expression is either a constant, symbol or an expression in
brackets with 0 or more elements. Any element in a bracket expression
can again be a constant, symbol or another expression in brackets.
The first element in an evaluated bracket expression must always refer
to a function while the remaining elements are its arguments.

Example dialog:

```
alps: (+ 2 2)
4
alps: (+ 1 2 3 4)              ; + takes any number of arguments
10
alps: (+ 1 2 3 (+ 2 2))        ; using an expression as an argument
10
alps: (a Foo [1 2 3 4])        ; assign to the symbol Foo the vector [1 2 3 4]
alps: Foo                      ; examine the value of Foo
[1 2 3 4]                      ; value of Foo printed
alps: (* Foo 10)               ; multiply the elements of Foo by 10
[10 20 30 40]
alps: (a Bar (list 1 2 'Foo Foo  "polly")) ; assign to Bar a list 
alps: Bar                                  ; examine the value of Bar
(1 2 Foo [1 2 3 4]  "polly")
; prepend the symbol Hello to the rest of the list after the first element  
alps: (cons 'Hello (cdr Bar))  
(Hello 2 Foo [1 2 3 4]  "polly")
alps:
```
To exit the interpreter enter the expresion (quit) or send an end-of-file
by entering the appropriate control character (control-d for most unixen).

Example:
```
alps: (quit)

/alps: Session ended per user request.
bash$ 
```

See the file [userdoc.txt](doc/userdoc.txt) for further information.
