## Introduction

owlisp is a compiler that compiles Common Lisp to native binaries. To do that, it
generates C code from Lisp and then compiles that C code to native binaries, using
your C compiler.

## News

For the current status of this project as well as ideas and thoughts that I
have about it, please take a look at my blog:

http://blog.lambda-startup.com

and follow the #owlisp twitter account:

http://twitter.com/owlisp

## Status

owlisp is not even alpha yet ;) :
* it does not yet support all language features; currently supported lisp
  features (in their very basic form) include:
    * constant integers, e.g. ```22```
    * anonymous functions, e.g. ```(lambda () 1)```
    * named functions, e.g. ```(defun myfn () 1)```
    * variable declarations & references (only lexically scoped), e.g. ```(lambda (a) a)```
    * function application, e.g. ```((lambda (a) a) 42)```
* it is still a lisp-1 (meaning, that it does not use separate namespaces
  for variable vs. function bindings

## Motivation

I created this project for my own mere fun and because I was searching for
something that could translate Common Lisp to JavaScript. This would allow
me to write even the client side code of web applications in Lisp.

Why compile to C then? Well, once this can be done, one can use LLVM to
compile from C to JavaScript.

I can only work in my spare time on this project, so don't expect a quick
progression on this project.

Feedback is always welcome!

## Prerequisites

### For building it

For building owlisp, you need:

1. CMAKE (Version >= 2.8)

2. C compiler (e.g. gcc or clang)

3. A working common lisp (currently, only SBCL will work) with the packages
   **ASDF**, **CFFI** & **APPLY-ARGV** available.

### For running it

For running owlisp (i.e. compile Common Lisp source code to binaries), you need
prerequisites 2 from above, as well as the owlisp compiler toolchain of course.

## Usage

### Building & installing owlisp

1. Link the file **owlisp.asd** to wherever your asdf search path is.

2. cd into the root directory of owlisp.

3. Do a ```mkdir build && cd build```.

4. Do a ```cmake ..```. This will generate Makefiles that are tailored specifically
   for your system.
   You might want to define an installation prefix: ```cmake -DCMAKE_INSTALL_PREFIX=/my/installdir ..```

5. Do a ```make```. This will do several things:
   * compile the owlisp compiler binaries & toolchain
   * build a static runtime library (libowlisprt.a) that will be linked to any
     Lisp program that you compile with owlisp.

6. Do a ```make install```. This will install the compiler toolchain as well as
   the runtime library, the to your installation prefix (or the cmake default
   installation prefix).

7. Try it, e.g. create a file (e.g. ```test.lisp```) with the following contents:
   ```lisp
   (defun main ()
      ((lambda (a b) b) 11 22))
   ```
   Note that you must defun a function called ```main```. This will be the
   entry point for your program when you run it.
   Compile the file by invoking ```owlisp```:
   ```shell
   $ /path/to/owlisp/bin/owlisp -o test test.lisp
   ```
   Run it:
   ```shell
   $ ./test
   ```
   This example probably won't print much output, but it should at least run
   successfully :) .
