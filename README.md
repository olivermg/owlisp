# Introduction

owlisp is a compiler frontend to LLVM that compiles Common Lisp to LLVM-IR code,
which then in turn can be processed further by LLVM Tools to translate it to any
target that LLVM offers.

# Status

owlisp is not even alpha yet ;)
(...so it does not support any language features yet apart from defun and
calling functions)

I created this project for my own mere fun and because I was searching for
something that could translate Common Lisp to JavaScript, so I could also
use Lisp for writing client browser code during web application development.

However, I aim to push this forward until it is a full featured compiler,
although I can only work in my spare time on this project.
But who knows... maybe people out there (you?) get interested in this and
start helping coding ;) .

Feedback is always welcome too!

# Prerequisites

For using owlisp (at least in this stage of development) you need:

 * A working common lisp with at least asdf & cffi.
   The Makefile in the root directory of this project provides a target to
   create a binary. This currently depends on SBCL specifically.

 * LLVM dynamic library in your library search path.
   Currently, owlisp tries to load "libLLVM-3.2.so"; if you want to use a
   different version of LLVM, you will at least have to adjust that in
   src/compiler/llvm-ir/cffi/loader.lisp . Maybe you will also have to
   generate a new src/compiler/llvm-ir/cffi/llvmcffi.lisp by running "make"
   in that very directory.

# Getting started

 1. Link the file owlisp.asd to wherever your asdf search path is.

 2. Start up your Common Lisp (I have only tested SBCL yet) and load owlisp by
      CL-USER> (asdf:load-system :owlisp)

 3. You can now compile something, e.g.
      CL-USER> (with-input-from-string
                   (s "(defun main () (+ 1 2))")
                 (owlisp:evaluate-stream s))
    The compiler will print the result LLVM-IR code to stdout (if you
    want to change this, edit src/compiler/llvm-ir/globals.lisp).

If you desire, you can also create a binary by running make in the root
directory of the project (NOTE: currently this assumes that you are using
SBCL). This will create a binary called "owlispc" in a folder "build"
that, when run, will read lisp code from stdin and after pressing Ctrl+D
will output the resulting LLVM-IR code to stdout.

