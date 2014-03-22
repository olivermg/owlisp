## Introduction

owlisp is a compiler frontend to LLVM that compiles Common Lisp to LLVM-IR code,
which then in turn can be processed further by LLVM Tools to translate it to any
target that LLVM offers.

## Blog

For the current status of this project as well as ideas and thoughts that I
have about it, please take a look at my blog:

http://blog.lambda-startup.com

## Status

> NOTE: Currently, the LLVM-IR generation described below does not work,
> since as a first step, I am trying to establish an interpreter and then
> advance to a compiler from there.
>
> Instead of LLVM-IR generation, owlisp will currently try to interpret
> all expressions immediately.

owlisp is not even alpha yet ;)
(...so it does not support any language features yet apart from things like
lambda, let and calling functions)

I created this project for my own mere fun and because I was searching for
something that could translate Common Lisp to JavaScript. This would allow
me to write even the client side code of web applications in Lisp.

However, I aim to push this forward until it is a full-fledged compiler,
although I can only work in my spare time on this project.

Feedback is always welcome!

## Prerequisites

For using owlisp (at least in this stage of development) you need:

* A working common lisp with at least **ASDF** & **CFFI**.

  If you want to use the Makefile in the root directory of this project
  to create a compiler binary, you need SBCL specifically (or adjust
  **make.lisp**).

* LLVM dynamic library in your library search path.
  Currently, owlisp tries to load **libLLVM-3.2.so**; if you want to use a
  different version of LLVM, you will at least have to adjust that in
  **src/compiler/llvm-ir/cffi/loader.lisp** . Maybe you will also have to
  generate a new **src/compiler/llvm-ir/cffi/llvmcffi.lisp** by running `make`
  in that very directory (You will maybe have to adjust the Makefile to
  reflect the paths on your system).

## Usage

### Via REPL

1. Link the file **owlisp.asd** to wherever your asdf search path is.

2. Start up your Common Lisp (I have only tested SBCL yet) and load owlisp by

   ```lisp
     CL-USER> (asdf:load-system :owlisp)
   ```

3. You can now compile something, e.g.

   ```lisp
     CL-USER> (with-input-from-string
                  (s "((lambda (a b) (+ a b)) 22 33)")
                (owlisp/evaluator:evaluate-stream s))
   ```

   The compiler will print the resulting LLVM-IR code to stdout (if you
   want to change this, edit **src/compiler/llvm-ir/globals.lisp**).

### Compiler Binary

If you desire, you can also create a binary by running `make` in the root
directory of the project (NOTE: currently this assumes that you are using
SBCL). This will create a binary **build/owlispc**
that, when run, will read lisp code from stdin and after pressing **Ctrl+D**
will output the resulting LLVM-IR code to stdout.

> NOTE: Currently, when running the binary, you will be presented with a
> REPL that you can use to have expressions interpreted immediately (instead
> of any compilation).

#### Example

```
$ ./build/owlispc
(defun main () (+ 22 33)

  <Ctrl+D pressed>

; ModuleID = 'cl'

define i32 @owadd(i32, i32) {
entry:
  %2 = add i32 %0, %1
  ret i32 %2
}

define i32 @main() {
entry:
  %0 = call i32 @owadd(i32 22, i32 33)
  ret i32 %0
}
```

## Running resulting LLVM-IR code

You can run the resulting LLVM-IR code for example with `lli`, which is
an interpreter for LLVM-IR that comes with the LLVM distribution:

```
$ ./build/owlispc >test.ll
(defun main () (+ 22 33))

  <press Ctrl+D>

$ lli test.ll
$ echo $?
55
```

Yay! :)

For further instructions on how to compile this LLVM-IR code, please refer
to the LLVM documentation on http://llvm.org .
