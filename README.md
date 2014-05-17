## Introduction

owlisp is a compiler frontend to LLVM that compiles Common Lisp to LLVM-IR code,
which then in turn can be processed further by LLVM Tools to translate it to any
target that LLVM offers.

### Technical background, briefly

To translate from lisp code to LLVM-IR code, owlisp currently (this may be subject
to changes) takes an intermediate step to abstract away some otherwise complex
transformations.

The intermediate step consists of translating the original code into an internal
byte-code representation. This byte-code is then being interpreted by an internal
SECD virtual machine (see http://en.wikipedia.org/wiki/SECD_machine ) and this
finally is supposed to result in the final LLVM-IR code that can be processed
with external programs of the LLVM distribution.

(**NOTE**: The last step of transforming to LLVM-IR code is not yet implemented yet,
see below)

## Blog

For the current status of this project as well as ideas and thoughts that I
have about it, please take a look at my blog:

http://blog.lambda-startup.com

## Status

owlisp is not even alpha yet ;) :
* it does not yet support all language features; currently supported lisp
  features (in their very basic form) include:
    * constants, e.g. ```22```
    * lambda, e.g. ```(lambda () 1)```
    * variable declarations & references (only lexically scoped), e.g. ```(lambda (a) a)```
    * function application, e.g. ```((lambda (a) a) 42)```
    * if-statement, e.g. ```(if 1 2 3)```
* it does not yet compile to LLVM-IR code
* it is still a lisp-1 (meaning, that it does not use separate namespaces
  for variable vs. function bindings

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

3. You can now start a toplevel and evaluate & run lisp expressions, e.g.

   ```lisp
     CL-USER> (owlisp/evaluator:toplevel)
     owlisp> ((lambda (a b) (if a a b)) nil 22)

          (APPLICATION ...
            (ABSTRACTION ...
              (ALTERNATIVE ...
                (REFERENCE (0 . 0))
                (REFERENCE (0 . 0))
                (REFERENCE (0 . 1))))
            (CONSTANT NIL)
            (CONSTANT 22))
        COMPILED CODE: (17 (NIL 22) 21 (18 0 0 19 (18 0 0) (18 0 1) 23) 22)

        STACK: NIL
        ENV: #<CLOSURE (LAMBDA (OWLISP/ENVIRONMENT::MSG)
                         :IN
                         OWLISP/ENVIRONMENT:ENV.B.EXTEND) {100454A03B}>
        CODE: (17 (NIL 22) 21 (18 0 0 19 (18 0 0) (18 0 1) 23) 22)
        DUMP: NIL
        DISASSEMBLED CODE: NIL (LDC (NIL 22)) (LDF (18 0 0 19 (18 0 0) (18 0 1) 23)) (AP)

     RESULT: 22

     owlisp> (exit)

     22
     CL-USER>
   ```

   What happens behind the scenes during evaluation of the expression here is:
   - analysis of the expression that has been entered
   - compilation of the expression to byte-code for an owlisp-internal SECD-VM
   - running the byte-code in the internal SECD-VM
   - printing the result
   - when exiting the toplevel via "(exit)", the result of the last evaluation
     is being returned

   As you can see, there is a lot of debug information that includes the status during
   analysis of the expressions as well as the byte-code and virtual machine status.
   Note, that the result of the evaluation is being printed in the line reading
   **RESULT: 22**.

### Compiler Binary

If you desire, you can also create a binary by running `make` in the root
directory of the project (NOTE: currently this assumes that you are using
SBCL). This will create a binary **build/owlispc**
that, when run, will read lisp code from stdin and after entering **(exit)**
will output the resulting LLVM-IR code to stdout.

#### Example

```
$ ./build/owlispc
owlisp> ((lambda (a b) (if a a b)) 11 22)

...<some debug output>...

RESULT: 11

owlisp> (exit)
$
```

Yay! :)
