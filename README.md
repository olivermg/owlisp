## Introduction

owlisp is a compiler that compiles Common Lisp to native binaries. To do that, it
makes use of the LLVM compiler platform.

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
* it is still a lisp-1 (meaning, that it does not use separate namespaces
  for variable vs. function bindings

## Motivation

I created this project for my own mere fun and because I was searching for
something that could translate Common Lisp to JavaScript. This would allow
me to write even the client side code of web applications in Lisp.

However, I aim to push this forward until it is a full-fledged compiler,
although I can only work in my spare time on this project.

Feedback is always welcome!

## Prerequisites

For using owlisp (at least in this stage of development) you need:

* SWIG (Version >= 3.0 should work)

* A working common lisp (currently, only SBCL will work) with the packages
  **ASDF**, **CFFI** & **APPLY-ARGV** available.

* LLVM dynamic library in your library search path.
  Currently, owlisp tries to load **libLLVM-3.4.so**; if you want to use a
  different version of LLVM, you will at least have to adjust that in
  **src/compiler/llvm-ir/cffi/loader.lisp** .

* LLVM toolchain in your path (llc, llvm-link, llvm-as)

* LLVM C-API header files (Core.h, BitWriter.h) in system include path or
  /usr/include or /usr/local/include

* CLANG C compiler in your path (clang)

## Usage

### Building owlisp compiler

1. Link the file **owlisp.asd** to wherever your asdf search path is.

2. cd into the root directory of owlisp.

3. Do a ```make```. This will do several things:
   * generate LLVM CFFI bindings via SWIG
   * generate owlisp runtime llvm bytecode
   * compile the owlisp compiler frontend
   After successful completion, you will find the compiler frontend
   (owlisp-frontend) as well as a compilation logfile under ./build/

4. Do a ```make install```. This will copy the compiler frontend, the
   owlisp runtime & a script (owlisp) into the directory ./installed.

5. Try it out, e.g. compile the included test file './tests/compileme.lisp':
   ```$ ./installed/owlisp tests/compileme.lisp```
   After completion, you should find several generated files in the ./tests
   folder, among them a native executable file './tests/compileme'.

### Via REPL

You can also use owlisp as a REPL. Proceed like above until step 4, and then

5. ./installed/owlisp-frontend

6. You can now start a toplevel and evaluate & run lisp expressions, e.g.
   ```lisp
    owlisp> ((lambda (a b) b) 11 22)

	(APPLICATION ...
		(ABSTRACTION ...
			(REFERENCE (0 . 1)))
				(CONSTANT 11)
    ; ModuleID = 'main'

	%struct._value_t = type { i8, i32 }
	%struct._frame_t = type { %struct._frame_t*, [16 x %struct._value_t*] }

	declare %struct._value_t* @new_value_int(i32)

	declare %struct._value_t* @get_binding(%struct._frame_t*, i32, i32)

	declare %struct._frame_t* @new_frame(%struct._frame_t*)

	declare void @set_binding(%struct._frame_t*, i32, i32, %struct._value_t*)

	declare void @init_global_frame()

	declare %struct._frame_t* @get_global_frame()

	declare %struct._frame_t* @extend_global_frame()

	declare %struct._frame_t* @shrink_global_frame()

	define i32 @main() {
		entry:
			call void @init_global_frame()
			%0 = call %struct._value_t* @new_value_int(i32 11)
			%1 = call %struct._value_t* @new_value_int(i32 22)
			%2 = call %struct._frame_t* @get_global_frame()
			%3 = call %struct._frame_t* @new_frame(%struct._frame_t* %2)
			call void @set_binding(%struct._frame_t* %3, i32 0, i32 0, %struct._value_t* %0)
			call void @set_binding(%struct._frame_t* %3, i32 0, i32 1, %struct._value_t* %1)
			%4 = call %struct._value_t* @abstraction1(%struct._frame_t* %2)
			ret i32 0
    }

	define %struct._value_t* @abstraction1(%struct._frame_t*) {
		entry:
			%1 = call %struct._value_t* @get_binding(%struct._frame_t* %0, i32 0, i32 1)
			ret %struct._value_t* %1
    }

    (CONSTANT 22))

	owlisp>
   ```
   You can exit from the REPL by entering ```(exit)```.

Yay! :)
