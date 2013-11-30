(in-package :owlisp)

(export '(load-libraries
	  initialize
	  compile-defpackage
	  compile-defun
	  compile-call
	  write-compilation))



(defun load-libraries ()
  (owlisp/llvm-ir:load-llvm-library))

(defun initialize ()
  (owlisp/llvm-ir:initialize))

(defun compile-defpackage (name)
  (owlisp/llvm-ir:compile-defpackage name))

(defun compile-defun (name args body)
  (owlisp/llvm-ir:compile-defun name args body))

(defun compile-call (name args)
  (owlisp/llvm-ir:compile-call name args))

(defun write-compilation ()
  (owlisp/llvm-ir:write-compilation))
