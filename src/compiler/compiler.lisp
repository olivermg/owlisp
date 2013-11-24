(in-package :owlisp)

(export '(compile-defpackage
	  compile-defun
	  compile-call
	  write-compilation))



(defun compile-defpackage (name)
  (owlisp/llvm-ir:compile-defpackage name))

(defun compile-defun (name args body)
  (owlisp/llvm-ir:compile-defun name args body))

(defun compile-call (name args)
  (owlisp/llvm-ir:compile-call name args))

(defun write-compilation ()
  (owlisp/llvm-ir:write-compilation))
