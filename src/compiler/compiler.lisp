(in-package :owlisp)

(export '(compile-defun
	  compile-call))



(defun compile-defun (name args body)
  (owlisp/llvm-ir:compile-defun name args body))

(defun compile-call (name args)
  (owlisp/llvm-ir:compile-call name args))
