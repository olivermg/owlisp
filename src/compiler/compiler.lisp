(in-package :owlisp)

(export '(compile-defun/ow))



(defun compile-defun (name args body)
  (owlisp/llvm-ir:compile-defun name args body))
