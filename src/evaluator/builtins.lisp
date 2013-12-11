(in-package :owlisp)

(export '(define-builtins))



(defun define-builtins ()
  (owlisp/llvm-ir:define-default-package)
  (owlisp/llvm-ir:define-boxtype)
  (list (owlisp/llvm-ir:define-+)))
