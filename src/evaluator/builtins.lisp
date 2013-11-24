(in-package :owlisp)

(export '(define-builtins))



(defun define-builtins ()
  (owlisp/llvm-ir:define-default-package)
  (list (owlisp/llvm-ir:define-+)))
