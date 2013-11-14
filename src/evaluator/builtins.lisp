(in-package :owlisp)

(export '(define-builtins))



(defun define-builtins ()
  (list (owlisp/llvm-ir/builtins:define-+)))
