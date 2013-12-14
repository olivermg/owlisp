(in-package :owlisp)

(export '(define-builtins))



(defun define-builtins ()
  (owlisp/llvm-ir:define-default-package)
  (owlisp/llvm-ir:define-boxtype)
  (owlisp/llvm-ir:define-typemap)
  (owlisp/llvm-ir:define-lookup-type)
  (list (owlisp/llvm-ir:define-+)))
