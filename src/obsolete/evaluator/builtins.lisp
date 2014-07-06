(in-package :owlisp)

(export '(define-builtins))



(defun define-builtins ()
  (owlisp/parrot:define-default-package)
  ;(owlisp/llvm-ir:define-boxtype)
  ;(owlisp/llvm-ir:define-typemap)
  ;(owlisp/llvm-ir:define-lookup-type)
  (list (owlisp/parrot:define-+)))
