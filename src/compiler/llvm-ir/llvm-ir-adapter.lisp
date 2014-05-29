(in-package :owlisp/llvm-ir)

(export '())



;; native types
(defun i32 ()
  [i32])


;; functions

(defun define (return-type name &rest parameter-types)
  [.return-type .name .parameter-types])
