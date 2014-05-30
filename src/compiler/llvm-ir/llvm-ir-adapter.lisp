(in-package :owlisp/llvm-ir)

(export '())



;; native types
(defun i32 ()
  "i32")

(defun i64 ()
  "i64")


;; functions

(defun define (return-type name &rest parameter-types)
  (format nil "define ~a @~a(~{~a~^, ~})"
	  return-type name parameter-types))
