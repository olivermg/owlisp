(in-package :owlisp/llvm-ir)

(export '(compile-defun
	  compile-call))



(defun compile-defun (name args body)
  (declare (ignore args body))
  (format t "define void ~a() {~%" name)
  (format t "entry:~%")
  (format t "~tret~%")
  (format t "}~%"))

(defun compile-call (name args)
  (declare (ignore args))
  (format t "~tcall void ~a()~%" name))
