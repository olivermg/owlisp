(in-package :owlisp/llvm-ir)

(export '(compile-defun))



(defun compile-defun (name args body)
  (declare (ignore args body))
  (format t "define void ~a() {~%" name)
  (format t "entry:~%")
  (format t "~tret~%")
  (format t "}~%"))
