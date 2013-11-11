(in-package :owlisp)

(export '(compile-defun/ow))



(defun compile-defun/ow (name args body)
  (declare (ignore args body))
  (format t "define void ~a() {~%" name)
  (format t "entry:~%")
  (format t "~tret~%")
  (format t "}~%"))
