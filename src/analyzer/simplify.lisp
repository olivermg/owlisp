(in-package :owlisp/analyzer)

(export '(do-simplify))


(defparameter *simplify-walker*

  (make-walker

    (declare (ignore #'walk-sequence #'walk-sequence-last))))


(defun do-simplify (expr)
  (funcall *simplify-walker*
	   expr))
