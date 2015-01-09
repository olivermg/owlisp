(in-package :owlisp/analyzer)

(export '(do-simplify))


(defparameter *simplify-walker*

  (make-walker))

(defun do-simplify (expr)
  (funcall *simplify-walker*
	   expr))
