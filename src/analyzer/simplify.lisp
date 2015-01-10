(in-package :owlisp/analyzer)

(export '(do-simplify))


(defparameter *simplify-walker*

  (make-walker

    (declare (ignore #'walk-sequence #'walk-sequence-last))

    (defrule
	#'(lambda (expr)
	    (declare (ignore expr))
	    t)
	(expr nil)
      expr)))

(defun do-simplify (expr)
  (funcall *simplify-walker*
	   expr))
