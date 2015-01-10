(in-package :owlisp/analyzer)

(export '(do-macroexpansion))


(defparameter *global-macro-symboltable* (make-symboltable))

(defparameter *macroexpansion-walker*

  (make-walker

    (declare (ignore #'walk-sequence #'walk-sequence-last))

    (defrule
	#'defmacro-p
	((defm name (&rest arglist) &body body) nil)
      (declare (ignore defm))
      arglist body name)

    (defrule
	#'true-p
	(expr nil)
      expr)))


(defun do-macroexpansion (expr)
  (funcall *macroexpansion-walker*
	   expr))
