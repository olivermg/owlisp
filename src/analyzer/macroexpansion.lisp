(in-package :owlisp/analyzer)

(export '(do-macroexpansion))


(defparameter *global-macro-symboltable* (make-symboltable))
;(defparameter *backtick-level* 0)
;(defparameter *unquote-count* 0)

(defparameter *macroexpansion-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (defrule
	#'defmacro-p
	((defm name (&rest arglist) &body body) nil)
      (declare (ignore defm))
      (add-symbols *global-macro-symboltable*
		   (list name))
      (eval `(defmacro ,name (,@arglist) ; TODO: implement our own macro logic instead of relying on our host CL implementation
	       ,@body))
      nil) ; FIXME: introduce process to ignore expression in subsequent passes

    (defrule
	#'application-p
	((fn &rest args) nil)
      (let* ((walked-args (walk-sequence args))
	     (expr `(,fn ,@walked-args)))
	(if (symbol-exists-p *global-macro-symboltable* fn)
	    (macroexpand expr)   ; TODO: see hint in 'defmacro' rule
	    expr)))

    (defrule
	#'true-p
	(expr nil)
      expr)))


(defun do-macroexpansion (expr)
  (funcall *macroexpansion-walker*
	   expr))
