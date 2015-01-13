(in-package #:owlisp/analyzer)

(export '(do-macroexpansion))


(defparameter *global-macro-symboltable* (make-symboltable :symbols owl/m:*builtin-macro-symbols*))
;(defparameter *backtick-level* 0)
;(defparameter *unquote-count* 0)


(defparameter *convert-builtin-macros-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (defrule
	#'defun-p
	(defn name (&rest arglist) &body body)
      (declare (ignore defn))
      (let ((walked-body (walk-sequence body)))
	`(owl/m:defun ,name (,@arglist)
	   ,@walked-body)))))


(defparameter *macroexpansion-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (defrule
	#'defmacro-p
	(defm name (&rest arglist) &body body)
      (declare (ignore defm))
      (add-symbols *global-macro-symboltable*
		   (list name))
      (eval `(defmacro ,name (,@arglist) ; TODO: implement our own macro logic instead of relying on our host CL implementation
	       ,@body))
      nil) ; FIXME: introduce process to ignore expression in subsequent passes / to abort further processing

    (defrule
	#'application-p
	(fn &rest args)
      (let* ((walked-args (walk-sequence args))
	     (expr `(,fn ,@walked-args)))
	(if (symbol-exists-p *global-macro-symboltable* fn)
	    (walk (macroexpand-1 expr)) ; TODO: see hint in 'defmacro' rule | don't to recursive macroexpand, since we don't want to expand macros that are part of our host CL implementation
	    expr)))))


(defun do-macroexpansion (expr)
  (funcall *macroexpansion-walker*
	   (funcall *convert-builtin-macros-walker*
		    expr)))
