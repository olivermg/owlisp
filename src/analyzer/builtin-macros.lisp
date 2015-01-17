(cl:in-package #:owlisp/builtin/macros)

(cl:export '(*builtin-macro-symbols*
	     defun
	     setf
	     setq))

(cl:defparameter *builtin-macro-symbols*
  '(defun
    setf
    setq))


(cl:defmacro setq (symbolname value)
  `(owlisp/analyzer::set (owlisp/analyzer::quote ,symbolname)
			 ,value))

(cl:defmacro setf (location value)
  (cl:cond ((cl:consp location)
	    (cl:let ((fn-symbol `(cl:setf ,(cl:car location))))
	      `(cl:funcall (cl:function ,fn-symbol) ,value ,@(cl:cdr location))))
	   (cl:t
	    `(setq ,location ,value))))

(cl:defmacro defun (name (cl:&rest arglist) cl:&body body)
  `(setf (owlisp/analyzer::symbol-function (owlisp/analyzer::quote ,name))
	 (owlisp/analyzer::lambda (,@arglist)
	   ,@body)))
