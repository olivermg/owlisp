(cl:in-package #:owlisp/builtin/macros)

(cl:export '(*builtin-macro-symbols*
	     defun
	     setf))

(cl:defparameter *builtin-macro-symbols*
  '(defun
    setf))


(cl:defmacro setf (location value)
  (cl:cond ((cl:consp location)
	    (cl:let ((fn-symbol `(cl:setf ,(cl:car location))))
	      `(cl:funcall (cl:function ,fn-symbol) ,value ,@(cl:cdr location))))
	   (cl:t
	    `(cl:setq ,location ,value))))

(cl:defmacro defun (name (cl:&rest arglist) cl:&body body)
  `(owlisp/analyzer::setf (owlisp/analyzer::symbol-function (owlisp/analyzer::quote ,name))
	 (owlisp/analyzer::lambda (,@arglist)
	   ,@body)))
