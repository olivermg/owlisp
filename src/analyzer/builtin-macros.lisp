(cl:in-package #:owlisp/builtin/macros)

(cl:export '(*builtin-macro-symbols*
	     defun))

(cl:defparameter *builtin-macro-symbols*
  '(defun))

(cl:defmacro setf (location value)
  `(list ,location ,value))

(cl:defmacro symbol-function (symbol)
  `(list ,symbol))

(cl:defmacro quote (value)
  `(list ,value))

(cl:defmacro defun (name (cl:&rest arglist) cl:&body body)
  `(owlisp/analyzer::setf (owlisp/analyzer::symbol-function (owlisp/analyzer::quote ,name))
	 (owlisp/analyzer::lambda (,@arglist)
	   ,@body)))
