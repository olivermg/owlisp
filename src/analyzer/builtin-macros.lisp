(cl:in-package #:owlisp/builtin/macros)

(cl:export '(*builtin-macro-symbols*
	     owl/defun))

(cl:defparameter *builtin-macro-symbols*
  '(owl/defun))

(cl:defmacro owl/defun (name (cl:&rest arglist) cl:&body body)
  `(setf (symbol-function (quote ,name))
	 (lambda (,@arglist)
	   ,@body)))
