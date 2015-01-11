(cl:in-package #:owlisp/builtin/macros)

(cl:export '(*builtin-macro-symbols*
	     defun))

(cl:defparameter *builtin-macro-symbols*
  '(defun))

(cl:defmacro defun (name (cl:&rest arglist) cl:&body body)
  `(setf (symbol-function (quote ,name))
	 (lambda (,@arglist)
	   ,@body)))
