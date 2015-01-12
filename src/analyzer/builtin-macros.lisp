(cl:in-package #:owlisp/builtin/macros)

(cl:export '(*builtin-macro-symbols*
	     defun))

(cl:defparameter *builtin-macro-symbols*
  '(defun))


(cl:defmacro defun (name (cl:&rest arglist) cl:&body body)
  `(owlisp/analyzer::setf (owlisp/analyzer::symbol-function (owlisp/analyzer::quote ,name))
	 (owlisp/analyzer::lambda (,@arglist)
	   ,@body)))
