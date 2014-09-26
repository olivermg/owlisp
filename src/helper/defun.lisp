(in-package :owlisp/helper)

(export '(defun/destructure))


(defmacro defun/destructure (name (&rest lambda-list) &body body)
  (let ((expr-var (gensym)))
    `(defun ,name (,expr-var)
       (destructuring-bind ,lambda-list ,expr-var
	 ,@body))))
