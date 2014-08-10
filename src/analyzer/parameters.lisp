(in-package :owlisp/analyzer)

(export '())


(defclass parameter-container ()
  ((params :initarg :params)))


(defmacro parameter-transform (expr)
  (cond ((lambda-p expr)
	 `(parameter-transform-lambda ,expr))

	((application-p expr)
	 `(parameter-transform-application ,expr))

	(t expr)))


(defmacro parameter-transform-lambda ((lam (&rest params) &rest body))
  (let ((param-obj-var (gensym)))
    `(let ((,param-obj-var ,params))
       (,lam (,param-obj-var)
	     ,@body))))

(defmacro parameter-transform-application ((fn &rest params))
  `(,fn (make-instance 'parameter-container
		       :params ',params)))
