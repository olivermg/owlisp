(in-package :owlisp/analyzer)

(export '())


(defparameter *closure-conversion-definitions* '())


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(variable-p expr))

  (expr nil)

  )


(defmacro call-closure (&rest args)
  `(funcall (car (get-closure)) ,@args))

(defmacro closure-lambda ((&rest args) &body body)
  (let ((closure-var (gensym)))
    `(macrolet ((get-closure ()
		  ,closure-var)
		(lookup-symbol (symbol)
		  ))
       (lambda (,closure-var ,@args)
	 ,@body))))
