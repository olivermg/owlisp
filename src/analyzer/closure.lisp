(in-package :owlisp/analyzer)

(export '())


(defparameter *closure-conversion-definitions* '())


(defun closure-convert-sequence (exprs)
  (if (consp (cdr exprs))
      `(prog2
	   (walk *closure-conversion-definitions*
		 ,(car exprs))
	   (closure-convert-sequence ',(cdr exprs)))
      `(walk *closure-conversion-definitions*
	     ,(car exprs))))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(variable-p expr))

  (expr nil)

  `(lookup-symbol ,expr))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(lambda-p expr))

  ((lam (&rest args) &body body) nil)

  (declare (ignore lam))
  `(closure-lambda ,args (closure-convert-sequence ',body)))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(application-p expr))

    ((closure &rest args) nil)

  `(call-closure ,closure ,@args))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(declare (ignore expr))
	t)

    (expr nil)

  expr)


(defmacro call-closure (closure &rest args)
  `(funcall (car ,closure) ,@args))

(defmacro closure-lambda ((&rest args) &body body)
  (let ((closure-var (gensym)))
    `(macrolet ((get-closure ()
		  ,closure-var)
		(lookup-symbol (symbol)
		  `(gethash ,symbol (cadr ,closure-var))))
       (list (lambda (,closure-var ,@args)
	       ,@body)
	     (let ((bindings-var (make-hash-table)))
	       (loop
		  for arg in ,args
		  do (setf (gethash arg bindings-var)
			   '?))))))) ; TODO: need to access args one level higher
