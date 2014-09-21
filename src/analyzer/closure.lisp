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


(defparameter *static-symbols* '())


(defmacro call-closure (closure &rest args)
  (setf *static-symbols*
	(cons args (cadr closure)))
  ;; TODO: implement separate invoke function
  (let ((closure-var (gensym)))
   `(let ((,closure-var ,closure))
     (funcall (car ,closure-var)
	      ,closure-var
	      ,@args))))

(defmacro closure-lambda ((&rest args) &body body)
  (let ((closure-var (gensym)))
    `(list (lambda (,closure-var ,@args)
	     ,@body)
	   ,*static-symbols*)))
