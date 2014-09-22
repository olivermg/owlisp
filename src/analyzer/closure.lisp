(in-package :owlisp/analyzer)

(export '())


(defparameter *local-variables* '())
(defparameter *closure-conversion-definitions* '())
(defparameter *static-symbols* '())


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

  (if (position expr *local-variables*)
      expr
      `(lookup-symbol ,expr)))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(lambda-p expr))

  ((lam (&rest args) &body body) nil)

  (declare (ignore lam))
  (let ((*local-variables* args))
    `(closure-lambda ,args (closure-convert-sequence ',body))))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(application-p expr))

    ((closure &rest args) nil)

  (let ((*static-symbols* (cons args (cadr closure))))
    `(call-closure ,closure ,@args)))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(declare (ignore expr))
	t)

    (expr nil)

  expr)


(defmacro call-closure (closure &rest args)
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
