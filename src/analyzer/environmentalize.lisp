(in-package :owlisp/analyzer)

(export '())


(defparameter *envize-rules* '())


(defwalker-rule *envize-rules*

    #'(lambda (expr)
	(variable-p expr))

    (expr (env))

  (format t "variable: ~a~%" expr)
  `(owlisp/environment:lookup ,env ',expr))


(defwalker-rule *envize-rules*

    #'(lambda (expr)
	(lambda-p expr))

    ((lam (&rest arglist) &body body) (env))

  (declare (ignore env))
  (format t "lambda: (~a ~a ~a)~%" lam arglist body)
  (let ((env-var (gensym)))
    `(,lam (,env-var)
	   ,@(mapcar #'(lambda (body-expr)
			 `(walk *envize-rules*
				,body-expr
				(,env-var)))
		     body))))


#|
(defwalker-rule *envize-rules*

    #'(lambda (expr)
	(and (consp expr)
	     (equal 'owlisp/helper:walk
		    (car expr))))

    ((mcr ruleset mcr-expr &optional userdata) (env))

  (declare (ignore env))
  `(,mcr ,ruleset ,mcr-expr ,userdata))
|#


(defwalker-rule *envize-rules*

    #'(lambda (expr)
	(application-p expr))

    ((fn &rest paramlist) (env))

  (format t "application: ~a ~a~%" fn paramlist)
  `(,fn ,@(mapcar #'(lambda (body-expr)
		      `(walk *envize-rules*
			     ,body-expr
			     (,env)))
		  paramlist)))


(defwalker-rule *envize-rules*

    #'(lambda (expr)
	(declare (ignore expr))
	t)

    (expr (env))

  (declare (ignore env))
  (format t "fallback: ~a~%" expr)
  expr)
