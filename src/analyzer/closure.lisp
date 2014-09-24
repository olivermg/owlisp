(in-package :owlisp/analyzer)

(export '())


(defparameter *local-variables* '())
(defparameter *closure-conversion-definitions* '())
(defparameter *static-symbols* '())


(defstruct closure code env)

(defgeneric invoke (obj &rest args))

(defmethod invoke ((obj closure) &rest args)
  (cl:apply (closure-code obj) obj args))


(defstruct environment parent symbols)

(defgeneric lookup (env symbol))

(defmethod lookup ((env environment) (symbol symbol))
  (labels ((lookup-rec (env frameindex)
	     (if (null env)
		 (error "unknown symbol ~a" symbol)
		 (let ((pos (position symbol (environment-symbols env))))
		   (if pos
		       (cons frameindex pos)
		       (lookup-rec (environment-parent env)
				   (1+ frameindex)))))))
    (lookup-rec env 0)))


(defun closure-convert-sequence (exprs)
  (if (consp (cdr exprs))
      (prog2
	  (walk *closure-conversion-definitions*
		(car exprs))
	  (closure-convert-sequence (cdr exprs)))
      (walk *closure-conversion-definitions*
	    (car exprs))))

(defun closure-convert-sequence-list (exprs)
  (if (consp exprs)
      (cons (walk *closure-conversion-definitions*
		  (car exprs))
	    (closure-convert-sequence-list (cdr exprs)))))


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

  (let ((*local-variables* args))
    (make-closure
     :code (let ((closure-var (gensym)))
	     `(,lam (,closure-var ,@(closure-convert-sequence-list args))
		    ,@(let ((*static-symbols* (cons args *static-symbols*)))
			   (closure-convert-sequence-list body))))
     :env *static-symbols*)))


(defwalker-rule *closure-conversion-definitions*

    #'(lambda (expr)
	(application-p expr))

    ((closure &rest args) nil)

  `(invoke ,(walk *closure-conversion-definitions*
		  closure)
	   ,@(closure-convert-sequence-list args)))


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
