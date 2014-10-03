(in-package :owlisp/analyzer)

(export '())

(defparameter *local-variables* '())
(defparameter *static-symbols* '())
(defparameter *referenced-free-variables* '())

(defparameter *closure-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (defrule

	#'(lambda (expr)
	    (variable-p expr))

	(expr nil)

      (if (position expr *local-variables*)
	  expr
	  (progn
	    (setf *referenced-free-variables*
		  (cons expr *referenced-free-variables*))
	    `(lookup-symbol ,expr))))


    (defrule

	#'(lambda (expr)
	    (lambda-p expr))

	((lam (&rest args) &body body) nil)

      (let* ((*local-variables* args)
	     (*referenced-free-variables* '())
	     (converted-body (let ((*static-symbols* (cons args *static-symbols*)))
			       (walk-sequence body))))
	(if *referenced-free-variables*
	    (make-closure*
	     :code (let ((closure-var (gensym)))
		     `(,lam (,closure-var ,@args)
			    ,@converted-body))
	     :env *static-symbols*)
	    `(,lam (,@args)
		   ,@converted-body))))


    (defrule

	#'(lambda (expr)
	    (application-p expr))

	((closure &rest args) nil)

      `(invoke ,(walk closure)
	       ,@(walk-sequence args)))


    (defrule

	#'(lambda (expr)
	    (declare (ignore expr))
	    t)

	(expr nil)

      expr)))


#|
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
|#
