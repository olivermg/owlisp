(in-package :owlisp/analyzer)

(export '(walk-closure))


(defparameter *local-variables* '())
(defparameter *static-symbols* '())
(defparameter *referenced-free-variables* '())


(with-walker-definitions closure-conversion

  (defwalker-rule *closure-conversion-definitions*

      #'(lambda (expr)
	  (variable-p expr))

      (expr nil)

    (if (position expr *local-variables*)
	expr
	(progn
	  (setf *referenced-free-variables*
		(cons expr *referenced-free-variables*))
	  `(lookup-symbol ,expr))))


  (defwalker-rule *closure-conversion-definitions*

      #'(lambda (expr)
	  (lambda-p expr))

      ((lam (&rest args) &body body) nil)

    (let* ((*local-variables* args)
	   (*referenced-free-variables* '())
	   (converted-body (let ((*static-symbols* (cons args *static-symbols*)))
			     (walk-closure-sequence body))))
      (if *referenced-free-variables*
	  (make-closure*
	   :code (let ((closure-var (gensym)))
		   `(,lam (,closure-var ,@args)
			  ,@converted-body))
	   :env *static-symbols*)
	  `(,lam (,@args)
		 ,@converted-body))))


  (defwalker-rule *closure-conversion-definitions*

      #'(lambda (expr)
	  (application-p expr))

      ((closure &rest args) nil)

    `(invoke ,(walk-closure closure)
	     ,@(walk-closure-sequence args)))


  (defwalker-rule *closure-conversion-definitions*

      #'(lambda (expr)
	  (declare (ignore expr))
	  t)

      (expr nil)

    expr))


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
