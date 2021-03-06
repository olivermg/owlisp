(in-package :owlisp/analyzer)

(export '(do-closure-conversion))


(defparameter *local-variables* '())
(defparameter *static-symbols* '())
(defparameter *referenced-free-variables* '())

(defun is-local-variable (var)
  (position var *local-variables*))

(defparameter *closure-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (defrule
	#'reference*-p
	(obj nil)
      (if (is-local-variable (reference*-symbol obj)) ; TODO: generate distinct object for free or bound references
	  obj
	  (progn
	    (setf *referenced-free-variables*
		  (cons obj *referenced-free-variables*))
	    `(lookup-symbol ,obj))))


    (defrule
	#'abstraction*-p
	(obj nil)
      (let ((args (abstraction*-args obj))
	    (body (abstraction*-body obj)))
	(let* ((*local-variables* args)
	       (*referenced-free-variables* '())
	       (converted-body (let ((*static-symbols* (cons args *static-symbols*)))
				 (walk-sequence body))))
	  (if *referenced-free-variables*
	      (with-gensyms (closure-var)
		(make-closure*
		 :args (cons closure-var args)
		 :body converted-body
		 :env *static-symbols*))
	      (make-abstraction*
	       :args args
	       :body converted-body)))))


    (defrule
	#'application*-p
	(obj nil)
      `(invoke ,(walk (application*-fn obj)) ; TODO: update this
	       ,@(walk-sequence (application*-args obj))))


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

(defun do-closure-conversion (expr)
  (funcall *closure-walker*
	   expr))
