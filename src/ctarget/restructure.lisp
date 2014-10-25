(in-package #:owlisp/c)

(export '(do-restructure))


(defmacro with-index-counters (&body body)
  (with-gensyms (varname-index-var procedurename-index-var)
    `(let ((,varname-index-var 0)
	   (,procedurename-index-var 0))
       (declare (special ,varname-index-var ,procedurename-index-var))
       (let ((*next-varname* #'(lambda ()
				 (declare (special ,varname-index-var))
				 (intern
				  (format nil "VAR~d" (incf ,varname-index-var)))))

	     (*next-procedurename* #'(lambda ()
				       (declare (special ,procedurename-index-var))
				       (intern
					(format nil "PROC~d" (incf ,procedurename-index-var))))))
	 (declare (special *next-varname* *next-procedurename*))
	 ,@body))))


(defparameter *restructure-walker*

  (make-walker

    (declare (ignore #'walk-sequence #'walk-sequence-last))

    (defrule
	#'constant*-p
	(obj nil)
      (declare (special *next-varname*))
      (make-assignment/c :lvalue (funcall *next-varname*)
			 :value (constant*-value obj)))

    (defrule
	#'symbol*-p
	(obj nil)
      (declare (ignore obj))
      (error "not yet implemented"))

    (defrule
	#'reference*-p
	(obj nil)
      (make-reference/c :lvalue (next-varname)
			:symbol (reference*-symbol obj)))

    (defrule
	#'abstraction*-p
	(obj nil)
      (make-assignment/c :lvalue (next-varname)
			 :value (make-abstraction/c :name (next-procedurename)
						    :args (abstraction*-args obj)
						    :body (abstraction*-body obj))))

    (defrule
	#'application*-p
	(obj nil)
      (make-assignment/c :lvalue (next-varname)
			 :value (make-application/c :fn (application*-fn obj)
						    :args (application*-args obj))))

    (defrule
	#'(lambda (obj)
	    (declare (ignore obj))
	    t)
	(obj nil)
      (error "unknown object ~a" obj))))


(defun do-restructure (expr)
  (funcall #'(lambda ()
	       (with-index-counters
		   (funcall *restructure-walker*
			    expr)))))
