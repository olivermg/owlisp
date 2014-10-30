(in-package #:owlisp/c)

(export '(do-restructure))


(defmacro with-index-counters (&body body)

  (with-gensyms (varname-index-var procedurename-index-var)

    `(let ((,varname-index-var 0)
	   (,procedurename-index-var 0))

       (labels ((next-varname ()
		  (intern
		   (format nil "VAR~d" (incf ,varname-index-var))))

		(next-procedurename ()
		  (intern
		   (format nil "PROC~d" (incf ,procedurename-index-var)))))

	 ,@body))))


(defparameter *restructure-walker*

  (with-index-counters

    (make-walker
      (declare (ignore #'walk-sequence #'walk-sequence-last))

      (defrule
	  #'constant*-p
	  (obj nil)
	(make-assignment/c :lvalue (next-varname)
			   :value (constant*-value obj)))

      (defrule
	  #'symbol*-p
	  (obj nil)
	(declare (ignore obj))
	(error "not yet implemented"))

      (defrule
	  #'reference*-p
	  (obj nil)
	(make-assignment/c :lvalue (next-varname)
			   :value (make-reference/c :symbol (reference*-symbol obj))))

      (defrule
	  #'abstraction*-p
	  (obj nil)
	(make-assignment/c :lvalue (next-varname)
			   :value (make-abstraction/c :name (next-procedurename)
						      :args (abstraction*-args obj)
						      :body (walk-sequence (abstraction*-body obj)))))

      (defrule
	  #'application*-p
	  (obj nil)
	(let* ((fn-assignment
		(make-assignment/c :lvalue (next-varname)
				   :value (walk (application*-fn obj))))
	       (args-assignments
		(mapcar #'(lambda (arg)
			    (make-assignment/c :lvalue (next-varname)
					       :value arg))
			(walk-sequence (application*-args obj))))
	       (result-assignment
		(make-assignment/c :lvalue (next-varname)
				   :value (make-application/c :fn (assignment/c-lvalue fn-assignment)
							      :args (mapcar #'assignment/c-lvalue
									    args-assignments)))))
	  (make-sequence/c :sequence (append
				      (cons fn-assignment
					    args-assignments)
				      (list result-assignment)))))

      (defrule
	  #'(lambda (obj)
	      (declare (ignore obj))
	      t)
	  (obj nil)
	(error "unknown object ~a" obj)))))


(defun do-restructure (expr)
  (funcall *restructure-walker*
	   expr))
