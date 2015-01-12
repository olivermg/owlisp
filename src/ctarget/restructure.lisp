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
      (declare (ignore #'walk-sequence-last))

      (defrule
	  #'null*-p
	  obj
	(declare (ignore obj))
	(make-assignment/c :lvalue (next-varname)
			   :value (make-null/c)))

      (defrule
	  #'constant-int*-p
	  obj
	(make-assignment/c :lvalue (next-varname)
			   :value (make-constant-int/c :value (constant-int*-value obj))))

      (defrule
	  #'constant-string*-p
	  obj
	(make-assignment/c :lvalue (next-varname)
			   :value (make-constant-string/c :value (constant-string*-value obj))))

      (defrule
	  #'symbol*-p
	  obj
	(declare (ignore obj))
	(error "not yet implemented"))

      (defrule
	  #'if*-p
	  obj
	(let ((walked-cond (walk (if*-cond obj)))
	      (walked-then (walk (if*-then obj)))
	      (walked-else (walk (if*-else obj))))
	 (make-sequence/c :sequence ; FIXME: don't evaluate then/else depending on cond
			  (list walked-cond
				walked-then
				walked-else
				(make-if/c :cond (get-return-var walked-cond)
					   :then (get-return-var walked-then)
					   :else (get-return-var walked-else)
					   :lvalue (next-varname))))))

      (defrule
	  #'reference*-p
	  obj
	(make-assignment/c :lvalue (next-varname)
			   :value (make-reference/c :frameindex (reference*-frameindex obj)
						    :varindex (reference*-varindex obj))))

      (defrule
	  #'bindings*-p
	  obj
	(let ((set-bindings
	       (loop
		  for (symboladdress expr) in (bindings*-bindings obj)
		  collect
		    (let ((walked-expr (walk expr)))
		      (make-sequence/c
		       :sequence
		       (list walked-expr
			     (make-set-binding/c :frameindex (symbol-address-frameindex symboladdress)
						 :varindex (symbol-address-symbolindex symboladdress)
						 :value (get-return-var walked-expr))))))))
	  (make-sequence/c :sequence (append
				      (list (make-extend-bindings/c :size
								    (length set-bindings)))
				      set-bindings
				      (walk-sequence (bindings*-body obj))))))

      (defrule
	  #'function-reference*-p
	  obj
	(make-assignment/c :lvalue (next-varname)
			   :value (make-function-reference/c :name
							     (function-reference*-name obj))))

      (defrule
	  #'abstraction*-p
	  obj
	(let* ((walked-body (walk-sequence (abstraction*-body obj)))
	       (last-var (get-return-var walked-body))
	       (body-with-return (append walked-body
					 (list (make-return/c :variable-name last-var))))
	       (name (let ((a-name (abstraction*-name obj)))
		       (if a-name
			   a-name
			   (next-procedurename)))))
	  (make-assignment/c :lvalue (next-varname)
			     :value (make-abstraction/c :name name
							:args (abstraction*-args obj)
							:body (make-sequence/c :sequence
									       body-with-return)))))

      (defrule
	  #'application*-p
	  obj
	(let* ((fn-assignment
		(walk (application*-fn obj)))
	       (args-assignments
		(walk-sequence (application*-args obj)))
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
	  obj
	(error "unknown object ~a" obj)))))


(defun do-restructure (expr)
  (funcall *restructure-walker*
	   expr))
