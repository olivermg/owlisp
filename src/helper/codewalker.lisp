(in-package :owlisp/helper)

(export '(make-walker
	  defrule
	  walk
	  walk-sequence
	  walk-sequence-last))


#|
(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))
|#

#|
(defun walk-internal (definitions expr &optional (userdata '()))
  (format t "walk-internal (in) : ~a ~a~%" expr userdata)
  (let ((result (loop
		   for (testfn . transformfn) in definitions
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr userdata))))))
    (format t "walk-internal (out): ~a~%" result)
    result))
|#

(defmacro make-walker (&body body)

  (with-gensyms (rules-var)

    `(let ((,rules-var '()))

       (macrolet ((defrule (testfn pattern &body transformation)
		    (with-gensyms (form-arg userdata-arg)
		      `(setf ,',rules-var
			     (append ,',rules-var
				     (list (cons ,testfn
						 #'(lambda (,form-arg ,userdata-arg)
						     (destructuring-bind
							   ,pattern
							 (list ,form-arg ,userdata-arg)
						       ,@transformation)))))))))

	 (labels ((walk (expr &optional (userdata '()))
		    (loop
		       for (testfn . transformfn) in ,rules-var
		       do (if (funcall testfn expr)
			      (return (funcall transformfn expr userdata)))))

		  (walk-sequence (expr-list)
		    (mapcar #'(lambda (expr)
				(walk expr))
			    expr-list))

		  (walk-sequence-last (expr-list)
		    (reduce #'(lambda (l expr)
				(declare (ignore l))
				(walk expr))
			    expr-list)))

	   ,@body

	   #'(lambda (expr)
	       (walk expr)))))))
