(in-package :owlisp/helper)

(export '(with-walker-definitions
	  defwalker-rule
	  walk
	  walk-sequence
	  walk-sequence-last))


#|
(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))
|#

(defun walk-internal (definitions expr &optional (userdata '()))
  (format t "walk-internal (in) : ~a ~a~%" expr userdata)
  (let ((result (loop
		   for (testfn . transformfn) in definitions
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr userdata))))))
    (format t "walk-internal (out): ~a~%" result)
    result))

(defmacro with-walker-definitions (name &body body)

  (let* ((name-upcase (string-upcase name))
	 (rules-symbol (intern (concatenate 'string name-upcase "-WALKER-RULES"))))

    `(progn

       (let ((,rules-symbol '()))

	 (macrolet ((defwalker-rule (testfn pattern &body transformation)
			(with-gensyms (form-arg userdata-arg)
			  `(setf ,',rules-symbol
				 (append ,',rules-symbol
					 (list (cons ,testfn
						     #'(lambda (,form-arg ,userdata-arg)
							 (destructuring-bind
							       ,pattern
							     (list ,form-arg ,userdata-arg)
							   ,@transformation)))))))))

	   (labels ((walk (expr)
		      (walk-internal ,rules-symbol
				     expr))

		    (walk-sequence (expr-list)
		      (mapcar #'(lambda (expr)
				  (walk expr))
			      expr-list))

		    (walk-sequence-last (expr-list)
		      (reduce #'(lambda (l expr)
				  (declare (ignore l))
				  (walk expr))
			      expr-list)))

	     ,@body))))))
