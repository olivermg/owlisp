(in-package :owlisp/helper)

(export '(with-walker-definitions
	  walk
	  defwalker-rule))


#|
(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))
|#

(defun walk (definitions expr &optional (userdata '()))
  (format t "walk (in) : ~a ~a~%" expr userdata)
  (let ((result (loop
		   for (testfn . transformfn) in definitions
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr userdata))))))
    (format t "walk (out): ~a~%" result)
    result))

(defmacro with-walker-definitions (name &body body)

  (macrolet ((defwalker-rule (definitions testfn pattern &body transformation)
	       (with-gensyms (form-arg userdata-arg)
		 `(setf ,definitions
			(append ,definitions
				(list (cons ,testfn
					    #'(lambda (,form-arg ,userdata-arg)
						(destructuring-bind
						      ,pattern
						    (list ,form-arg ,userdata-arg)
						  ,@transformation)))))))))

    (let* ((name-upcase (string-upcase name))
	   (rules-symbol (intern (concatenate 'string "*" name-upcase "-WALKER-RULES*")))
	   (walk-symbol (intern (concatenate 'string "WALK-" name-upcase)))
	   (walk-sq-symbol (intern (concatenate 'string "WALK-" name-upcase "-SEQUENCE")))
	   (walk-sq-symbol-last (intern (concatenate 'string "WALK-" name-upcase "-SEQUENCE-LAST"))))

      `(progn

	 (defparameter ,rules-symbol '())

	 (labels ((,walk-symbol (expr)
		    (walk ,rules-symbol
			  expr))

		  (,walk-sq-symbol (expr-list)
		    (mapcar #'(lambda (expr)
				(,walk-symbol expr))
			    expr-list))

		  (,walk-sq-symbol-last (expr-list)
		    (reduce #'(lambda (l expr)
				(declare (ignore l))
				(,walk-symbol expr))
			    expr-list)))

	   ,@body)))))
