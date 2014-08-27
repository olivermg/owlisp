(in-package :owlisp/helper)

(export '(walk
	  defwalker-rule))


(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))

(defun walk (definitions expr &optional (userdata '()))
  (format t "walk (in) : ~a ~a~%" expr userdata)
  (let ((result (loop
		   for (testfn . transformfn) in definitions
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr userdata))))))
    (format t "walk (out): ~a~%" result)
    result))

#|
(defmacro walk (definitions expr &optional (userdata '()))
  (format t "walk (in) : ~a ~a~%" expr userdata)
  (let ((result (loop
		   for (testfn . transformfn) in (eval definitions)
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr userdata))))))
    (format t "walk (out): ~a~%" result)
    result))
|#

(defmacro defwalker-rule (definitions testfn pattern &body transformation)
  (let ((form-arg (gensym))
	(userdata-arg (gensym)))
    `(setf ,definitions
	   (append ,definitions
		   (list (cons ,testfn
			       #'(lambda (,form-arg ,userdata-arg)
				   (format t "destructuring pattern ~a on (~a ~a)~%" ',pattern ,form-arg ,userdata-arg)
				   (destructuring-bind
					 ,pattern
				       (list ,form-arg ,userdata-arg)
				     ,@transformation))))))))
