(in-package :owlisp/helper)

(export '(walk
	  defwalker-step))


(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))

(defmacro walk (definitions expr)
  (format t "walk (in) : ~a~%" expr)
  (let ((result (loop
		   for (testfn . transformfn) in definitions
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr))))))
    (format t "walk (out): ~a~%" result)
    result))

(defmacro defwalker-step (definitions testfn pattern &body transformation)
  (let ((form-arg (gensym)))
    `(setf ,definitions
	   (append ,definitions
		   (list (cons ,testfn
			       #'(lambda (,form-arg)
				   (destructuring-bind
					 (,pattern)
				       (list ,form-arg)
				     ,@transformation))))))))
