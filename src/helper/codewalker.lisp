(in-package :owlisp/helper)

(export '(defwalker-transformation
	  walk))


(defparameter *walker-hooks*
  '())


(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))

(defmacro walk (expr)
  (format t "walk (in) : ~a~%" expr)
  (let ((result (loop
		   for (testfn . transformfn) in *walker-hooks*
		   do (if (funcall testfn expr)
			  (return (funcall transformfn expr))))))
    (format t "walk (out): ~a~%" result)
    result))

(defmacro defwalker-transformation (testfn pattern &body transformation)
  (let ((form-arg (gensym)))
    `(setf *walker-hooks*
	   (append *walker-hooks*
		   (list (cons ,testfn
			       #'(lambda (,form-arg)
				   (destructuring-bind
					 (,pattern)
				       (list ,form-arg)
				     ,@transformation))))))))
