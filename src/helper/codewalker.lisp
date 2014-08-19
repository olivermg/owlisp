(in-package :owlisp/helper)

(export '())


(defparameter *walker-hooks*
  '())


(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))

(defmacro walk (expr)
  (loop
     for (testfn transformfn) in *walker-hooks*
     do (if (funcall testfn expr)
	    (return (funcall transformfn expr)))))

(defmacro defwalker-transformation (testfn pattern &body transformation)
  (let ((form-arg (gensym)))
    `(setf *walker-hooks*
	   (append *walker-hooks*
		   (list ,testfn
			 #'(lambda (,form-arg)
			     (destructuring-bind
				   ,pattern
				 ,form-arg
			       ,@transformation)))))))

(defwalker-transformation
    #'(lambda (expr)
	(declare (ignore expr))
	t)
    (lambda (&rest args) &body body)
  (declare (ignore lambda))
  (let ((k-arg (gensym)))
    `#'(lambda (,k-arg ,@args) ,@body)))
