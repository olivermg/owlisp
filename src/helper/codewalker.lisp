(in-package :owlisp/helper)

(export '())


(defparameter *walker-hooks*
  (make-hash-table))


(defmacro walk (expr)
  (if (consp expr)
      (let* ((form-name (car expr))
	     (form-hook (gethash form-name *walker-hooks*)))
	(if form-hook
	    (funcall form-hook expr)
	    expr))
      expr))

(defmacro defwalker-transformation ((form-name &rest pattern) &body transformation)
  (let ((form-arg (gensym)))
    `(setf (gethash ',form-name *walker-hooks*)
	   #'(lambda (,form-arg)
	       (destructuring-bind ,pattern
		   (cdr ,form-arg)
		 ,@transformation)))))

(defwalker-transformation
    (fn a b c)
    (+ c b a))
