(in-package :owlisp/helper)

(export '())


(defparameter *walker-hooks*
  (make-hash-table))


(defun generalize-symbol (symbol)
  (intern (symbol-name symbol)
	  'keyword))

(defmacro walk (expr)
  (if (consp expr)
      (let* ((form-name (car expr))
	     (form-name-key (generalize-symbol form-name))
	     (form-hook (gethash form-name-key *walker-hooks*)))
	(if form-hook
	    (funcall form-hook expr)
	    expr))
      expr))

(defmacro defwalker-transformation ((form-name &rest pattern) &body transformation)
  (let ((form-arg (gensym)))
    `(setf (gethash (generalize-symbol ',form-name)
		    *walker-hooks*)
	   #'(lambda (,form-arg)
	       (destructuring-bind ,pattern
		   (cdr ,form-arg)
		 ,@transformation)))))

(defwalker-transformation
    (fn a b c)
  `(+ ,c ,b ,a))
