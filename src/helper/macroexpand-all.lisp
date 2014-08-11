(in-package :owlisp/helper)

(export '(macroexpand-all))


(defmacro macroexpand-all (expr)
  `',(mea expr))

(defun mea (expr)
  (let ((me (macroexpand expr)))
    (if (consp me)

	;; recursive solution using map:
	(if (equalp (car me)
		    'function)

	    (cons
	     (cons (car me)
		   (list (caadr me)))
	     (mapcar #'mea
		     (cdadr me)))

	    (mapcar #'mea
		    me))

#|
	;; custom recursive solution:
	(cons (mea (car expr))
	      (mea (cdr expr)))
|#

#|
	;; iterative solution:
	(loop
	   for mee in me
	   collect (mea mee))
|#

    me)))