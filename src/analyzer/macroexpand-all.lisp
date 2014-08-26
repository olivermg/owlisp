(in-package :owlisp/analyzer)

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


(defparameter *macroexpand-rules* '())


(defwalker-rule *macroexpand-rules*

    #'(lambda (expr)
	(lambda-p expr))

    ((lam (&rest arglist) &body body) nil)

  `(,lam ,arglist ,@(mapcar #'(lambda (body-expr)
				(macroexpand
				 `(walk *macroexpand-rules*
					,body-expr)))
			    body)))


(defwalker-rule *macroexpand-rules*

    #'(lambda (expr)
	(application-p expr))

    ((fn &rest paramlist) nil)

  (macroexpand
   `(,fn ,@(mapcar #'(lambda (param-expr)
		       (macroexpand
			`(walk *macroexpand-rules*
			       ,param-expr)))
		   paramlist))))


(defwalker-rule *macroexpand-rules*

    #'(lambda (expr)
	(declare (ignore expr))
	t)

    (expr nil)

  expr)


(defmacro macroexpand-all-ng (expr)
  `(walk *macroexpand-rules*
	 ,expr))
