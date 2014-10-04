(in-package :owlisp/analyzer)

(export '())


(defparameter *transform-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (defrule

	#'self-evaluating-p

	(expr nil)

      (make-constant* :value expr))


    (defrule

	#'quote-p

	(expr nil)

      (make-symbol* :name expr))


    (defrule

	#'variable-p

	(expr nil)

      (make-reference* :symbol expr))


    (defrule

	#'lambda-p

	((lam (&rest arglist) &body body) nil)

      (declare (ignore lam))
      (let ((transformed-body (walk-sequence body)))
	(make-abstraction* :args arglist
			   :body transformed-body)))


    (defrule

	#'application-p

	((fn &rest args) nil)

      (let ((transformed-fn (walk fn))
	    (transformed-args (walk-sequence args)))
	(make-application* :fn transformed-fn
			   :args transformed-args)))


    (defrule

	#'(lambda (expr)
	    (declare (ignore expr))
	    t)

	(expr nil)

      (error "unknown expression: ~a" expr))))
