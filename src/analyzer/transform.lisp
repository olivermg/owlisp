(in-package :owlisp/analyzer)

(export '())


(defparameter *transform-rules* '())


(defun walk-transform (expr)
  (walk *transform-rules*
	expr))

(defun walk-transform-sequence (exprs)
  (mapcar #'walk-transform
	  exprs))


(defwalker-rule *transform-rules*

    #'self-evaluating-p

    (expr nil)

  (make-constant* :value expr))


(defwalker-rule *transform-rules*

    #'quote-p

    (expr nil)

  (make-symbol* :name expr))


(defwalker-rule *transform-rules*

    #'variable-p

    (expr nil)

  (make-reference* :symbol expr))


(defwalker-rule *transform-rules*

    #'lambda-p

    ((lam (&rest arglist) &body body) nil)

  (let ((transformed-body (walk-transform-sequence body)))
    (make-abstraction* :code `#'(,lam (,@arglist) ,@transformed-body))))


(defwalker-rule *transform-rules*

    #'application-p

    ((fn &rest args) nil)

  (let ((transformed-fn (walk-transform fn))
	(transformed-args (walk-transform-sequence args)))
    (make-application* :fn transformed-fn
		       :args transformed-args)))


(defwalker-rule *transform-rules*

    #'(lambda (expr)
	(declare (ignore expr))
	t)

    (expr nil)

  (error "unknown expression: ~a" expr))
