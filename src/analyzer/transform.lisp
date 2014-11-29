(in-package :owlisp/analyzer)

(export '(do-transform))


(defparameter *transform-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last)
	     (special *symboltable*))

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
      (multiple-value-bind (frameindex varindex)
	  (symbol-address *symboltable* expr)
	(make-reference* :frameindex frameindex
			 :varindex varindex)))

    (defrule
	#'lambda-p
	((lam (&rest arglist) &body body) nil)
      (declare (ignore lam))
      (let ((*symboltable* (make-symboltable :parent *symboltable*)))
	(declare (special *symboltable*))
	(add-symbols *symboltable* arglist)
	(let ((transformed-body (walk-sequence body)))
	  (make-abstraction* :args arglist
			     :body transformed-body))))

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


(defun do-transform (expr)
  (let ((*symboltable* (make-symboltable)))
    (declare (special *symboltable*))
    (funcall *transform-walker*
	     expr)))
