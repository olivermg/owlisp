(in-package :owlisp/analyzer)

(export '(do-transform))


(defparameter *transform-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last)
	     (special *symboltable*))

    (labels ((transform-function (args body &optional (name nil))
	       (let ((*symboltable* (make-symboltable :parent *symboltable*)))
		 (declare (special *symboltable*))
		 (add-symbols *symboltable* args)
		 (let ((transformed-body (walk-sequence body)))
		   (make-abstraction* :name name
				      :args args
				      :body transformed-body)))))

      (defrule
	  #'self-evaluating-p
	  (expr nil)
	(make-constant* :value expr))

      (defrule
	  #'quote-p
	  (expr nil)
	(make-symbol* :name expr))

      (defrule
	  #'reference-p
	  (expr nil)
	(multiple-value-bind (frameindex varindex)
	    (symbol-address *symboltable* expr)
	  (make-reference* :frameindex frameindex
			   :varindex varindex)))

      (defrule
	  #'lambda-p
	  ((lam (&rest arglist) &body body) nil)
	(declare (ignore lam))
	(transform-function arglist body))

      (defrule
	  #'defun-p
	  ((defn name (&rest arglist) &body body) nil)
	(declare (ignore defn))
	(transform-function arglist body name))

      (defrule
	  #'funcall-p
	  ((func fn &rest args) nil)
	(declare (ignore func))
	(let ((transformed-fn (walk fn))
	      (transformed-args (walk-sequence args)))
	  (make-application* :fn transformed-fn
			     :args transformed-args)))
      (defrule
	  #'application-p
	  ((fn &rest args) nil)
	(let (;(transformed-fn (walk fn))
	      (transformed-fn (make-function-reference* :name fn))
	      (transformed-args (walk-sequence args)))
	  (make-application* :fn transformed-fn
			     :args transformed-args)))

      (defrule
	  #'(lambda (expr)
	      (declare (ignore expr))
	      t)
	  (expr nil)
	(error "unknown expression: ~a" expr)))))


(defun do-transform (expr)
  (let ((*symboltable* (make-symboltable)))
    (declare (special *symboltable*))
    (funcall *transform-walker*
	     expr)))
