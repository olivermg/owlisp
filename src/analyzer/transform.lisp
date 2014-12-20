(in-package :owlisp/analyzer)

(export '(do-transform))


(defparameter *global-fn-symboltable* (make-symboltable))

(defparameter *transform-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last)
	     (special *symboltable*))

    (labels ((transform-function (args body &optional (name nil))
	       (let ((*symboltable* (make-symboltable :parent *symboltable*)))
		 (declare (special *symboltable*))
		 (when name
		   (add-symbols *global-fn-symboltable* (list name)))
		 (add-symbols *symboltable* args)
		 (let ((transformed-body (walk-sequence body)))
		   (make-abstraction* :name name
				      :args args
				      :body transformed-body))))

	     (transform-function-reference (fn)
	       (if (symbol-address *global-fn-symboltable* fn)
		   (make-function-reference* :name fn)
		   (error "unknown function ~a" fn))))

      (defrule
	  #'null-p
	  (expr nil)
	(declare (ignore expr))
	(make-null*))

      (defrule
	  #'constant-int-p
	  (expr nil)
	(make-constant-int* :value expr))

      (defrule
	  #'constant-string-p
	  (expr nil)
	(make-constant-string* :value expr))

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
	  #'function-p
	  ((func fn) nil)
	(declare (ignore func))
	(if (consp fn)
	    (walk fn)
	    (transform-function-reference fn)))

      (defrule
	  #'application-p
	  ((fn &rest args) nil)
	(let ((transformed-fn (transform-function-reference fn))
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
