(in-package :owlisp/analyzer)

(export '(do-transform))


(defparameter *global-fn-symboltable* (make-symboltable))
(defparameter *symboltable* (make-symboltable))

(defparameter *transform-walker*

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (labels ((transform-function (args body &optional (name nil))
	       (with-extended-symboltable *symboltable* (args)
		 (when (and name
			    (not (symbol-exists-p *global-fn-symboltable* name)))
		   (add-symbols *global-fn-symboltable* (list name)))
		 (let ((transformed-body (walk-sequence body)))
		   (make-abstraction* :name name
				      :args args
				      :body transformed-body))))

	     (transform-function-reference (fn)
	       (let ((fn-symboladdress (find-symbol-address *global-fn-symboltable* fn)))
		 (make-function-reference* :frameindex (symbol-address-frameindex fn-symboladdress)
					   :varindex (symbol-address-symbolindex fn-symboladdress)))))

      (defrule
	  #'null-p
	  expr
	(declare (ignore expr))
	(make-null*))

      (defrule
	  #'constant-int-p
	  expr
	(make-constant-int* :value expr))

      (defrule
	  #'constant-string-p
	  expr
	(make-constant-string* :value expr))

      (defrule
	  #'if-p
	  (if cond then &optional else)
	(declare (ignore if))
	(make-if* :cond (walk cond)
		  :then (walk then)
		  :else (walk else)))

      (defrule
	  #'quote-p
	  (qt symbol)
	(declare (ignore qt))
	(make-symbol* :name symbol))

      (defrule
	  #'reference-p
	  expr
	(let ((symboladdress (find-symbol-address *symboltable* expr)))
	  (make-reference* :frameindex (symbol-address-frameindex symboladdress)
			   :varindex (symbol-address-symbolindex symboladdress))))

      (defrule
	  #'let-p
	  (lt (&rest bindings) &body body)
	(declare (ignore lt))
	(with-extended-symboltable *symboltable* ((mapcar #'car bindings))
	  (let ((transformed-body (walk-sequence body)))
	    (loop
	       for (symbol expr) in bindings
	       collect (list (find-symbol-address *symboltable* symbol)
			     (walk expr)) into transformed-bindings
	       finally (return (make-bindings* :bindings transformed-bindings
					       :body transformed-body))))))

      (defrule
	  #'set-p
	  (st symbol value)
	(declare (ignore st))
	(make-set* :variable (walk symbol)
		   :value (walk value)))

      (defrule
	  #'setf-p
	  (stf location value)
	(declare (ignore stf))
	(make-setf* :location (walk location) ; TODO: insert asserts in places like this, i.e. check that we have a setf-able location
		    :value (walk value)))

      (defrule
	  #'symbol-function-p
	  (smbfn symbol)
	(declare (ignore smbfn))
	(let* ((walked-symbol (walk symbol)) ; TODO: assert walked-symbol is symbol-struct
	       (symbol-name (symbol*-name walked-symbol))
	       (fn-address (progn
			     (when (not (symbol-exists-p *global-fn-symboltable* symbol-name))
			       (add-symbols *global-fn-symboltable* (list symbol-name))) ; FIXME: need to do this in setf context instead and otherwise signal error
			     (find-symbol-address *global-fn-symboltable* symbol-name))))
	  (make-function-reference* :frameindex (symbol-address-frameindex fn-address)
				    :varindex (symbol-address-symbolindex fn-address))))

      (defrule
	  #'lambda-p
	  (lam (&rest arglist) &body body)
	(declare (ignore lam))
	(transform-function arglist body))

      (defrule
	  #'defun-p
	  (defn name (&rest arglist) &body body)
	(declare (ignore defn))
	(transform-function arglist body name))

      (defrule
	  #'funcall-p
	  (func fn &rest args)
	(declare (ignore func))
	(let ((transformed-fn (walk fn))
	      (transformed-args (walk-sequence args)))
	  (make-application* :fn transformed-fn
			     :args transformed-args)))

      (defrule
	  #'function-p
	  (func fn)
	(declare (ignore func))
	(if (consp fn)
	    (walk fn)
	    (transform-function-reference fn)))

      (defrule
	  #'application-p
	  (fn &rest args)
	(let ((transformed-fn (transform-function-reference fn))
	      (transformed-args (walk-sequence args)))
	  (make-application* :fn transformed-fn
			     :args transformed-args)))

      (defrule
	  #'true-p
	  expr
	(error "unknown expression: ~a" expr)))))


(defun do-transform (expr)
  (funcall *transform-walker*
	   expr))
