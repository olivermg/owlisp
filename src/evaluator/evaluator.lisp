(in-package :owlisp/evaluator)

(export '(evaluate-stdin
	  evaluate-stream
	  evaluate-form
	  evaluate-forms))



(defun symbol-name-equals (symbol name)
  (string-equal (symbol-name symbol)
		(symbol-name name)))



(defun evaluate-stdin (decl-env cfgraph)
  (evaluate-stream *standard-input* decl-env cfgraph))

(defun evaluate-stream (stream decl-env cfgraph)
  (load-libraries)
  (initialize)
  (append
   (define-builtins)
   (list (evaluate-forms (read-stream stream) decl-env cfgraph))
   (write-compilation)))

(defun evaluate-forms-to-last (forms decl-env cfgraph)
  (let ((last-val nil))
    (dolist (form forms last-val)
      (setf last-val (evaluate-form form decl-env cfgraph)))
    last-val))

(defun evaluate-forms-to-list (forms decl-env cfgraph)
  (mapcar #'(lambda (form)
	      (evaluate-form form decl-env cfgraph))
	  forms))

(defun evaluate-forms (forms decl-env cfgraph)
  (evaluate-forms-to-last forms decl-env cfgraph))

#|
(defun evaluate-form (expr env)
  (format t "~%EVALUATE-FORM: ~A~%" expr)
  (cond ((self-evaluating-p expr) expr)
	((quote-p expr) (quoted-text expr))
	((variable-p expr) (lookup-variable-value expr env))
	((lambda-p expr) (make-procedure (lambda-parameters expr)
					 (lambda-body expr)
					 env))
	((let-p expr) (evaluate-forms (let-body expr)
				      (let-environment (let-bindings expr) env)))
	((application-p expr) (apply (evaluate-form (operator expr) env)
				     (evaluate-forms-to-list (operands expr) env)))))
|#

(defun evaluate-form (expr decl-env cfgraph)
  (let* ((analyzed-expr (analyze expr decl-env cfgraph))
	 ;(result (funcall bind-env))
	 )
    ;result
    ;(send-message machine :set-code analyzed-expr)
    ;(format t "~%OPCODES: ~a~%" (send-message machine :get-code))
    analyzed-expr))



(defparameter *indentation* "")

(defun analyze (expr decl-env cfgraph)
  (let ((*indentation*
	 (concatenate 'string *indentation* "  ")))
    (format t "~%~a(" *indentation*)
    (multiple-value-bind
	  (analyzed-expr analyzed-cfgraph)
	(cond ((self-evaluating-p expr)
	       (analyze-self-evaluating expr decl-env cfgraph))

	      ((quote-p expr)
	       (analyze-quote expr decl-env cfgraph))

	      ((variable-p expr)
	       (analyze-variable expr decl-env cfgraph))

	      ((lambda-p expr)
	       (analyze-lambda expr decl-env cfgraph))

	      ((let-p expr)
	       (analyze-let expr decl-env cfgraph))

	      ((if-p expr)
	       (analyze-if expr decl-env cfgraph))

	      ((application-p expr)
	       (analyze-application expr decl-env cfgraph)))

      (format t ")")
      (values analyzed-expr
	      analyzed-cfgraph))))

#|
(defun analyze-bindings (bindings decl-env)
  (mapcar #'(lambda (binding)
	      (let ((var (first binding))
		    (value (second binding)))
		(list var (analyze value decl-env))))
	  bindings))
|#



#|
(defun apply (proc-def args)
  (format t "~%APPLY ~a ~a~%" proc-def args)
  (cond ((compound-procedure-p proc-def)
	 (evaluate-forms (compound-procedure-body proc-def)
			 (make-environment (compound-procedure-env proc-def)
					   (compound-procedure-params proc-def)
					   args)))
	((primitive-procedure-p proc-def)
	 (apply-primitive-procedure (primitive-procedure-implementation proc-def)
				    args))
	(t (error 'unknown-form
		  :name proc-def))))
|#

#|
(defun apply-primitive-procedure (implementation bind-env)
  (cl:apply implementation
	    (send-message bind-env :get-current-bindings)))
|#



(defun is-tagged-list (expr tag)
  (if (consp expr)
      (let ((head (first expr)))
	(if (symbolp head)
	    (symbol-name-equals head tag)))))

#|
(defun primitive-procedure-p (expr)
  (is-tagged-list expr :primitive-procedure))
|#

(defun self-evaluating-p (expr)
  (or
   (numberp expr)
   (stringp expr)
   (null expr)))

(defun variable-p (expr)
  (symbolp expr))

(defun quote-p (expr)
  (is-tagged-list expr :quote))

(defun let-p (expr)
  (is-tagged-list expr :let))

(defun lambda-p (expr)
  (is-tagged-list expr :lambda))

(defun if-p (expr)
  (is-tagged-list expr :if))

(defun application-p (expr)
  (declare (ignore expr))
  t)



(defun quoted-text (expr)
  (second expr))

(defun lookup-variable-address (name decl-env)
  (send-message decl-env :address name))

(defun lookup-variable-value (address bind-env)
  (send-message bind-env :lookup address))

#|
(defun make-procedure (params body env)
  (evaluate-lambda params body env))
|#

(defun lambda-parameters (expr)
  (second expr))

(defun lambda-body (expr)
  (rest (rest expr)))

(defun operator (expr)
  (first expr))

(defun operands (expr)
  (rest expr))

#|
(defun primitive-procedure-implementation (proc-definition)
  (second proc-definition))
|#

#|
(defun let-environment (bindings base-env)
  (make-environment base-env
		    (mapcar #'car bindings)
		    (mapcar (lambda (binding-proc)
			      (funcall binding-proc base-env))
			    (mapcar #'cadr bindings))))
|#

(defun let-bindings (expr)
  (second expr))

(defun let-bindings-vars (expr)
  (mapcar #'first
	  (let-bindings expr)))

(defun let-bindings-vals (expr)
  (mapcar #'second
	  (let-bindings expr)))

(defun let-body (expr)
  (rest (rest expr)))

(defun if-predicate (expr)
  (second expr))

(defun if-then (expr)
  (third expr))

(defun if-else (expr)
  (fourth expr))



(defun analyze-self-evaluating (expr decl-env cfgraph)
  (declare (ignore decl-env))
  (format t "CONSTANT ~a" expr)
  (funcall cfgraph :append-content (CONSTANT expr))
  (values (CONSTANT expr)
	  cfgraph))

(defun analyze-quote (expr decl-env cfgraph)
  (declare (ignore decl-env))
  (format t "CONSTANT ~a" (quoted-text expr))
  (funcall cfgraph :append-content (CONSTANT (quoted-text expr)))
  (values (CONSTANT (quoted-text expr))
	  cfgraph))

(defun analyze-variable (expr decl-env cfgraph)
  (let ((address (lookup-variable-address expr decl-env)))
    (format t "REFERENCE ~a" address)
    (funcall cfgraph :append-content (REFERENCE address))
    (values (REFERENCE address)
	    cfgraph)))

(defun analyze-lambda (expr decl-env cfgraph)
  (format t "ABSTRACTION ...")
  (let* ((params (lambda-parameters expr))
	 (extended-decl-env (env.d.extend params decl-env))
	 (new-cfgraph (make-node :parents (list cfgraph))))
    (multiple-value-bind
	  (analyzed-body analyzed-cfgraph)
	(analyze-sequence (lambda-body expr) extended-decl-env new-cfgraph)
      ;(funcall analyzed-cfgraph :append-content (ABSTRACTION analyzed-body))
      (values (ABSTRACTION analyzed-body)
	      analyzed-cfgraph))))

(defun analyze-let (expr decl-env cfgraph)
  (format t "LET-BINDING ...")
  (let* ((extended-decl-env (env.d.extend (let-bindings-vars expr) decl-env))
	 (analyzed-vals-procs (mapcar #'(lambda (val-expr)
					  (analyze val-expr decl-env cfgraph))
				      (let-bindings-vals expr))))
    (multiple-value-bind
	  (analyzed-body analyzed-cfgraph)
	(analyze-sequence (let-body expr) extended-decl-env cfgraph)
      (values (LET-BINDING analyzed-vals-procs analyzed-body)
	      analyzed-cfgraph))))

(defun analyze-if (expr decl-env cfgraph)
  (format t "ALTERNATIVE ...")
  (multiple-value-bind
	(analyzed-predicate analyzed-cfgraph)
      (analyze (if-predicate expr) decl-env cfgraph)
    ;(funcall analyzed-cfgraph :append-content analyzed-predicate)
    (let* ((then-cfgraph (make-node :parents (list analyzed-cfgraph)))
	   (else-cfgraph (make-node :parents (list analyzed-cfgraph))))
      (multiple-value-bind
	    (analyzed-then analyzed-then-cfgraph)
	  (analyze (if-then expr) decl-env then-cfgraph)
	(multiple-value-bind
	      (analyzed-else analyzed-else-cfgraph)
	    (analyze (if-else expr) decl-env else-cfgraph)
	  (let ((merge-cfgraph (make-node :parents
					  (list analyzed-then-cfgraph
						analyzed-else-cfgraph))))
	    (values (ALTERNATIVE analyzed-predicate analyzed-then analyzed-else)
		    merge-cfgraph)))))))

(defun analyze-application (expr decl-env cfgraph)
  (format t "APPLICATION ...")
  (multiple-value-bind
	(analyzed-operator analyzed-op-cfgraph)
      (analyze (operator expr) decl-env cfgraph)
    (let* ((analyzed-sequence (reduce #'(lambda (cur-result operand)
					  (let* ((prev-operands (car cur-result))
						 (prev-graphs (cdr cur-result))
						 (last-graph (car (last prev-graphs))))
					    (multiple-value-bind
						  (analyzed-operand analyzed-cfgraph)
						(analyze operand decl-env last-graph)
					      (cons (append prev-operands
							    (list analyzed-operand))
						    (append prev-graphs
							    (list analyzed-cfgraph))))))
				      (operands expr)
				      :initial-value (cons '()
							   (list analyzed-op-cfgraph))))
	   (analyzed-operands (car analyzed-sequence))
	   (analyzed-cfgraph (car (last (cdr analyzed-sequence)))))
      (values (APPLICATION analyzed-operator analyzed-operands)
	      analyzed-cfgraph))))

(defun analyze-sequence (exprs decl-env cfgraph)
  (let ((analyzed-body-list (mapcar #'(lambda (expr)
					(analyze expr decl-env cfgraph))
				    exprs)))
    ;(funcall cfgraph :append-content exprs)
    (values (SEQUENCE_ analyzed-body-list)
	    cfgraph)))



(defun CONSTANT (value)
#|
  (lambda (bind-env)
    (declare (ignore bind-env))
    value)
  (list #x10 value)
|#
  (list #x11 value))

(defun REFERENCE (address)
#|
  (lambda (bind-env)
    (lookup-variable-value address bind-env))
  (list #x11 (car address) (cdr address))
|#
  (list #x12 (car address) (cdr address)))

(defun ABSTRACTION (body)
#|
  (lambda (bind-env)
    (lambda (evaluated-args)
      (funcall body (env.b.extend evaluated-args bind-env))))
|#
  (let* ((the-function (append body (RETURN_)))
	 (the-goto (GOTO (length the-function))))
    (append (list #x40 (length the-goto))
	    the-goto
	    the-function)
    (list #x15 the-function)))

(defun LET-BINDING (bound-values-procs body)
#|
  (lambda (bind-env)
    (let* ((runtime-vals (mapcar #'(lambda (bound-value-proc)
				     (funcall bound-value-proc bind-env))
				 bound-values-procs))
	   (extended-bind-env (env.b.extend runtime-vals bind-env)))
      (funcall body extended-bind-env)))
|#
  (list 13 bound-values-procs body))

(defun ALTERNATIVE (predicate then else)
#|
  (lambda (bind-env)
    (if (funcall predicate bind-env)
	(funcall then bind-env)
	(funcall else bind-env)))
  (list 14)
|#
  (let ((then-terminated (append then (JOIN_)))
	(else-terminated (append else (JOIN_))))
    (append predicate
	    (list #x13 then-terminated else-terminated))))

(defun APPLICATION (operator operands)
#|
  (lambda (bind-env)
    (let ((proc-def (funcall operator bind-env)))
      (labels ((evaluate-operands (operands)
		 (if operands
		     (STORE-ARGUMENT (funcall (first operands) bind-env)
				       (evaluate-operands (rest operands)))))) ; TODO: make tail call

	(cond ((functionp proc-def)
	       (funcall proc-def (evaluate-operands (MAKE-FRAME operands))))

	      (t (error "unknown function"))))))
  (labels ((make-push-args (opcodes operands)
	     (if operands
		 (make-push-args (append opcodes (first operands) (list #x01))
				 (rest operands))
		 opcodes)))
    (let ((the-pushes (make-push-args '() operands))
	  (operator-push (append operator (list #x01)))
	  (the-goto (list #x41)))
      (append the-pushes operator-push the-goto)))
|#
  (append
   (list #x11
	 (reduce #'(lambda (r e)
		     (append r (cdr e))) ; Workaround - we need a list of values, but operands are entire opcodes defining constants
		 operands
		 :initial-value nil))
   operator
   (list #x16)))

(defun SEQUENCE_ (body)
#|
  (lambda (bind-env)
    (let ((last-val nil))
      (loop
	 for proc in body
	 do (setf last-val
		  (funcall proc bind-env)))
      last-val))
|#
  (labels ((flatten-sub (flattened remaining) ; Workaround - we need to flatten, because we want the bytecode not to be multiple lists
	     (if remaining
		 (flatten-sub (append flattened (first remaining))
			      (rest remaining))
		 flattened)))
    (flatten-sub '() body)))

(defun GOTO (offset)
  (list #x30 offset))

(defun RETURN_ ()
#|
  (list #x31)
|#
  (list #x17))

(defun JOIN_ ()
  (list #x14))

#|
(defun MAKE-FRAME (size &optional parent-frame)
  (env.b.extend (make-list size)
		parent-frame))

(defun STORE-ARGUMENT (frame value index)
  (send-message frame :set-value value index)
  frame)
|#

(defun MAKE-FRAME (operands)
  operands)

(defun STORE-ARGUMENT (value other-values)
  (cons value other-values))
