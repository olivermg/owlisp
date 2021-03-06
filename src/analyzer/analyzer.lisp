(in-package :owlisp/analyzer)

(export '(evaluate-stdin
	  evaluate-stream
	  evaluate-form
	  evaluate-forms))



(defun evaluate-file (path decl-env)
  (with-open-file (stream path)
    (evaluate-stream stream
		     decl-env)))

(defun evaluate-stdin (decl-env)
  (evaluate-stream *standard-input* decl-env))

(defun evaluate-stream (stream decl-env)
  (evaluate-forms (read-stream stream) decl-env))

(defun evaluate-forms-to-last (forms decl-env)
  (let ((last-val nil))
    (dolist (form forms last-val)
      (setf last-val (evaluate-form form decl-env)))
    last-val))

(defun evaluate-forms-to-list (forms decl-env)
  (mapcar #'(lambda (form)
	      (evaluate-form form decl-env))
	  forms))

(defun evaluate-forms (forms decl-env)
  (evaluate-forms-to-last forms decl-env))

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

(defun evaluate-form (expr decl-env)
  (let* ((analyzed-expr (analyze expr decl-env))
	 ;(result (funcall bind-env))
	 )
    ;result
    ;(send-message machine :set-code analyzed-expr)
    ;(format t "~%OPCODES: ~a~%" (send-message machine :get-code))
    analyzed-expr))



(defparameter *indentation* "")

(defun analyze (expr &optional (decl-env nil))
  (declare (ignore decl-env))
  (walk-closure
   (walk-cps
    expr)))

(defun analyze-old (expr decl-env)
  (let ((*indentation*
	 (concatenate 'string *indentation* "  ")))
    (format t "~%~a(" *indentation*)
    (multiple-value-bind
	  (analyzed-expr)
	(cond ((self-evaluating-p expr)
	       (analyze-self-evaluating expr decl-env))

	      ((quote-p expr)
	       (analyze-quote expr decl-env))

	      ((variable-p expr)
	       (analyze-variable expr decl-env))

	      ((lambda-p expr)
	       (analyze-lambda expr decl-env))

	      ((let-p expr)
	       (analyze-let expr decl-env))

	      ((if-p expr)
	       (analyze-if expr decl-env))

	      ((application-p expr)
	       (analyze-application expr decl-env)))

      (format t ")")
      (values analyzed-expr))))

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



(defun analyze-self-evaluating (expr decl-env)
  (declare (ignore decl-env))
  (format t "CONSTANT ~a" expr)
  (CONSTANT expr))

(defun analyze-quote (expr decl-env)
  (declare (ignore decl-env))
  (format t "CONSTANT ~a" (quoted-text expr))
  (CONSTANT (quoted-text expr)))

(defun analyze-variable (expr decl-env)
  (let ((address (lookup-variable-address expr decl-env)))
    (format t "REFERENCE ~a" address)
    (REFERENCE address)))

(defun analyze-lambda (expr decl-env)
  (format t "ABSTRACTION ...")
  (let* ((params (lambda-parameters expr))
	 (extended-decl-env (env.d.extend params decl-env))
	 (defined-fn (ABSTRACTION-BEGIN))
	 (analyzed-body (analyze-sequence (lambda-body expr) extended-decl-env)))
    (ABSTRACTION-LEAVE (first (last analyzed-body)))))

(defun analyze-let (expr decl-env)
  (format t "LET-BINDING ...")
  (let* ((extended-decl-env (env.d.extend (let-bindings-vars expr) decl-env))
	 (analyzed-vals-procs (analyze-sequence (let-bindings-vals expr) decl-env))
	 (analyzed-body (analyze-sequence (let-body expr) extended-decl-env)))
    (LET-BINDING analyzed-vals-procs analyzed-body)))

(defun analyze-if (expr decl-env)
  (format t "ALTERNATIVE ...")
  (let* ((predicate-bb (ALTERNATIVE-PREDICATE))
	 (phi-var (ALTERNATIVE-PHI-VAR))
	 (analyzed-predicate (analyze (if-predicate expr) decl-env))
	 (then-bb (ALTERNATIVE-THEN))
	 (analyzed-then (analyze (if-then expr) decl-env))
	 (then-end (ALTERNATIVE-STORE-TO-PHI analyzed-then phi-var))
	 (else-bb (ALTERNATIVE-ELSE))
	 (analyzed-else (analyze (if-else expr) decl-env))
	 (else-end (ALTERNATIVE-STORE-TO-PHI analyzed-else phi-var))
	 (merged-value (ALTERNATIVE-MERGE phi-var predicate-bb analyzed-predicate then-bb else-bb)))
    ;(ALTERNATIVE analyzed-predicate analyzed-then analyzed-else)
    merged-value))

(defun analyze-application (expr decl-env)
  (format t "APPLICATION ...")
  (let ((analyzed-operator (analyze (operator expr) decl-env))
	(analyzed-operands (analyze-sequence (operands expr) decl-env)))
    (APPLICATION analyzed-operator analyzed-operands)))

(defun analyze-sequence (exprs decl-env)
  (let ((analyzed-body-list (mapcar #'(lambda (expr)
					(analyze expr decl-env))
				    exprs)))
    (SEQUENCE_ analyzed-body-list)))



(defun CONSTANT (value)
#|
  (lambda (bind-env)
    (declare (ignore bind-env))
    value)
  (list #x10 value)
|#
#|
  (list #x11 value)
|#
  (TARGET-CONSTANT value))

(defun REFERENCE (address)
#|
  (lambda (bind-env)
    (lookup-variable-value address bind-env))
  (list #x11 (car address) (cdr address))
|#
#|
  (list #x12 (car address) (cdr address))
|#
  (TARGET-REFERENCE (car address) (cdr address)))

#|
(defun ABSTRACTION (body)
|#
#|
  (lambda (bind-env)
    (lambda (evaluated-args)
      (funcall body (env.b.extend evaluated-args bind-env))))
|#
#|
  (let* ((the-function (append body (RETURN_)))
	 (the-goto (GOTO (length the-function))))
    (append (list #x40 (length the-goto))
	    the-goto
	    the-function)
    (list #x15 the-function))
|#
#|
  (TARGET-DEFINE "abstraction"))
|#

(defun ABSTRACTION-BEGIN ()
  (TARGET-DEFINE "abstraction1"))

(defun ABSTRACTION-LEAVE (return-value)
  (TARGET-LEAVE-DEFINE return-value))

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

(defun ALTERNATIVE-PREDICATE ()
  (TARGET-ALTERNATIVE-PREDICATE))

(defun ALTERNATIVE-PHI-VAR ()
  (TARGET-ALTERNATIVE-PHI-VAR))

(defun ALTERNATIVE-STORE-TO-PHI (value phi-var)
  (TARGET-ALTERNATIVE-STORE-TO-PHI value phi-var))

(defun ALTERNATIVE-THEN ()
  (TARGET-ALTERNATIVE-THEN))

(defun ALTERNATIVE-ELSE ()
  (TARGET-ALTERNATIVE-ELSE))

(defun ALTERNATIVE-MERGE (phi-var predicate-bb predicate then-bb else-bb)
  (TARGET-ALTERNATIVE-MERGE phi-var predicate-bb predicate then-bb else-bb))

(defun ALTERNATIVE (predicate then else)
#|
  (lambda (bind-env)
    (if (funcall predicate bind-env)
	(funcall then bind-env)
	(funcall else bind-env)))
  (list 14)
|#
#|
  (let ((then-terminated (append then (JOIN_)))
	(else-terminated (append else (JOIN_))))
    (append predicate
	    (list #x13 then-terminated else-terminated)))
|#
  nil)

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
#|
  (append
   (list #x11
	 (reduce #'(lambda (r e)
		     (append r (cdr e))) ; Workaround - we need a list of values, but operands are entire opcodes defining constants
		 operands
		 :initial-value nil))
   operator
   (list #x16))
|#
  (cl:apply #'TARGET-CALL operator operands))

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
#|
  (labels ((flatten-sub (flattened remaining) ; Workaround - we need to flatten, because we want the bytecode not to be multiple lists
	     (if remaining
		 (flatten-sub (append flattened (first remaining))
			      (rest remaining))
		 flattened)))
    (flatten-sub '() body))
|#
  body)

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
