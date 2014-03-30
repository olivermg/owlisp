(in-package :owlisp/evaluator)

(export '(evaluate-stdin
	  evaluate-stream
	  evaluate-form
	  evaluate-forms))



(defun symbol-name-equals (symbol name)
  (string-equal (symbol-name symbol)
		(symbol-name name)))



(defun evaluate-stdin (env)
  (evaluate-stream *standard-input* env))

(defun evaluate-stream (stream env)
  (load-libraries)
  (initialize)
  (append
   (define-builtins)
   (list (evaluate-forms (read-stream stream) env))
   (write-compilation)))

(defun evaluate-forms-to-last (forms env)
  (let ((last-val nil))
    (dolist (form forms last-val)
      (setf last-val (evaluate-form form env)))
    last-val))

(defun evaluate-forms-to-list (forms env)
  (mapcar (lambda (form)
	    (evaluate-form form env))
	  forms))

(defun evaluate-forms (forms env)
  (evaluate-forms-to-last forms env))

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

(defun evaluate-form (expr env)
  (funcall (analyze expr)
	   env))



(defun analyze (expr)
  (format t "~%ANALYZE: ~A~%" expr)
  (cond ((self-evaluating-p expr) (analyze-self-evaluating expr))
	((quote-p expr) (analyze-quote expr))
	((variable-p expr) (analyze-variable expr))
	((lambda-p expr) (analyze-lambda expr))
	((let-p expr) (analyze-let expr))
	((if-p expr) (analyze-if expr))
	((application-p expr) (analyze-application expr))))

(defun analyze-sequence (exprs)
  (let ((procs (mapcar #'analyze exprs)))
    (lambda (env)
      (let ((last-val nil))
	(loop
	   for proc in procs
	   do (setf last-val
		    (funcall proc env)))
	last-val))))

(defun analyze-bindings (bindings)
  (mapcar (lambda (binding)
	    (let ((var (first binding))
		  (value (second binding)))
	      (list var (analyze value))))
	  bindings))


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

(defun apply-primitive-procedure (implementation args)
  (cl:apply implementation args))



(defun execute-apply (proc-def args)
  (format t "~%EXECUTE-APPLY ~a ~a~%" proc-def args)
  (cond ((compound-procedure-p proc-def)
	 (funcall (compound-procedure-body proc-def)
		  (make-environment (compound-procedure-env proc-def)
				    (compound-procedure-params proc-def)
				    args)))
	((primitive-procedure-p proc-def)
	 (apply-primitive-procedure (primitive-procedure-implementation proc-def)
				    args))
	(t (error 'unknown-form
		  :name proc-def))))



(defun is-tagged-list (expr tag)
  (if (consp expr)
      (let ((head (first expr)))
	(if (symbolp head)
	    (symbol-name-equals head tag)))))

(defun compound-procedure-p (expr)
  (is-tagged-list expr :compound-procedure))

(defun primitive-procedure-p (expr)
  (is-tagged-list expr :primitive-procedure))

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

(defun lookup-variable-value (name env)
  (lookup-in-environment env name))

(defun make-procedure (params body env)
  (evaluate-lambda params body env))

(defun lambda-parameters (expr)
  (second expr))

(defun lambda-body (expr)
  (rest (rest expr)))

(defun operator (expr)
  (first expr))

(defun operands (expr)
  (rest expr))

(defun compound-procedure-params (proc-definition)
  (second proc-definition))

(defun compound-procedure-body (proc-definition)
  (third proc-definition))

(defun compound-procedure-env (proc-definition)
  (fourth proc-definition))

(defun primitive-procedure-implementation (proc-definition)
  (second proc-definition))

(defun let-environment (bindings base-env)
  (make-environment base-env
		    (mapcar #'car bindings)
		    (mapcar (lambda (binding-proc)
			      (funcall binding-proc base-env))
			    (mapcar #'cadr bindings))))

(defun let-bindings (expr)
  (second expr))

(defun let-body (expr)
  (rest (rest expr)))

(defun if-predicate (expr)
  (second expr))

(defun if-then (expr)
  (third expr))

(defun if-else (expr)
  (fourth expr))



(defun analyze-self-evaluating (expr)
  (lambda (env)
    (declare (ignore env))
    expr))

(defun analyze-quote (expr)
  (lambda (env)
    (declare (ignore env))
    (quoted-text expr)))

(defun analyze-variable (expr)
  (lambda (env)
    (lookup-variable-value expr env)))

(defun analyze-lambda (expr)
  (let ((params (lambda-parameters expr))
	(bodyproc (analyze-sequence (lambda-body expr))))
    (lambda (env)
      (make-procedure params bodyproc env))))

(defun analyze-let (expr)
  (let ((bodyproc (analyze-sequence (let-body expr)))
	(bindings (analyze-bindings (let-bindings expr))))
    (lambda (env)
      (funcall bodyproc (let-environment bindings env)))))

(defun analyze-if (expr)
  (let ((predicate-proc (analyze (if-predicate expr)))
	(then-proc (analyze (if-then expr)))
	(else-proc (analyze (if-else expr))))
    (lambda (env)
      (if (funcall predicate-proc env)
	  (funcall then-proc env)
	  (funcall else-proc env)))))

(defun analyze-application (expr)
  (let ((operator-proc (analyze (operator expr)))
	(operands-procs (mapcar #'analyze (operands expr))))
    (lambda (env)
      (execute-apply (funcall operator-proc env)
		     (mapcar (lambda (operand-proc)
			       (funcall operand-proc env))
			     operands-procs)))))
