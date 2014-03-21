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
   (evaluate-forms (read-stream stream) env)
   (write-compilation)))

(defun evaluate-forms (forms env)
  (let ((last-val nil))
    (dolist (form forms last-val)
      (setf last-val (evaluate-form form env)))
    last-val))

(defun evaluate-forms-map (forms env)
  (mapcar (lambda (form)
	    (evaluate-form form env))
	  forms))

(defun evaluate-form (expr env)
  (format t "~%EVALUATE-FORM: ~A~%" expr)
  (cond ((self-evaluating-p expr) expr)
	((quote-p expr) (quoted-text expr))
	((variable-p expr) (lookup-variable-value expr env))
	((lambda-p expr) (make-procedure (lambda-parameters expr)
					 (lambda-body expr)
					 env))
	((application-p expr) (apply (evaluate-form (operator expr) env)
				     (evaluate-forms-map (operands expr) env)))))
#|
	((symbolp form) (lookup-in-environment env form))
	((atom form) form)
	(t (case (intern (symbol-name (first form))
			 'keyword)
	     (:QUOTE (second form))
	     (:DEFPACKAGE (evaluate-defpackage (second form) env))
	     (:LAMBDA (evaluate-lambda (second form) (cddr form) env))
	     (:DEFUN (evaluate-defun (second form) (third form) (cdddr form) env))
	     (:IN-PACKAGE (evaluate-inpackage (second form) env))
	     (:LET (evaluate-let (second form) (cddr form) env))
	     (:MYPRINT (format t "MYPRINT: ~a~%" (evaluate-forms (rest form) env)))
	     (:+ (evaluate-+ (cdr form) env))
	     (t (evaluate-call (first form)
			       (rest form)
			       env))))))
|#

(defun apply (proc-def args)
  (format t "appply ~a ~a~%" proc-def args)
  (if (compound-procedure-p proc-def)
      (evaluate-forms (procedure-body proc-def)
		      (procedure-env proc-def))
      (evaluate-call proc-def args)))



(defun is-tagged-list (expr tag)
  (if (consp expr)
      (let ((head (first expr)))
	(if (symbolp head)
	    (symbol-name-equals head tag)))))

(defun compound-procedure-p (proc)
  (is-tagged-list proc :procedure))

(defun self-evaluating-p (expr)
  (or
   (numberp expr)
   (stringp expr)))

(defun variable-p (expr)
  (symbolp expr))

(defun quote-p (expr)
  (is-tagged-list expr :quote))

(defun lambda-p (expr)
  (is-tagged-list expr :lambda))

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

(defun procedure-params (proc-definition)
  (first proc-definition))

(defun procedure-body (proc-definition)
  (second proc-definition))

(defun procedure-env (proc-definition)
  (third proc-definition))
