(in-package :owlisp/analyzer)

(export '())



(defmacro cps-transform (expr)
  (cond ((self-evaluating-p expr)
	 `(cps-transform-self-evaluating ,expr))

	((quote-p expr)
	 `(cps-transform-quote ,expr))

	((variable-p expr)
	 `(cps-transform-variable ,expr))

	((lambda-p expr)
	 `(cps-transform-lambda ,expr))

	((application-p expr)
	 `(cps-transform-application ,expr))

	(t (error "don't know how to cps transform ~a" expr))))

(defmacro cps-transform-self-evaluating (expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-quote (expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-variable (expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-lambda (expr)
  (let* ((lam (first expr))
	 (arglist (second expr))
	 (body (rest (rest expr)))
	 (k (gensym)) ; TODO: this is not 100% collision safe as argument name
	 (arglist-k (cons k arglist)))
    `(,lam ,arglist-k
	   (funcall ,k
		    (progn
		      ,@body)))))

(defmacro cps-transform-application (expr)
  (let* ((fn (first expr))
	 (paramlist (rest expr))
	 (k (gensym))
	 (paramlist-k (cons k paramlist)))
    `((cps-transform ,fn) ,@paramlist-k)))

(defmacro cps-transform-sequence (exprs &optional (transformed '()))
  exprs)
