(in-package :owlisp/analyzer)

(export '())


(defparameter *primitive-functions*
  '(funcall apply + * - / car cdr mapcar print format))


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
	 (dyn-k (gensym)) ; TODO: this is not 100% collision safe as argument name
	 (arglist-k (cons dyn-k arglist)))
    `(lambda (k)
       (funcall k
		(,lam ,arglist-k
		      (funcall ,dyn-k
			       (cps-transform-sequence ,body)))))))

(defmacro cps-transform-application (expr)
  (let* ((fn (first expr))
	 (paramlist (rest expr)))
    ;`((cps-transform ,fn ,k) ,@paramlist-k)
    `(lambda (k)
       (,fn (cps-transform-sequence (cons k ,paramlist))))))

(defmacro cps-transform-sequence (exprs)
  (if (consp exprs)
      (let* ((e (first exprs))
	     (e* (rest exprs))
	     (cur-k (if (consp e*)
			`(lambda (ret)
			   (declare (ignore ret))
			   (cps-transform-sequence ,e*))
			`(lambda (ret)
			   (funcall ret)))))
	;`(cps-transform ,e)
	cur-k)))
