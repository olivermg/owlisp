(in-package :owlisp/analyzer)

(export '())


(defparameter *primitive-functions*
  '(funcall apply + * - / car cdr mapcar print format))


(defmacro cps-transform (expr k)
  (cond ((self-evaluating-p expr)
	 `(cps-transform-self-evaluating ,expr ,k))

	((quote-p expr)
	 `(cps-transform-quote ,expr ,k))

	((variable-p expr)
	 `(cps-transform-variable ,expr ,k))

	((lambda-p expr)
	 `(cps-transform-lambda ,expr ,k))

	((application-p expr)
	 `(cps-transform-application ,expr ,k))

	(t (error "don't know how to cps transform ~a" expr))))

(defmacro cps-transform-self-evaluating (expr k)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-quote (expr k)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-variable (expr k)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-lambda (expr k)
  (let* ((lam (first expr))
	 (arglist (second expr))
	 (body (rest (rest expr)))
	 (dyn-k (gensym)) ; TODO: this is not 100% collision safe as argument name
	 (arglist-k (cons dyn-k arglist)))
    `(,lam ,arglist-k
	   (invoke/k ,k ,dyn-k
		     (cps-transform-sequence ,body ,dyn-k)))))

(defmacro cps-transform-application (expr k)
  (let* ((fn (first expr))
	 (paramlist (rest expr))
	 (paramlist-k (cons k paramlist)))
    ;`((cps-transform ,fn ,k) ,@paramlist-k)
    `(,fn (cps-transform-sequence ,paramlist-k ,k))))

(defmacro cps-transform-sequence (exprs k)
  (if (consp exprs)
      (let* ((e (first exprs))
	     (e* (rest exprs))
	     (cur-k (if (consp e*)
			`(lambda (ret)
			   (declare (ignore ret))
			   (cps-transform-sequence ,e* ,k))
			`(lambda (ret)
			   (funcall ,k ret)))))
	`(cps-transform ,e ,cur-k))))

(defun invoke/k (k fn &rest args)
  (funcall k (cl:apply fn args)))
