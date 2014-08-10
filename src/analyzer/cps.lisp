(in-package :owlisp/analyzer)

(export '())


(defparameter *primitive-functions*
  '(funcall apply + * - / car cdr mapcar print format))


(defmacro cps-transform (expr)
  (cond ((primitive-p expr)
	 `,expr)

	((self-evaluating-p expr)
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

(defun primitive-p (expr)
  (let ((primitives '(funcall car cdr print + - * / format apply)))
    (if (position expr primitives)
	t
	nil)))

(defmacro cps-transform-self-evaluating (expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-quote (expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-variable (expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-lambda ((lam (&rest arglist) &rest body))
  (let* ((dyn-k (gensym)) ; TODO: this is not 100% collision safe as argument name
	 (arglist-k (cons dyn-k arglist)))
    `(lambda (k)
       (funcall k
		(,lam ,arglist-k
		      (funcall ,dyn-k
			       (cps-transform-sequence ,@body)))))))

(defmacro cps-transform-application (expr)
  (let* ((fn (first expr))
	 (paramlist (rest expr)))
    ;`((cps-transform ,fn ,k) ,@paramlist-k)
    `(lambda (k)
       (,fn (cps-transform-sequence (cons k ,paramlist))))))

(defmacro cps-transform-sequence (head . rest)
  (if (consp rest)
      `(lambda (k)
	 ((cps-transform ,head)
	  (lambda (x)
	    ((cps-transform-sequence ,rest)
	     k))))
      `(lambda (k)
	 (funcall k
		  (cps-transform ,head)))))
