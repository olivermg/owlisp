(in-package :owlisp/analyzer)

(export '())


(defparameter *primitive-functions*
  '(funcall apply + * - / car cdr mapcar print format))


(defmacro cps-transform (expr)
  (format t "cps-transform processing on ~a~%" expr)
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

(defun primitive-p (expr)
  (let ((primitives '(funcall car cdr print + - * / format apply)))
    (if (position expr primitives)
	t
	nil)))

(defmacro cps-transform-self-evaluating (expr)
  (format t "cps-transform-self-evaluating ~a~%" expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-quote (expr)
  (format t "cps-transform-quote ~a~%" expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-variable (expr)
  (format t "cps-transform-variable ~a~%" expr)
  `(lambda (k)
     (funcall k ,expr)))

(defmacro cps-transform-lambda ((lam (&rest arglist) &rest body))
  (format t "cps-transform-lambda lam:~a arglist:~a body:~a~%" lam arglist body)
  (let* ((dyn-k (gensym)) ; TODO: this is not 100% collision safe as argument name
	 (arglist-k (cons dyn-k arglist)))
    `(lambda (k)
       (funcall k
		(,lam ,arglist-k
		      (funcall (cps-transform-sequence ,@body)
			       ,dyn-k))))))

(defmacro cps-transform-application ((fn param))
  ;; TODO: this only implements applications with exactly one parameter. in order for
  ;;       this to be sufficient, we need to transform applications to currying form
  ;;       before.
  ;;       we also need to take care of applications with no parameters at all.
  (format t "cps-transform-application fn:~a param:~a~%" fn param)
  (if (primitive-p fn)
      `(lambda (k)
	 (cl:apply k (,fn ,param)))
      `(lambda (k)
	 (funcall (cps-transform ,fn)
		  (lambda (fn-result)
		    (funcall (cps-transform ,param)
			     (lambda (param-result)
			       (funcall fn-result k param-result))))))))

(defmacro cps-transform-params (head . rest)
  (format t "cps-transform-params head:~a rest:~a~%" head rest)
  (if (consp rest)
      `(lambda (k)
	 (funcall (cps-transform ,head)
		  (lambda ())))))

(defmacro cps-transform-sequence (head . rest)
  (format t "cps-transform-sequence head:~a rest:~a~%" head rest)
  (if (consp rest)
      `(lambda (k)
	 (funcall (cps-transform ,head)
		  (lambda (discard-k)
		    (declare (ignore discard-k))
		    (funcall (cps-transform-sequence ,@rest)
			     k))))
      `(lambda (k)
	 (funcall (cps-transform ,head)
		  k))))
