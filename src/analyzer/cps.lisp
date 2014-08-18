(in-package :owlisp/analyzer)

(export '())


(defparameter *primitive-functions*
  '(funcall apply + * - / car cdr mapcar print format))


(defmacro cps-transform (expr)
  (format t "~%cps-transform processing on ~a~%" expr)
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
  (let ((result `(lambda (k)
		   (funcall k ,expr))))
    (format t "  result: ~a~%" result)
    result))

(defmacro cps-transform-quote (expr)
  (format t "cps-transform-quote ~a~%" expr)
  (let ((result `(lambda (k)
		   (funcall k ,expr))))
    (format t "  result: ~a~%" result)
    result))

(defmacro cps-transform-variable (expr)
  (format t "cps-transform-variable ~a~%" expr)
  (let ((result `(lambda (k)
		   (funcall k ,expr))))
    (format t "  result: ~a~%" result)
    result))

(defmacro cps-transform-lambda ((lam (&rest arglist) &rest body))
  (format t "cps-transform-lambda lam:~a arglist:~a body:~a~%" lam arglist body)
  (let ((result (let* ((dyn-k (gensym)) ; TODO: this is not 100% collision safe as argument name
		       (arglist-k (cons dyn-k arglist)))
		  `(lambda (k)
		     (funcall k
			      (,lam ,arglist-k
				    (funcall (cps-transform-sequence ,@body)
					     ,dyn-k)))))))
    (format t "  result: ~a~%" result)
    result))

(defmacro cps-transform-application ((fn &rest params))
  (format t "cps-transform-application fn:~a params:~a~%" fn params)
  (let ((result (if (primitive-p fn)
		    `(lambda (k)
		       (funcall k (,fn ,@params)))
		    (labels ((tp (fn k paramsv &rest params)
			       (if (consp params)
				   (let ((newparamv (gensym)))
				     `(funcall (cps-transform ,(car params))
					       (lambda (,newparamv)
						 ,(cl:apply #'tp
							    fn
							    k
							    (append paramsv (list newparamv))
							    (cdr params)))))
				   `(funcall ,fn
					     ,k
					     ,@paramsv))))
		      `(lambda (k)
			 (funcall (cps-transform ,fn)
				  (lambda (fn-result)
				    ,(cl:apply #'tp
					       'fn-result
					       'k
					       '()
					       params))))))))
    (format t "  result: ~a~%" result)
    result))

(defmacro cps-transform-params (head . rest)
  (format t "cps-transform-params head:~a rest:~a~%" head rest)
  (let ((result (if (consp rest)
		    `(lambda (k)
		       (funcall (cps-transform ,head)
				(lambda ()))))))
    (format t "  result: ~a~%" result)
    result))

(defmacro cps-transform-sequence (head . rest)
  (format t "cps-transform-sequence head:~a rest:~a~%" head rest)
  (let ((result (if (consp rest)
		    `(lambda (k)
		       (funcall (cps-transform ,head)
				(lambda (discard-k)
				  (declare (ignore discard-k))
				  (funcall (cps-transform-sequence ,@rest)
					   k))))
		    `(lambda (k)
		       (funcall (cps-transform ,head)
				k)))))
    (format t "  result: ~a~%" result)
    result))
