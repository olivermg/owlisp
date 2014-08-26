(in-package :owlisp/analyzer)

(export '())


#|
(defparameter *primitive-functions*
  '(funcall apply + * - / car cdr mapcar print format))
|#


#|
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
|#

(defmacro cps-transform-sequence (head . rest)
  (format t "cps-transform-sequence head:~a rest:~a~%" head rest)
  (let ((result (if (consp rest)
		    `(lambda (k)
		       (funcall (walk ,head)
				(lambda (discard-k)
				  (declare (ignore discard-k))
				  (funcall (cps-transform-sequence ,@rest)
					   k))))
		    `(lambda (k)
		       (funcall (walk ,head)
				k)))))
    (format t "  result: ~a~%" result)
    result))



(defparameter *cps-transformation-definitions* '())


;; self evaluating
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(self-evaluating-p expr))

    expr

  `#'(lambda (k)
       (funcall k ,expr)))


;; quoted expression
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(quote-p expr))

    expr

  `#'(lambda (k)
       (funcall k ,expr)))


;; variable
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(variable-p expr))

    expr

  `#'(lambda (k)
       (funcall k ,expr)))


;; lambda expression
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(lambda-p expr))

    (lam (&rest arglist) &body body)

  (let* ((dyn-k (gensym))
	 (arglist-k (cons dyn-k arglist)))
    `(lambda (k)
       (funcall k
		(,lam ,arglist-k
		      (funcall (cps-transform-sequence ,@body)
			       ,dyn-k))))))


;; application expression
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(application-p expr))

    (fn &rest params)

  (if (primitive-p fn)
      `(lambda (k)
	 (funcall k (,fn ,@params)))
      (labels ((tp (fn k paramsv &rest params)
		 (if (consp params)
		     (let ((newparamv (gensym)))
		       `(funcall (walk ,(car params))
				 (lambda (,newparamv)
				   ,(cl:apply #'tp
					      fn
					      k
					      (append paramsv (list newparamv))
					      (cdr params)))))
		     `(funcall ,fn
			       ,k
			       ,@paramsv))))
	(let ((fn-result (gensym)))
	  `(lambda (k)
	     (funcall (walk ,fn)
		      (lambda (,fn-result)
			,(cl:apply #'tp
				   fn-result
				   'k
				   '()
				   params))))))))


#|
(defwalker-transformation

    #'(lambda (expr)
	(declare (ignore expr))
	t)

    (lambda (&rest args) &body body)

  (declare (ignore lambda))
  (let ((k-arg (gensym)))
    `#'(lambda (,k-arg ,@args) ,@body)))
|#
