(in-package :owlisp/analyzer)

(export '())


(defparameter *cps-transformation-definitions* '())

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

(defmacro cps-transform-sequence (head . rest)
  (format t "cps-transform-sequence head:~a rest:~a~%" head rest)
  (let ((result (if (consp rest)
		    `(lambda (k)
		       (funcall (walk *cps-transformation-definitions*
				      ,head)
				(lambda (discard-k)
				  (declare (ignore discard-k))
				  (funcall (cps-transform-sequence ,@rest)
					   k))))
		    `(lambda (k)
		       (funcall (walk *cps-transformation-definitions*
				      ,head)
				k)))))
    (format t "  result: ~a~%" result)
    result))
|#

(defun cps-transform-sequence (seq)
  (let ((k-var (gensym))
	(head (car seq))
	(rest (cdr seq)))
    (format t "cps-transform-sequence head:~a rest:~a~%" head rest)
    (let ((result (if (consp rest)
		      (let ((k-discard-var (gensym)))
			`(lambda (,k-var)
			   (funcall ,(walk *cps-transformation-definitions*
					   head)
				    (lambda (,k-discard-var)
				      (declare (ignore ,k-discard-var))
				      (funcall ,(cps-transform-sequence rest)
					       ,k-var)))))
		      `(lambda (,k-var)
			 (funcall ,(walk *cps-transformation-definitions*
					 head)
				  ,k-var)))))
      (format t "cps-transform-sequence result: ~a~%" result)
      result)))



;; self evaluating
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(self-evaluating-p expr))

    (expr nil)

  (format t "self-evaluating-p: ~a~%" expr)
  (let ((k-var (gensym)))
    (let ((result `(lambda (,k-var)
		     (funcall ,k-var ,expr))))
      (format t "self-evaluating-p result: ~a~%" result)
      result)))


;; quoted expression
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(quote-p expr))

    (expr nil)

  (format t "quote-p: ~a~%" expr)
  (let ((result (let ((k-var (gensym)))
		  `(lambda (,k-var)
		     (funcall ,k-var ,expr)))))
    (format t "quote-p result: ~a~%" result)
    result))


;; variable
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(variable-p expr))

    (expr nil)

  (format t "variable-p: ~a~%" expr)
  (let ((result (let ((k-var (gensym)))
		  `(lambda (,k-var)
		     (funcall ,k-var ,expr)))))
    (format t "variable-p result: ~a~%" result)
    result))


;; lambda expression
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(lambda-p expr))

    ((lam (&rest arglist) &body body) nil)

  (format t "lambda-p: ~a ~a ~a~%" lam arglist body)
  (let ((result (let* ((k-var (gensym))
		       (dyn-k (gensym))
		       (arglist-k (cons dyn-k arglist)))
		  `(lambda (,k-var)
		     (funcall ,k-var
			      (,lam ,arglist-k
				    (funcall ,(cps-transform-sequence body)
					     ,dyn-k)))))))
    (format t "lambda-p result: ~a~%" result)
    result))


#|
;; invocations of codewalker macro
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(and (consp expr)
	     (eql 'owlisp/helper:walk
		  (car expr))))

    ((mcr ruleset mcr-expr &optional userdata) nil)

  `(,mcr ,ruleset
	 (walk *cps-transformation-definitions*
	       ,mcr-expr)
	 ,userdata))
|#

;; application expression
(defwalker-rule *cps-transformation-definitions*

    #'(lambda (expr)
	(application-p expr))

    ((fn &rest params) nil)

  (format t "application-p: ~a ~a~%" fn params)
  (let* ((k-var (gensym))
	 (result (if (primitive-p fn)
		     `(lambda (,k-var)
			(funcall ,k-var (,fn ,@params)))
		     (labels ((tp (fn k paramsv &rest params)
				(if (consp params)
				    (let ((newparamv (gensym)))
				      `(funcall ,(walk *cps-transformation-definitions*
						       (car params))
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
			 `(lambda (,k-var)
			    (funcall ,(walk *cps-transformation-definitions*
					    fn)
				     (lambda (,fn-result)
				       ,(cl:apply #'tp
						  fn-result
						  k-var
						  '()
						  params)))))))))
    (format t "application-p result: ~a~%" result)
    result))


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
