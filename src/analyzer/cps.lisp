(in-package :owlisp/analyzer)

(export '(do-cps-conversion))


;;;
;;; NOTE:
;;; here we have basically two distinct lambda constructs that we create and are dealing with:
;;;  1. continuation (denoted by 'k')
;;;     this is a lambda expression that expects a result value of the preceding expression
;;;  2. continuation expression (denoted by 'kexpr')
;;;     this is a lambda expression that expects a contination to call after a result value has
;;;     been computed for the expression that this kexpr represents
;;;


(defparameter *cps-walker* ; FIXME: always return a single(!) kexpr

  (make-walker

    (declare (ignore #'walk-sequence-last))

    (labels ((cps-convert-sequence (walked-seq inner-body-provide-fn &optional (argseq '()))
	       (if walked-seq
		   (with-gensyms (v)
		     `(funcall ,(car walked-seq)
			       (lambda (,v)
				 ,(cps-convert-sequence (cdr walked-seq)
							inner-body-provide-fn
							(append argseq
								(list v))))))
		   (funcall inner-body-provide-fn argseq))))

      (defrule
	  #'constant-int-p
	  (expr nil)
	(with-gensyms (k)
	  `(lambda (,k)
	     (funcall ,k ,expr))))

      (defrule
	  #'constant-string-p
	  (expr nil)
	(with-gensyms (k)
	  `(lambda (,k)
	     (funcall ,k ,expr))))

      (defrule
	  #'reference-p
	  (expr nil)
	(with-gensyms (k)
	  `(lambda (,k)
	     (funcall ,k ,expr))))

      (defrule
	  #'if-p
	  ((if cond then &optional else) nil)
	(declare (ignore if))
	(let ((walked-cond (walk cond))
	      (walked-then (walk then))
	      (walked-else (walk else)))
	  (with-gensyms (k condv)
	    `(lambda (,k)
	       (funcall ,walked-cond
			(lambda (,condv)
			  (if ,condv
			      (funcall ,walked-then
				       ,k)
			      (funcall ,walked-else
				       ,k))))))))

      (defrule
	  #'lambda-p
	  ((lam (&rest arglist) &body body) nil)
	(declare (ignore lam))
	(let ((walked-body (walk-sequence body)))
	  (with-gensyms (k dynk)
	    `(lambda (,k)
	       (funcall ,k
			(lambda (,@arglist ,dynk)
			  ,(cps-convert-sequence walked-body
						 (lambda (args)
						   `(funcall ,dynk
							     ,(first (last args)))))))))))

      (defrule
	  #'defun-p		   ; TODO: implement defun-conversion?
	  ((defn name (&rest arglist) &body body) nil)
	(declare (ignore defn))
	`(defun ,name (,@arglist)
	   ,@(walk-sequence body)))

      (defrule
	  #'funcall-p
	  ((fnc fn &rest args) nil)
	(declare (ignore fnc))
	(let* ((walked-fn (walk fn))
	       (walked-args (walk-sequence args))
	       (walked-all (cons walked-fn walked-args)))
	  (with-gensyms (k)
	    `(lambda (,k)
	       ,(cps-convert-sequence walked-all
				      (lambda (args)
					`(funcall ,(car args) ,@(cdr args) ,k)))))))

      (defrule
	  #'application-p
	  ((fn &rest args) nil)
	(let ((walked-args (walk-sequence args)))
	  (with-gensyms (k)
	    `(lambda (,k)
	       ,(cps-convert-sequence walked-args
				      (lambda (args)
					`(,fn ,@args ,k)))))))

      (defrule
	  #'(lambda (expr)
	      (declare (ignore expr))
	      t)
	  (expr nil)
	(error "don't know how to cps transform expression ~a" expr)))))


(defun do-cps-conversion (expr)
  (funcall *cps-walker*
	   expr))
