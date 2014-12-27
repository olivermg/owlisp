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

(defparameter *cps-walker* ; FIXME: always returns a single(!) kexpr

  (make-walker

    (declare (ignore #'walk-sequence-last))

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
	#'lambda-p
	((lam (&rest arglist) &body body) nil)
      (declare (ignore lam))
      (let ((walked-body (walk-sequence body)))
	(assert (= 1 (length walked-body))) ; FIXME: this does not always hold yet
	(with-gensyms (k dynk)
	  `(lambda (,k)
	     (funcall ,k
		      (lambda (,@arglist ,dynk)
			(funcall ,@walked-body ,dynk)))))))

    (defrule
	#'funcall-p
	((fnc fn arg1) nil)
      (declare (ignore fnc))
      (let ((walked-fn (walk fn))
	    (walked-arg1 (walk arg1)))
	(with-gensyms (k fnv arg1v)
	  `(lambda (,k)
	     (funcall ,walked-fn
		      (lambda (,fnv)
			(funcall ,walked-arg1
				 (lambda (,arg1v)
				   (funcall ,fnv ,arg1v ,k)))))))))

    (defrule
	#'application-p
	((fn arg1) nil)
      (let ((walked-arg1 (walk arg1)))
	(with-gensyms (k arg1v)
	  `(lambda (,k)
	     (funcall ,walked-arg1
		      (lambda (,arg1v)
			(,fn ,arg1v ,k)))))))

    (defrule
	#'(lambda (expr)
	    (declare (ignore expr))
	    t)
	(expr nil)
      (error "don't know how to cps transform expression ~a" expr))))


(defun do-cps-conversion (expr)
  (funcall *cps-walker*
	   expr))
