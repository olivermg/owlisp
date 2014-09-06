;;;; owlisp.lisp

(in-package #:owlisp)


(defparameter *global-env* (env.extend '()))
(defparameter *symbol-table* (make-hash-table))


(defun ow.compile (expr &optional (env *global-env*))
  (with-dumper
    (ow.compile-expression expr env)))

(defun ow.compile-expression (expr &optional (env *global-env*))
  (cond

    ((symbolp expr) (ow.compile-reference expr env))

    ((atom expr) (ow.compile-atom expr env))

    (t (if (atom (car expr))

	   (case (intern (symbol-name (car expr)) 'keyword)

	     (:QUOTE (ow.compile-quoted expr env))

	     (:LAMBDA (ow.compile-abstraction (cadr expr) (cddr expr) env))

	     (t (ow.compile-application (car expr) (cdr expr) env)))

	   (ow.compile-application (car expr) (cdr expr) env)))))

(defun ow.compile-sequence (exprs &optional (env *global-env*) compiled)
  (if exprs
      (ow.compile-sequence (cdr exprs)
			   env
			   (append compiled
				   (list (ow.compile-expression (car exprs) env))))
      compiled))

(defun ow.compile-sequence-last (exprs &optional (env *global-env*))
  (car (last (ow.compile-sequence exprs env))))


(defun ow.compile-reference (symbol &optional (env *global-env*))
  (let ((address (funcall env symbol)))
    (dump-reference (car address) (cdr address))))

(defun ow.compile-atom (value &optional (env *global-env*))
  (declare (ignore env))
  (dump-constant value))

(defun ow.compile-quoted (symbol &optional (env *global-env*))
  (declare (ignore env))
  (error "don't know yet how to compile symbol ~a" symbol))

(defun ow.compile-abstraction (args body &optional (env *global-env*))
  (let* ((ext-env (env.extend args env))
	 (procname (dump-fndefinition-start))
	 (resultname (ow.compile-sequence-last body ext-env)))
    (setf (gethash procname *symbol-table*)
	  (list procname args ext-env))
    (dump-fndefinition-end resultname)
    procname))

(defun ow.compile-application (operator params &optional (env *global-env*))
  (dump-application (ow.compile-expression operator env)
		    (ow.compile-sequence params env)))
