;;;; owlisp.lisp

(in-package #:owlisp)


(defparameter *global-env* (env.extend '()))
(defparameter *symbol-table* (make-hash-table))


(defun ow.compile (expr &optional (env *global-env*))
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
				   (list (ow.compile (car exprs) env))))
      compiled))


(defun ow.compile-reference (symbol &optional (env *global-env*))
  (let ((address (funcall env symbol)))
    (format nil "lookup( ~a, ~a )" (car address) (cdr address))))

(defun ow.compile-atom (value &optional (env *global-env*))
  (declare (ignore env))
  (format nil "~a" value))

(defun ow.compile-quoted (symbol &optional (env *global-env*))
  (declare (ignore env))
  (format nil "quoted: ~a~%" symbol))

(defun ow.compile-abstraction (args body &optional (env *global-env*))
  (let* ((resultvar (ow.next-varname))
	 (procname (ow.next-procedurename))
	 (ext-env (env.extend args env))
	 (compiled-body (format nil "{~%~{  ~a;~%~}}~%" (ow.compile-sequence body ext-env))))
    (setf (gethash procname *symbol-table*)
	  (list procname args ext-env))
    (concatenate 'string
		 (format nil "int ~a() {~%" procname)
		 (format nil "int ~a = ~a~%" resultvar compiled-body)
		 (format nil "return ~a;~%" resultvar)
		 (format nil "}~%"))))

(defun ow.compile-application (operator params &optional (env *global-env*))
  (let ((compiled-operator (ow.compile operator env))
	(compiled-params (ow.compile-sequence params env)))
   (format nil "invoke( ~a, ~{~a~^, ~} );~%" compiled-operator compiled-params)))
