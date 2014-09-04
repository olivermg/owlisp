;;;; owlisp.lisp

(in-package #:owlisp)


(defparameter *global-env* (env.extend '()))
(defparameter *varname-index* 0)
(defparameter *procedurename-index* 0)
(defparameter *symbol-table* (make-hash-table))


(defun ow.compile (expr &optional (env *global-env*))
  (cond

    ((symbolp expr) (ow.compile-reference expr env))

    ((atom expr) (ow.compile-atom expr env))

    (t (case (intern (symbol-name (car expr)) 'keyword)

	 (:QUOTE (ow.compile-quoted expr env))

	 (:LAMBDA (ow.compile-abstraction (cadr expr) (cddr expr) env))

	 (t (ow.compile-application (car expr) (cdr expr) env))))))

(defun ow.compile-sequence (exprs &optional (compiled "") (env *global-env*))
  (if exprs
      (ow.compile-sequence (cdr exprs)
			   (concatenate 'string
					compiled
					(ow.compile (car exprs) env))
			   env)
      (format nil "{~%~a}~%" compiled)))


(defun ow.compile-reference (symbol &optional (env *global-env*))
  (let ((address (funcall env symbol)))
    (format nil "lookup( ~a, ~a );~%" (car address) (cdr address))))

(defun ow.compile-atom (value &optional (env *global-env*))
  (format nil "~a;~%" value))

(defun ow.compile-quoted (symbol &optional (env *global-env*))
  (format nil "quoted: ~a~%" symbol))

(defun ow.compile-abstraction (args body &optional (env *global-env*))
  (let ((resultvar (ow.next-varname))
	(procname (ow.next-procedurename))
	(ext-env (env.extend args env)))
    (setf (gethash procname *symbol-table*)
	  (list args ext-env))
    (concatenate 'string
		 (format nil "int ~a() {~%" procname)
		 (format nil "int ~a = ~a" resultvar (ow.compile-sequence body "" ext-env))
		 (format nil "return ~a;~%" resultvar)
		 (format nil "}~%"))))

(defun ow.compile-application (operator params &optional (env *global-env*))
  (let ((procname (ow.next-procedurename)))
    (concatenate 'string
		 (format nil "void* (*~a)() = lookup_procedure( ~a );~%" procname operator)
		 (format nil "invoke( ~a, ~a );~%" procname params))))


(defun ow.next-varname ()
  (intern
   (format nil "var~4,'0d" (incf *varname-index*))))

(defun ow.next-procedurename ()
  (intern
   (format nil "proc~4,'0d" (incf *procedurename-index*))))
