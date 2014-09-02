;;;; owlisp.lisp

(in-package #:owlisp)


(defparameter *global-env* (env.extend '()))


(defun ow.compile (expr &optional (env *global-env*))
  (cond

    ((symbolp expr) (ow.compile-reference expr env))

    ((atom expr) (ow.compile-atom expr env))

    (t (case (intern (symbol-name (car expr)) 'keyword)

	 (:QUOTE (ow.compile-quoted expr env))

	 (:LAMBDA (ow.compile-abstraction (cadr expr) (cddr expr) env))

	 (t (ow.compile-application (car expr) (cdr expr) env))))))


(defun ow.compile-reference (symbol &optional (env *global-env*))
  (dump (format nil "int val = lookup( \"~a\" );~%" symbol)))

(defun ow.compile-atom (value &optional (env *global-env*))
  (dump (format nil "int val = ~a;~%" value)))

(defun ow.compile-quoted (symbol &optional (env *global-env*))
  (format nil "quoted: ~a~%" symbol))

(defun ow.compile-abstraction (args body &optional (env *global-env*))
  (dump (format nil "int fn() {~%"))
  (dump (format nil "set_local_values( ~a );~%" args))
  (dump (format nil "~a;~%" body))
  (dump (format nil "return result;~%"))
  (dump (format nil "}~%")))

(defun ow.compile-application (operator params &optional (env *global-env*))
  (dump (format nil "void* procedure = lookup_procedure( ~a );~%" operator))
  (dump (format nil "invoke( procedure, ~a );~%" params)))
