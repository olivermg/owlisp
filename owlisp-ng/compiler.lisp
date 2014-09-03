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
  (dump (format nil "int ~a = lookup( \"~a\" );~%" (ow.next-varname) symbol)))

(defun ow.compile-atom (value &optional (env *global-env*))
  (dump (format nil "int ~a = ~a;~%" (ow.next-varname) value)))

(defun ow.compile-quoted (symbol &optional (env *global-env*))
  (format nil "quoted: ~a~%" symbol))

(defun ow.compile-abstraction (args body &optional (env *global-env*))
  (let ((resultvar (ow.next-varname)))
    (dump (format nil "int ~a() {~%" (ow.next-procedurename)))
    (dump (format nil "set_local_values( ~a );~%" args))
    (dump (format nil "~a = ~a;~%" resultvar body))
    (dump (format nil "return ~a;~%" resultvar))
    (dump (format nil "}~%"))))

(defun ow.compile-application (operator params &optional (env *global-env*))
  (let ((procname (ow.next-procedurename)))
    (dump (format nil "void* ~a = lookup_procedure( ~a );~%" procname operator))
    (dump (format nil "invoke( ~a, ~a );~%" procname params))))


(defparameter *varname-index* 0)
(defparameter *procedurename-index* 0)

(defun ow.next-varname ()
  (format nil "v~d" (incf *varname-index*)))

(defun ow.next-procedurename ()
  (format nil "p~d" (incf *procedurename-index*)))
