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


(defun ow.compile-reference (symbol &optional (env *global-env*))
  (let ((address (funcall env symbol)))
   (dump (format nil "int ~a = lookup( ~a, ~a );~%" (ow.next-varname) (car address) (cdr address)))))

(defun ow.compile-atom (value &optional (env *global-env*))
  (dump (format nil "int ~a = ~a;~%" (ow.next-varname) value)))

(defun ow.compile-quoted (symbol &optional (env *global-env*))
  (format nil "quoted: ~a~%" symbol))

(defun ow.compile-abstraction (args body &optional (env *global-env*))
  (let ((resultvar (ow.next-varname))
	(procname (ow.next-procedurename)))
    (dump (format nil "int ~a() {~%" procname))
    (dump (format nil "~a = ~a;~%" resultvar body))
    (dump (format nil "return ~a;~%" resultvar))
    (dump (format nil "}~%"))
    (dump (format nil "insert_into_symboltable( ~a );~%" procname))
    (setf (gethash procname *symbol-table*)
	  nil)
    procname))

(defun ow.compile-application (operator params &optional (env *global-env*))
  (let ((procname (ow.next-procedurename)))
    (dump (format nil "void* ~a = lookup_procedure( ~a );~%" procname operator))
    (dump (format nil "invoke( ~a, ~a );~%" procname params))))


(defun ow.next-varname ()
  (intern
   (format nil "var~4,'0d" (incf *varname-index*))))

(defun ow.next-procedurename ()
  (intern
   (format nil "proc~4,'0d" (incf *procedurename-index*))))
