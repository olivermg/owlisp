(in-package :owlisp)

(export '(read-stream/ow
	  compile-stream/ow))



(define-condition unknown-form (error)
  ((name :initarg :name
	 :reader unknown-form-name)))



(defun read-stream/ow (stream)
  (let ((sexpr-list '()))
    (handler-case
	(do ((sexpr (read stream) (read stream)))
	    (nil)
	  (setf sexpr-list
		(append sexpr-list (list sexpr))))
      (end-of-file (e)
	(declare (ignore e))
	sexpr-list))))

(defun output-defun/ow (name args body)
  (declare (ignore args body))
  (format t "define void ~a() {~%" name)
  (format t "entry:~%")
  (format t "~tret~%")
  (format t "}~%"))

(defun compile-sexpr/ow (sexpr)
  (let ((funname (car sexpr))
	(args (cdr sexpr)))
    (cond ((string= funname "DEFUN")
	   (output-defun/ow funname args '()))
	  (t (error 'unknown-form
		    :name funname)))))

(defun compile-stream/ow (stream)
  (loop
     for sexpr
     in (read-stream/ow stream)
     collect (compile-sexpr/ow sexpr)))
