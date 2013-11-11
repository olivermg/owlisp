(in-package :owlisp)

(export '(unknown-form
	  read-stream/ow))



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
