(in-package :owlisp)

(export '(compile-stream/ow))

(defun compile-stream/ow (stream)
  (let ((sexpr-list '()))
    (handler-case
	(do ((sexpr (read stream) (read stream)))
	    (nil)
	  (setf sexpr-list
		(append sexpr-list (list sexpr))))
      (end-of-file (e)
	(declare (ignore e))
	sexpr-list))))
