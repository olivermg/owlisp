(in-package :owlisp)

(export '(read-stream/ow
	  compile-stream/ow))

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

(defun compile-stream/ow (stream)
  (read-stream/ow stream))
