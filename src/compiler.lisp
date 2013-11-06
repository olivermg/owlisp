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

(defun compile-sexpr/ow (sexpr)
  (let ((fn (car sexpr))
	(args (cdr sexpr)))
    (format t "fn: ~a - args: ~a~%" fn args)
    fn))

(defun compile-stream/ow (stream)
  (loop
     for sexpr
     in (read-stream/ow stream)
     collect (compile-sexpr/ow sexpr)))
