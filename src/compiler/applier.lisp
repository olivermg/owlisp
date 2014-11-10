(in-package :owlisp/compiler)

(export '(compile-file*
	  compile-stdin
	  compile-stream
	  compile-forms
	  compile-form))


(defparameter *transformation-chain*
  #'(lambda (expr)
      (let* ((transformed-expr (do-transform expr))
	     (restructured-expr (do-restructure transformed-expr))
;	     (closured-expr (do-closure-conversion transformed-expr))
	     (dumped-expr (do-dump restructured-expr)))
	dumped-expr)))


(defun compile-file* (path)
  (with-open-file (stream path)
    (compile-stream stream)))

(defun compile-stdin ()
  (compile-stream *standard-input*))

(defun compile-stream (stream)
  (compile-forms (read-stream stream)))

(defun compile-forms (forms)
  (mapcar #'(lambda (form)
	      (compile-form form))
	  forms))

(defun compile-form (expr)
  (funcall *transformation-chain*
	   expr))
