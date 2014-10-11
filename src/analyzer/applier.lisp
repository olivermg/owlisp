(in-package :owlisp/analyzer)

(export '(apply-to-file
	  apply-to-stdin
	  apply-to-stream
	  apply-to-forms
	  apply-to-form))


(defparameter *transformation-chain*
  #'(lambda (expr)
      (let* ((transformed-expr (do-transform expr))
	     (closured-expr (do-closure-conversion transformed-expr)))
	closured-expr)))


(defun apply-to-file (path)
  (with-open-file (stream path)
    (apply-to-stream stream)))

(defun apply-to-stdin ()
  (apply-to-stream *standard-input*))

(defun apply-to-stream (stream)
  (apply-to-forms (read-stream stream)))

(defun apply-to-forms (forms)
  (mapcar #'(lambda (form)
	      (apply-to-form form))
	  forms))

(defun apply-to-form (expr)
  (funcall *transformation-chain*
	   expr))
