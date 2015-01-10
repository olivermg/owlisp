(in-package :owlisp/compiler)

(export '(compile-file*
	  compile-stdin
	  compile-stream
	  compile-forms
	  compile-form))


(defparameter *transformation-chain*
  #'(lambda (expr)
      (let* ((simplified-expr (do-simplify expr))
	     (cps-converted-expr (do-cps-conversion simplified-expr))
	     (transformed-expr (do-transform cps-converted-expr))
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
  (concatenate 'string
	       (format nil "#include <stdlib.h>~%")
	       (format nil "#include <owlisp/owlisprt.h>~%")
	       (compile-forms (read-stream stream))))

(defun compile-forms (forms)
  (reduce #'(lambda (str form)
	      (concatenate 'string
			   str
			   (compile-form form)))
	  forms
	  :initial-value ""))

(defun compile-form (expr)
  (funcall *transformation-chain*
	   expr))
