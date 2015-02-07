(in-package :owlisp/compiler)

(export '(compile-file*
	  compile-stdin
	  compile-stream
	  compile-forms
	  eval-form
	  compile-form))


(defparameter *eval-chain*
  #'(lambda (expr)
      (let* ((macroexpanded-expr (do-macroexpansion expr))
	     (cps-converted-expr (do-cps-conversion macroexpanded-expr))
	     (transformed-expr (do-transform cps-converted-expr)))
	transformed-expr)))

(defparameter *compile-chain*
  #'(lambda (expr)
      (let* ((restructured-expr (do-restructure expr))
	     (dumped-expr (do-dump restructured-expr)))
	dumped-expr)))

(defparameter *transformation-chain*
  #'(lambda (expr)
      (let* ((macroexpanded-expr (do-macroexpansion expr))
;	     (simplified-expr (do-simplify expr))
	     (cps-converted-expr (do-cps-conversion macroexpanded-expr))
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

(defun eval-form (expr)
  (funcall *eval-chain*
	   expr))

(defun compile-form (expr)
  (funcall *compile-chain*
	   (eval-form expr)))
