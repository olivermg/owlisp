(in-package :owlisp)

(export '(evaluate-stdin
	  evaluate-stream
	  evaluate-form))



(defun evaluate-stdin ()
  (evaluate-stream *standard-input*))

(defun evaluate-stream (stream)
  (load-libraries)
  (initialize)
  (append
   (define-builtins)
   (evaluate-forms (read-stream stream) nil)
   (write-compilation)))

(defun evaluate-forms (forms env)
  (loop
     for form
     in forms
     collect (evaluate-form form env)))

(defun evaluate-form (form env)
  (if (consp form)
      (case (car form)
	((defpackage) (compile-defpackage (cadr form)))
	((defun) (compile-defun (cadr form)
				(caddr form)
				(evaluate-forms (cdddr form) env)))
	(t (compile-call (car form)
			 (cdr form))))
      form))
