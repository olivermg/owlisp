(in-package :owlisp)

(export '(toplevel-evaluator
	  evaluate-stream
	  evaluate-form))



(defun toplevel-evaluator ()
  (load-libraries)
  (with-input-from-string (s "(defun main () (+ 23 45))")
    (evaluate-stream s)))

(defun evaluate-stream (stream)
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
