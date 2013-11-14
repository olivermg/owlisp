(in-package :owlisp)

(export '(evaluate-stream
	  evaluate-form))



(defun evaluate-stream (stream)
  (append
   (define-builtins)
   (evaluate-forms (read-stream stream) nil)))

(defun evaluate-forms (forms env)
  (loop
     for form
     in forms
     collect (evaluate-form form env)))

(defun evaluate-form (form env)
  (if (consp form)
      (case (car form)
	((defun) (compile-defun (cadr form)
				(caddr form)
				(evaluate-forms (cdddr form) env)))
	(t (compile-call (car form)
			 (cdr form))))))
