(in-package :owlisp)

(export '(evaluate-stream
	  evaluate-form))



(defun evaluate-stream (stream)
  (loop
     for form
     in (read-stream stream)
     collect (evaluate-form form nil)))

(defun evaluate-form (form env)
  (if (consp form)
      (case (car form)
	((defun) (compile-defun (cadr form)
				(evaluate-form (cddr form) env)
				'()))
	(t (compile-call (car form)
			 (cadr form))))))
