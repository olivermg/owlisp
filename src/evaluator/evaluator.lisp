(in-package :owlisp)

(export '(evaluate-stream
	  evaluate-form))



(defun evaluate-stream (stream)
  (loop
     for form
     in (read-stream stream)
     collect (evaluate-form form nil)))

(defun evaluate-form (form env)
  (declare (ignore env))
  (if (consp form)
      (case (car form)
	((defun) (compile-defun (cadr form)
				(cddr form)
				'()))
	(t (error 'unknown-form
		  :name (car form))))))
