(in-package :owlisp)

(export '(evaluate-stream
	  evaluate-form))



(defun evaluate-stream (stream)
  (loop
     for sexpr
     in (read-stream stream)
     collect (evaluate-form sexpr)))

(defun evaluate-form (sexpr)
  (let ((funname (car sexpr))
	(args (cdr sexpr)))
    (cond ((string= funname "DEFUN")
	   (compile-defun funname args '()))
	  (t (error 'unknown-form
		    :name funname)))))
