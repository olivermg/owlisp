(in-package :owlisp)

(export '(evaluate-stream/ow
	  evaluate-form/ow))



(defun evaluate-stream/ow (stream)
  (loop
     for sexpr
     in (read-stream/ow stream)
     collect (evaluate-form/ow sexpr)))

(defun evaluate-form/ow (sexpr)
  (let ((funname (car sexpr))
	(args (cdr sexpr)))
    (cond ((string= funname "DEFUN")
	   (compile-defun/ow funname args '()))
	  (t (error 'unknown-form
		    :name funname)))))
