(in-package :owlisp)

(export '(evaluate-stdin
	  evaluate-stream
	  evaluate-form
	  evaluate-forms))



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
  (let ((last-val nil))
    (dolist (form forms last-val)
      (setf last-val (evaluate-form form env)))))

(defun evaluate-form (form env)
  (format t "evaluate-form ~a~%" form)
  (if (atom form)
      form
      (case (car form)
	((defpackage) (evaluate-defpackage (cadr form) env))
	((defun) (evaluate-defun (cadr form)
				 (caddr form)
				 `(lambda ,(caddr form)
				    ,@(cdddr form))
				 env))
	((lambda) (let ((params (cadr form))
			(body (cddr form)))
		    #'(lambda (&rest args)
			(evaluate-forms body env))))
	((myprint) (print form))	; useful for debugging
	(t (evaluate-call (car form)
			  (cdr form)
			  env)))))
