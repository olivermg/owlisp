(in-package :owlisp)

(export '(evaluate-stdin
	  evaluate-stream
	  evaluate-form
	  evaluate-forms))



(defun evaluate-stdin (env)
  (evaluate-stream *standard-input* env))

(defun evaluate-stream (stream env)
  (load-libraries)
  (initialize)
  (append
   (define-builtins)
   (evaluate-forms (read-stream stream) env)
   (write-compilation)))

(defun evaluate-forms (forms env)
  (let ((last-val nil))
    (dolist (form forms last-val)
      (setf last-val (evaluate-form form env)))))

(defun evaluate-form (form env)
  (format t "~%EVALUATE-FORM: ~A~%" form)
  (cond ((symbolp form) (lookup-in-environment env form))
	((atom form) form)
	(t (case (intern (symbol-name (first form))
			 'keyword)
	     (:QUOTE (second form))
	     (:DEFPACKAGE (evaluate-defpackage (second form) env))
	     (:LAMBDA (let ((params (second form))
			    (body (cddr form)))
			#'(lambda (&rest args)
			    (evaluate-forms body
					    (update-in-environment env params args)))))
	     (:DEFUN (evaluate-defun (second form)
				     (third form)
				     `(lambda ,(third form)
					,@(cdddr form))
				     env))
	     (:MYPRINT (format t "MYPRINT: ~a~%" form))
	     (t (evaluate-call (first form)
			       (rest form)
			       env))))))
