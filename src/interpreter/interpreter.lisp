(in-package :owlisp)

(export '(owlisp-toplevel))



(defun owlisp-toplevel ()
  (loop (format t "~&owlisp> ")
     (print (evaluate-form (read) nil))))



(defun load-libraries ())

(defun initialize ())

(defun write-compilation ())

(defun evaluate-defpackage (name env)
  (declare (ignore env))
  (format t "defpackage ~a~%" name))

(defun evaluate-lambda (params body env)
  #'(lambda (&rest args)
      (evaluate-forms body
		      (update-in-environment env params args))))

(defun evaluate-defun (name params body env)
  (format t "defun ~a ~a ~a~%" name params body)
  (let ((fn `(lambda ,params ,@body)))
    (update-in-environment env name fn)))

(defun evaluate-call (name args env)
  (format t "call ~a ~a~%" name args)
  (let ((fn (evaluate-form (lookup-in-environment env name) env)))
    (if fn
	(apply fn args)
	(error 'unknown-form))))

(defun evaluate-inpackage (name env)
  (update-current-package-in-environment env name))
