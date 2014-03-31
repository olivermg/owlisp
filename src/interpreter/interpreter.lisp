(in-package :owlisp/evaluator)

(export '(owlisp-toplevel))



(defun owlisp-toplevel ()
  (loop
     (format t "~&owlisp> ")
     (finish-output)
     (print (evaluate-form (read)
			   (env.d.extend '())
			   (env.b.extend '())))
     (finish-output)))



(defun load-libraries ())

(defun initialize ())

(defun write-compilation ())

(defun evaluate-defpackage (name env)
  (declare (ignore env))
  (format t "defpackage ~a~%" name))

(defun evaluate-lambda (params body env)
  (format t ".sub 'lambda001'~%")
  (loop
     for p in params
     do (format t "~t.param pmc ~a~%" p))
  (format t "~t.return (123)~%")
  (format t ".end~%")
  (list 'compound-procedure params body env))

(defun evaluate-defun (name params body env)
  (format t "defun ~a ~a ~a~%" name params body)
  (let ((fn `(lambda ,params ,@body)))
    (update-in-environment env name fn)))

(defun evaluate-call (fn args)
  (format t "call ~a ~a~%" fn args)
  (apply fn args))

(defun evaluate-inpackage (name env)
  (format t "in-package ~a~%" name)
  (update-current-package-in-environment env name))

#|
(defun evaluate-let (bindings body env)
  (format t "let ~a ~a~%" bindings body)
  (let ((env-extended env))
    (loop
       for (var value) in bindings
       do (setf env-extended
		(update-in-environment env-extended
				       var
				       (evaluate-form value env-extended))))
    (evaluate-forms body env-extended)))
|#

#|
(defun evaluate-+ (args env)
  (format t "+ ~a~%" args)
  (reduce #'(lambda (sum e)
	      (+ sum (lookup-in-environment env e)))
	  args
	  :initial-value 0))
|#
