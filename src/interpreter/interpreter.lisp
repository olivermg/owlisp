(in-package :owlisp/evaluator)

(export '(toplevel))



(defun is-exit-command (expr)
  (and (consp expr)
       (and (= (length expr) 1)
	    (symbolp (car expr)))
       (string-equal "EXIT"
		     (symbol-name (car expr)))))

(defun toplevel ()
  (let ((last-result nil))
    (loop
       (format t "~&owlisp> ")
       (finish-output)
       (let ((expr (read)))
	 (if (is-exit-command expr)

	     (return)

	     (let* ((cfgraph (make-node))
		    (code (evaluate-form
			   expr
			   (make-initialized-declaration-environment)
			   ;(make-initialized-binding-environment)
			   cfgraph))
		    (machine (make-default-machine code)))
	       (format t "~%COMPILED CODE: ~a~%~%" code)
	       (format t "~a~%"
		       (funcall machine :print))
	       (format t "GRAPH: ~a~%" (funcall (funcall cfgraph :root-node) :print))
	       (setf last-result
		     (funcall machine :run))
	       (format t "RESULT: ~a~%~%" last-result)
	       (finish-output)))))
    last-result))



(defun make-initialized-declaration-environment ()
  (env.d.extend (primitive-procedure-names)))

(defun make-initialized-binding-environment ()
  (env.b.extend (primitive-procedure-objects)))

(defun primitive-procedures ()
  `((:print . ,#'cl:print)
    (:+ . ,#'cl:+)
    (:- . ,#'cl:-)
    (:* . ,#'cl:*)
    (:/ . ,#'cl:/)
    (:list . ,#'cl:list)
    (:car . ,#'cl:car)
    (:cdr . ,#'cl:cdr)))

(defun primitive-procedure-names ()
  (mapcar #'car
	  (primitive-procedures)))

(defun primitive-procedure-implementations ()
  (mapcar #'cdr
	  (primitive-procedures)))

(defun primitive-procedure-objects ()
  (mapcar #'(lambda (proc-impl)
	      (lambda (args)
		(cl:apply proc-impl args)))
	  (primitive-procedure-implementations)))



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
