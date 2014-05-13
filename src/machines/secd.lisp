(in-package :owlisp/machines)

(export '())



(defun make-default-machine-secd (the-code)

  (let ((stack '())
	(env nil)
	(code the-code)
	(dump '())
	(pc 0)
	(interpretation-fn nil)
	(disassemble-fn nil))

    (labels ((next-byte ()
	       (let ((curbyte (car code)))
		 (incf pc)
		 (setf code
		       (rest code))
		 curbyte))

	     (get-code ()
	       code)

	     (reset ()
	       (setf pc 0)
	       (setf code the-code)
	       (setf stack '())
	       (setf env nil)
	       (setf dump '()))

	     (run+ ()
	       (reset)
	       (labels ((step-instruction ()
			    (funcall interpretation-fn (next-byte)))
			(run-instruction ()
			  (when code
			    (step-instruction)
			    (run-instruction))
			  (first stack)))
		 (run-instruction)))

	     (gotoa-code (absolute-address)
	       (setf pc absolute-address)
	       (let ((temp-code the-code))
		 (dotimes (n absolute-address temp-code)
		   (setf temp-code (rest temp-code)))))

	     (gotor-code (relative-address)
	       (gotoa-code (+ pc relative-address))))

      (macrolet ((pushv (the-stack value)
		   `(setf ,the-stack
			  (cons ,value ,the-stack)))

		 (popv (the-stack)
		   (let ((value (gensym)))
		     `(let ((,value (first ,the-stack)))
			(setf ,the-stack
			      (rest ,the-stack))
			,value)))

#|
		 (pushl (the-stack the-list)
		   `(setf ,the-stack
			  (append ,the-list ,the-stack)))
|#
		 )

	(multiple-value-bind
	      (interpretation-fn-tmp disassemble-fn-tmp)

	    (define-opcode-set

		#'next-byte
		#'get-code

	      (define-opcode NIL #x10 ()
			     (setf stack
				   (cons nil stack)))

	      (define-opcode LDC #x11 (value)
			     (setf stack
				   (cons value stack)))

	      (define-opcode LD #x12 (address.frame address.var)
			     (send-message env :lookup
					   (cons address.frame address.var)))

	      (define-opcode SEL #x13 (code-then code-else)
			     (pushv dump code)
			     (let ((value (popv stack)))
			       (if value
				   ;(setf code (gotor-code address-then))
				   (setf code code-then)
				   ;(setf code (gotor-code address-else))
				   (setf code code-else))))

	      (define-opcode JOIN #x14 ()
			     (setf code (popv dump)))

	      (define-opcode LDF #x15 (function)
			     (let ((closure (make-closure function env)))
			       (pushv stack closure)))

	      (define-opcode AP #x16 ()
			     (let* ((closure (popv stack))
				    (closure-code (funcall closure :code))
				    (closure-env (funcall closure :env))
				    (params (popv stack))
				    (new-env (env.b.extend params closure-env)))
			       (pushv dump stack)
			       (pushv dump env)
			       (pushv dump code)
			       (setf stack '())
			       (setf env (list new-env))
			       (setf code closure-code)))

	      (define-opcode RET #x17 ()
			     (let ((ret-value (popv stack)))
			       (setf code (popv dump))
			       (setf env (popv dump))
			       (setf stack (popv dump))
			       (pushv stack ret-value)))

	      (define-opcode DUM #x18 ()
			     (setf env (env.b.extend '() env)))

	      (define-opcode RAP #x19 ()
			     (let* ((closure (popv stack))
				    (closure-code (funcall closure :code))
				    (closure-env (funcall closure :env))
				    (params (popv stack))
				    (new-env (env.b.extend params closure-env)))
			       (pushv dump stack)
			       (pushv dump env)
			       (pushv dump code)
			       (setf stack '())
			       (send-message new-env :set-current-bindings
					     (send-message env :get-current-bindings))
			       (setf env (list new-env))
			       (setf code closure-code))))

	  (setf interpretation-fn interpretation-fn-tmp)
	  (setf disassemble-fn disassemble-fn-tmp)))

      (lambda (action)
	(case action
	  (:run (run+))
	  (:reset (reset))
	  (:print (format t "STACK:~a~%ENV:~a~%CODE:~a~%DUMP:~a~%"
			  stack env code dump))
	  (:disassemble (funcall disassemble-fn))
	  (t (error "unknown machine action ~a" action)))))))
