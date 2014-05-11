(in-package :owlisp/machines)

(export '())



(defun make-default-machine-secd (the-code)

  (let ((stack '())
	(env nil)
	(code the-code)
	(dump '())
	(interpretation-fn nil)
	(disassemble-fn nil))

    (labels ((next-byte ()
	       (let ((curbyte (car code)))
		 (setf code
		       (rest code))
		 curbyte))

	     (get-code ()
	       code)

	     (reset ()
	       (setf code the-code)
	       (setf stack '())
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
		 (run-instruction))))

      (macrolet ((pushv (the-stack value)
		   `(setf ,the-stack
			  (cons ,value ,the-stack)))

		 (popv (the-stack)
		   (let ((value (gensym)))
		     `(let ((,value (first ,the-stack)))
			(setf ,the-stack
			      (rest ,the-stack))
			,value)))

		 (pushl (the-stack the-list)
		   `(setf ,the-stack
			  (append ,the-list ,the-stack))))

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
			     (let ((value (popv stack)))
			       (if value
				   (pushl code code-then)
				   (pushl code code-else)))))

	  (setf interpretation-fn interpretation-fn-tmp)
	  (setf disassemble-fn disassemble-fn-tmp)))

      (lambda (action)
	(case action
	  (:run (run+))
	  (:reset (reset))
	  (:print (format t "STACK:~a~%ENV:~a~%CODE:~a~%DUMP:~a~%"
			  stack env code dump))
	  (:disassemble (funcall disassemble-fn))
	  (t (error "unknown action ~a" action)))))))
