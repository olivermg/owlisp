(in-package :owlisp/machines)

(export '(make-generic-machine
	  make-default-machine-register))



(defun make-generic-machine ()
  (let ((gp-registers (make-keyvalue-map))
	(pc '())
					;	(callstack '())
					;	(csp '())
	(code '())
	(val nil)
	(env nil)
	(fun nil)
	(stack '())
	(interpretation-fn nil)
	(disassemble-fn nil))
    (labels ((get-register (reg)
	       (lookup-in-keyvalue-map gp-registers
				       reg))

	     ((setf get-register) (val reg)
	       (update-in-keyvalue-map gp-registers
				       reg
				       val))

	     (get-code ()
	       code)

	     ((setf get-code) (val)
	       (setf code val))

	     (get-val ()
	       val)

	     ((setf get-val) (new-val)
	       (setf val new-val))

	     (get-env ()
	       env)

	     ((setf get-env) (new-env)
	       (setf env new-env))

	     (get-fun ()
	       fun)

	     ((setf get-fun) (new-fun)
	       (setf fun new-fun))

	     (get-pc ()
	       pc)

	     ((setf get-pc) (new-pc)
	       (setf pc new-pc))

	     (push-stack (value)
	       (setf stack
		     (cons value stack)))

	     (pop-stack ()
	       (let ((value (first stack)))
		 (setf stack
		       (rest stack))
		 value))

	     (reset ()
	       (setf pc code)
	       (setf stack '())
	       (setf val nil))

	     (add-instructions (instrs)
	       (setf (get-code)
		     (append (get-code)
			     instrs)))

	     (step-instruction ()
		 (funcall interpretation-fn (next-byte)))

	     (run ()
	       (reset)
	       (labels ((run-instruction ()
			  (when pc
			    (step-instruction)
			    (run-instruction))
			  val))
		 (run-instruction)))

	     (next-byte ()
	       (let ((byte (car pc)))
		 (setf pc
		       (rest pc))
		 byte)))

      (lambda (action)
	(case action
	  (:get-register #'get-register)
	  (:set-register #'(lambda (reg val)
			     (setf (get-register reg) val)))
	  (:get-val #'get-val)
	  (:set-val #'(lambda (new-val)
			(setf (get-val) new-val)))
	  (:get-code #'get-code)
	  (:set-code #'(lambda (new-code)
			 (setf (get-code) new-code)))
	  (:get-env #'get-env)
	  (:set-env #'(lambda (new-env)
			(setf (get-env) new-env)))
	  (:get-fun #'get-fun)
	  (:set-fun #'(lambda (new-fun)
			(setf (get-fun) new-fun)))
	  (:get-pc #'get-pc)
	  (:set-pc #'(lambda (new-pc)
		       (setf (get-pc) new-pc)))
	  (:push #'push-stack)
	  (:pop #'pop-stack)
	  (:reset #'reset)
	  (:step-instruction #'step-instruction)
	  (:set-interpretation-fn #'(lambda (fn)
				      (setf interpretation-fn fn)))
	  (:set-disassemble-fn #'(lambda (fn)
				   (setf disassemble-fn fn)))
	  (:disassemble #'(lambda () (funcall disassemble-fn (get-code))))
	  (:add-instructions #'add-instructions)
	  (:run #'run)
	  (:next-byte #'next-byte)
	  (:print #'(lambda ()
		      (format t "val:~a~%pc:~a~%code:~a~%stack:~a~%env:~a~%"
			      val pc code stack env))))))))



(defun make-default-machine-register ()
  (let ((machine (make-machine)))
    (multiple-value-bind
	  (interpretation-fn disassemble-fn)

	(define-opcode-set
	    (funcall machine :next-byte)

	  (define-opcode PUSH #x01 ()
			 (send-message machine :push
				       (send-message machine :get-val)))

	  (define-opcode POP #x02 ()
			 (send-message machine :set-val
				       (send-message machine :pop)))

	  (define-opcode CONSTANT #x10 (value)
			 (send-message machine :set-val
				       value))

	  (define-opcode REFERENCE #x11 (address.frame address.var)
			 (let* ((env (send-message machine :get-env))
				(address (cons address.frame address.var))
				(value (send-message env :lookup address)))
			   (send-message machine :set-val
					 value)))

	  (define-opcode ALLOCATE-FRAME #x20 (size)
			 (let* ((dummy-values (make-list size))
				(env (send-message machine :get-env))
				(extended-env (env.b.extend dummy-values env)))
			   (send-message machine :set-env extended-env)))

	  (define-opcode SET-FRAME-VALUE #x21 (address.frame address.var)
			 (let ((env (send-message machine :get-env))
			       (value (send-message machine :get-val)))
			   (send-message env :set-value
					 value
					 (cons address.frame address.var))))

	  (define-opcode GET-FRAME-VALUE #x22 (address.frame address.var)
			 (let ((env (send-message machine :get-env)))
			   (send-message env :get-value
					 (cons address.frame address.var))))

	  (define-opcode GOTO #x30 (offset)
			 (labels ((advance-pc (count)
				    (when (> count 0)
				      (send-message machine :set-pc
						    (rest (send-message machine :get-pc)))
				      (advance-pc (- count 1)))))
			   (advance-pc offset)))

	  (define-opcode RETURN #x31 ()
			 (send-message machine :set-pc
				       (send-message machine :pop)))

	  (define-opcode CREATE-CLOSURE #x40 (offset)
			 (labels ((advance (code count)
				    (if (> count 0)
					(advance (rest code) (- count 1))
					code)))
			   (send-message machine :set-val
					 (make-closure (advance (send-message machine :get-pc)
								offset)
						       (send-message machine :get-env)))))

	  (define-opcode CALL #x41 ()
			 (let* ((closure (send-message machine :get-fun))
				(code (funcall closure :code))
				(env (funcall closure :env)))
			   (send-message machine :set-env env)
			   (send-message machine :set-pc code))))

      (send-message machine :set-interpretation-fn interpretation-fn)
      (send-message machine :set-disassemble-fn disassemble-fn)
      machine)))
