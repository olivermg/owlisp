(in-package :owlisp/register)

(export '(make-machine
	  make-default-machine
	  define-opcode
	  define-opcode-set))



(defun make-machine ()
  (let ((gp-registers (make-keyvalue-map))
	(pc '())
;	(callstack '())
;	(csp '())
	(code '())
	(val nil)
	(env nil)
	(stack '())
	(interpretation-fn nil))
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

	     (disassemble-instruction (opcode)
	       (case opcode
		 ((10) 'constant)
		 ((11) 'reference)
		 ((12) 'abstraction)
		 ((13) 'let)
		 ((14) 'alternative)
		 ((15) 'application)
		 ((16) 'sequence)
		 ((nil) nil)
		 (t opcode)))

	     (disassemble-all ()
	       (reset)
	       (labels ((disassemble-step (disassembly)
			  (let ((cur-instr (first pc)))
			    (setf pc
				  (rest pc))
			    (if cur-instr
				(disassemble-step
				 (cons (disassemble-instruction cur-instr)
				       disassembly))
				disassembly))))
		 (let ((result (disassemble-step '())))
		   result)))

	     (step-instruction ()
		 (funcall interpretation-fn (next-byte)))

	     (run ()
	       (reset)
	       (labels ((run-instruction ()
			  (when pc
			    (setf val
				  (step-instruction))
			    (run-instruction)
			    val)))
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
	  (:get-pc #'get-pc)
	  (:set-pc #'(lambda (new-pc)
		       (setf (get-pc) new-pc)))
	  (:push #'push-stack)
	  (:pop #'pop-stack)
	  (:reset #'reset)
	  (:disassemble-all #'disassemble-all)
	  (:step-instruction #'step-instruction)
	  (:set-interpretation-fn #'(lambda (fn)
				      (setf interpretation-fn fn)))
	  (:add-instructions #'add-instructions)
	  (:run #'run)
	  (:next-byte #'next-byte)
	  (:print #'(lambda ()
		      (format t "val:~a~%pc:~a~%code:~a~%stack:~a~%env:~a~%" val pc code stack
			      (send-message env :get-current-bindings))
		      (maphash #'(lambda (k v)
				   (format t "~a:~a~%" k v))
			       gp-registers))))))))

(defmacro step-instruction (machine (&rest args) &body body)
  (let ((param-bytes (gensym))
	(iteration-arg (gensym)))
    `(let ((,param-bytes (loop
			   for ,iteration-arg in ',args
			   collect (send-message ,machine :next-byte))))
       (apply #'(lambda ,args ,@body) ,param-bytes))))

(defmacro define-opcode-set (machine &body body)
  (let ((opcode (gensym)))
    `(send-message ,machine :set-interpretation-fn
		   (lambda (,opcode)
		     (case ,opcode
		       ,@(loop
			    for opcode-definition in body
			    collect
			      (destructuring-bind
				    (define-opcode name code (&rest args) &body body)
				  opcode-definition

				(declare (type integer code)
					 (ignore name))
				(if (not (equal (symbol-name define-opcode)
						"DEFINE-OPCODE"))
				    (error "unknown expression for define-opcode-set: ~a"
					   define-opcode))
				(if (or (< code 0)
					(> code 255))
				    (error "code must be an integer between 0 and 255"))

				`((,code) (step-instruction ,machine ,args ,@body))))
		       (t (error "unknown opcode ~a" ,opcode)))))))


(defun make-default-machine ()
  (let ((machine (make-machine)))
    (define-opcode-set machine

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
	(let ((env (send-message machine :get-env))
	      (address (cons address.frame address.var)))
	  (send-message env :lookup
			address)))

      (define-opcode ALLOCATE-FRAME #x20 (size)
	(let* ((dummy-values (make-list size))
	       (env (send-message machine :get-env))
	       (extended-env (env.b.extend dummy-values env)))
	  (send-message machine :set-env extended-env)))

      (define-opcode SET-FRAME-VALUE #x21 (address.var)
	(let ((env (send-message machine :get-env))
	      (value (send-message machine :get-val)))
	  (send-message env :set-value
			value
			address.var)))

      (define-opcode GOTO #x30 (offset)
	(labels ((advance-pc (count)
		   (when (> count 0)
		     (send-message machine :set-pc
				   (rest (send-message machine :get-pc)))
		     (advance-pc (- count 1)))))
	  (advance-pc offset))))

    machine))
