(in-package :owlisp/register)

(export '(make-machine
	  define-opcode
	  define-opcode-set))



(defun make-machine ()
  (let ((gp-registers (make-keyvalue-map))
	(pc '())
;	(callstack '())
;	(csp '())
	(code '())
	(val nil)
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

	     (reset ()
	       (setf pc code))

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
	  (:get-code #'get-code)
	  (:set-code #'(lambda (new-code)
			 (setf (get-code) new-code)))
	  (:reset #'reset)
	  (:disassemble-all #'disassemble-all)
	  (:step-instruction #'step-instruction)
	  (:set-interpretation-fn #'(lambda (fn)
				      (setf interpretation-fn fn)))
	  (:add-instructions #'add-instructions)
	  (:run #'run)
	  (:next-byte #'next-byte)
	  (:print #'(lambda ()
		      (format t "pc:~a~%code:~a~%" pc code)
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

#|
(defmacro define-opcode (name code (&rest args) &body body)
  `(list ,name ,code ,args ,body))
|#
