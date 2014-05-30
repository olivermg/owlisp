(in-package :owlisp/machines)

(export '(make-default-machine-secd))



(defun make-default-machine-secd (the-code)

  (let ((stack '())
	(env (env.b.extend '()))
	(code the-code)
	(dump '())
	(pc 0)
	(interpretation-fn #'(lambda () nil))
	(disassemble-fn #'(lambda () nil))
	(compilation-fn #'(lambda () nil)))

    (labels ((next-byte ()
	       (let ((curbyte (car code)))
		 (incf pc)
		 (setf code
		       (rest code))
		 curbyte))

	     (reset ()
	       (setf pc 0)
	       (setf code the-code)
	       (setf stack '())
	       (setf env (env.b.extend '()))
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
	       (gotoa-code (+ pc relative-address)))

	     (compile+ ()
	       (reset)
	       (let ((target-code '()))
		 (labels ((step-instruction ()
			      (setf target-code
				    (append target-code
					    (funcall compilation-fn (next-byte)))))
			  (compile-instruction ()
			    (when code
			      (step-instruction)
			      (compile-instruction))
			    target-code))
		   (compile-instruction)))))

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

		 (define-state-transition (before after)
		   `(multiple-value-setq
			(stack env code dump)
		      (destructuring-bind ,before
			  (list stack env code dump)
			(values ,@after)))))

	(multiple-value-setq
	      (interpretation-fn disassemble-fn)

	    (define-opcode-set

		#'next-byte

	      (define-opcode NIL #x10 ()
			     (define-state-transition
				 (s e c d)
				 (`(nil . ,s) e c d)))

	      (define-opcode LDC #x11 (value)
			     (define-state-transition
				 (s e c d)
				 (`(,value . ,s) e c d)))

	      (define-opcode LD #x12 (address.frame address.var)
			     (let ((value
				    (send-message env :lookup
						  (cons address.frame address.var))))
			       (define-state-transition
				   (s e c d)
				   (`(,value . ,s) e c d))))

	      (define-opcode SEL #x13 (code-then code-else)
			     (define-state-transition
				 ((x . s) e c d)
				 (s e (if x code-then code-else) `(,c . ,d))))

	      (define-opcode JOIN #x14 ()
			     (define-state-transition
				 (s e c (cr . d))
				 (s e cr d)))

	      (define-opcode LDF #x15 (function)
			     (define-state-transition
				 (s e c d)
				 (`((,function . ,e) . ,s) e c d)))

	      (define-opcode AP #x16 ()
			     (define-state-transition
				 (((cc . ec) v . s) e c d)
				 (nil (env.b.extend v ec) cc `(,s ,e ,c . ,d))))

	      (define-opcode RET #x17 ()
			     (define-state-transition
				 ((x) ec cc (s e c . d))
				 (`(,x . ,s) e c d)))

	      (define-opcode DUM #x18 ()
			     (define-state-transition
				 (s e c d)
				 (s (env.b.extend '() e) c d)))

	      (define-opcode RAP #x19 ()
			     (send-message (cdar stack) :set-current-bindings
					   (cadr stack))
			     (define-state-transition
				 (((cc . ec) v . s) e c d)
				 (nil ec cc `(,s ,e ,c . ,d))))

	      (define-opcode CAR #x20 ()
			     (define-state-transition
				 (((a . b) . s) e c d)
				 (`(,a . ,s) e c d)))

	      (define-opcode CDR #x21 ()
			     (define-state-transition
				 (((a . b) . s) e c d)
				 (`(,b . ,s) e c d)))

	      (define-opcode ATOM #x22 ()
			     (define-state-transition
				 ((a . s) e c d)
				 (`(,(atom a) . ,s) e c d)))

	      (define-opcode CONS #x23 ()
			     (define-state-transition
				 ((a b . s) e c d)
				 (`((,a . ,b) . ,s) e c d)))

	      (define-opcode STOP #x24 ()
			     (define-state-transition
				 (s e c d)
				 (s e `(,#x24 . ,c) d)))))

	(multiple-value-setq
	      (compilation-fn)

	  (define-target-compilation-set

	      #'next-byte

	    (define-opcode NIL #x10 ()
			   (owlisp/llvm-ir::i32)))

#|
	 (setf compilation-fn
	       #'(lambda ()
		   (let ((current-i -1))
		     (declare (special current-i))
		     (labels ((compile-sub (compiled)
				(let ((opcode (next-byte)))
				  (if opcode
				      (progn
					(incf current-i)
					(compile-sub
					 (append
					  compiled
					  (list
					   (case opcode
					     (#x10 (format nil "~t$P~a = null" current-i))
					     (#x11 (format nil "~t$P~a = new 'Integer'~%~t$P~a = ~a" current-i current-i (next-byte)))
					     (#x12 nil))))))
				      compiled))))
		       (compile-sub '())))))
|#
	  ))

      (lambda (action)
	(case action
	  (:run (run+))
	  (:reset (reset))
	  (:print (format nil "STACK: ~a~%ENV: ~a~%CODE: ~a~%DUMP: ~a~%DISASSEMBLED CODE: ~a~%TARGET CODE:~a~%"
			  stack env code dump (funcall disassemble-fn code) (compile+)))
	  (:compile (compile+))
	  (:disassemble (funcall disassemble-fn code))
	  (t (error "unknown machine action ~a" action)))))))
