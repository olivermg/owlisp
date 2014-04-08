(in-package :owlisp/register)

(export '(make-machine))



(defun make-machine ()
  (let ((gp-registers (make-keyvalue-map))
	(pc '())
	(callstack '())
	(stack '())
	(code '())
	(val nil))
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

	     (step-instruction (bind-env)
	       (let ((instr (first pc)))
		 (setf val
		       (funcall instr bind-env))
		 (setf pc
		       (rest pc))))

	     (run (bind-env)
	       (reset)
	       (labels ((run-instruction ()
			  (when pc
			    (step-instruction bind-env)
			    (run-instruction))))
		 (run-instruction)
		 val))

	     (push-arg (arg)
	       (setf stack
		     (cons arg stack)))

	     (pop-arg ()
	       (let ((arg (first stack)))
		 (setf stack
		       (rest stack))
		 arg)))

      (lambda (action)
	(case action
	  (:get-register #'get-register)
	  (:set-register #'(lambda (reg val)
			     (setf (get-register reg) val)))
	  (:get-code #'get-code)
	  (:set-code #'(lambda (new-code)
			 (setf (get-code) new-code)))
	  (:reset #'reset)
	  (:step-instruction #'step-instruction)
	  (:add-instructions #'add-instructions)
	  (:run #'run)
	  (:print #'(lambda ()
		      (format t "pc:~a~%code:~a~%" pc code)
		      (maphash #'(lambda (k v)
				   (format t "~a:~a~%" k v))
			       gp-registers))))))))
