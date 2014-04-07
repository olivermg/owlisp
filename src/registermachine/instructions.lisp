(in-package :owlisp/register)

(export '(make-machine))



(defun make-machine ()
  (let ((gp-registers (make-keyvalue-map))
	(pc '())
	(callstack '())
	(csp '())
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

	     (step-instruction ()
	       (let* ((instr (first pc)))
		 (funcall instr)
		 (setf pc
		       (rest pc))))

	     (run ()
	       (reset)
	       (labels ((run-instruction ()
			  (when pc
			    (step-instruction)
			    (run-instruction)))))))

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
