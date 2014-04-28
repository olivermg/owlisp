(in-package :owlisp/machines)

(export '())



(defun make-closure (code env)
  (lambda (action)
    (case action
      (:code code)
      (:env env)
      (t (error "unknown action ~a" action)))))



(defmacro step-instruction (next-byte-fn (&rest args) &body body)
  (let ((param-bytes (gensym))
	(iteration-arg (gensym)))

    `(let ((,param-bytes
	    ,(if args
		 `(loop
		     for ,iteration-arg in ',args
		     collect (funcall ,next-byte-fn))
		 '())))
       (apply #'(lambda ,args ,@body) ,param-bytes))))

(defmacro define-opcode-set (next-byte-fn get-code-fn &body body)
  (let ((opcode (gensym))
	(opcodes (gensym))
	(disassembled (gensym))
	(new-opcodes (gensym))
	(nb-fn (gensym))
	(gc-fn (gensym)))

    `(let ((,nb-fn ,next-byte-fn)
	   (,gc-fn ,get-code-fn))

       (values

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

		     `((,code) (step-instruction ,nb-fn ,args ,@body))))
	    (t (error "unknown opcode ~a" ,opcode))))

	(lambda ()
	  (labels
	      ((disassemble-instruction (,disassembled ,opcodes)
		 (if ,opcodes
		     (let ((,new-opcodes nil))
		       (disassemble-instruction
			(format nil "~a ~a"
				,disassembled
				(case (first ,opcodes)
				  ,@(loop
				       for opcode-definition in body
				       collect
					 (destructuring-bind
					       (define-opcode name code (&rest args) &body body)
					     opcode-definition

					   (declare (ignore define-opcode body))
					   `((,code)
					     (setf ,new-opcodes (subseq ,opcodes
									(1+ (length ',args))))
					     (format nil "~a"
						     (append (list ',name)
							     (subseq (rest ,opcodes)
								     0
								     (length ',args)))))))))
			,new-opcodes))
		     ,disassembled)))
	    (disassemble-instruction '() (funcall ,gc-fn))))))))
