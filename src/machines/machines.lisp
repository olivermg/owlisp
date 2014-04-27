(in-package :owlisp/machines)

(export '())



(defun make-closure (code env)
  (lambda (action)
    (case action
      (:code code)
      (:env env)
      (t (error "unknown action ~a" action)))))



(defmacro step-instruction (machine (&rest args) &body body)
  (let ((param-bytes (gensym))
	(iteration-arg (gensym)))
    `(let ((,param-bytes (loop
			   for ,iteration-arg in ',args
			   collect (send-message ,machine :next-byte))))
       (apply #'(lambda ,args ,@body) ,param-bytes))))

(defmacro define-opcode-set (machine &body body)
  (let ((opcode (gensym))
	(opcodes (gensym))
	(disassembled (gensym))
	(new-opcodes (gensym)))
    `(progn
       (send-message ,machine :set-interpretation-fn
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
			 (t (error "unknown opcode ~a" ,opcode)))))
       (send-message ,machine :set-disassemble-fn
		     (lambda ()
		       (labels ((disassemble-instruction (,disassembled ,opcodes)
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
			 (disassemble-instruction '() (send-message ,machine :get-code))))))))
