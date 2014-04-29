(in-package :owlisp/machines)

(export '())



(defun make-closure (code env)
  (lambda (action)
    (case action
      (:code code)
      (:env env)
      (t (error "unknown action ~a" action)))))



(defmacro render-template ((&rest values) (&body template))
  (labels ((symbolize (name)
	     (intern
	      (concatenate
	       'string
	       "$"
	       (if (integerp name)
		   (write-to-string name)
		   (symbol-name name)))))

	   (build-value-alist (value-definitions)
	     (let ((value-alist '())
		   (value-index 0))
	       (loop
		  for val-def in value-definitions
		  do (multiple-value-bind
			   (val-key val-val)
			 (if (consp val-def)
			     (values (first val-def)
				     (second val-def))
			     (values nil
				     val-def))
		       (setf value-alist
			     (acons (symbolize (incf value-index))
				    val-val
				    value-alist))
		       (if val-key
			   (setf value-alist
				 (acons (symbolize val-key)
					val-val
					value-alist)))))
	       value-alist))

	   (resolve (value-alist expr)
	     (if (consp expr)
		 (loop
		    for elem in expr
		    collect (resolve value-alist elem))
		 (let ((found-var (assoc expr value-alist)))
		   (if found-var
		       (cdr found-var)
		       expr)))))

    (let ((value-alist (build-value-alist values)))
      (loop
	 for expr in template
	 collect (resolve value-alist expr)))))

#|
(defmacro destructure-define-opcode (definition)
  (destructuring-bind
	(define-opcode name code (&rest args) &body body)
      definition
    ))
|#

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
