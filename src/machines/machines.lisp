(in-package :owlisp/machines)

(export '())



(defun make-closure (code env)
  (lambda (action)
    (case action
      (:code code)
      (:env env)
      (t (error "unknown action ~a" action)))))



(defmacro render-template ((&rest values) (&body template))
  (labels ((numerize (prefix expr-name)
	     (parse-integer
	      (subseq expr-name (length prefix))))

	   (prefixed-p (prefix expr-name)
	     (let ((substring1 (subseq expr-name 0 (length prefix))))
	       (if (string-equal substring1 prefix)
		   t
		   nil)))

	   (lookup (value-alist key)
	     (cdr (assoc key value-alist)))

	   (build-value-alist (value-definitions)
	     (let ((value-alist '())
		   (value-index 0))
	       (loop
		  for val in value-definitions
		  do (setf value-alist
			   (acons (incf value-index)
				  val
				  value-alist)))
	       value-alist))

	   (resolve (value-alist expr)
	     (cond
	       ((consp expr) (loop
				for elem in expr
				collect (resolve value-alist elem)))
	       ((symbolp expr) (let ((expr-name (symbol-name expr)))
				 (cond
				   ((prefixed-p "$@" expr-name)
				    `(progn ,@(lookup value-alist
						      (numerize "$@" expr-name))))
				   ((prefixed-p "$" expr-name)
				    (lookup value-alist
					    (numerize "$" expr-name)))
				   (t expr))))
	       (t expr))))

    (let ((value-alist (build-value-alist values)))
      `',(loop
	    for expr in template
	    collect (resolve value-alist expr)))))

(defun destructuring-bind-for-define-opcode (definition)
  (destructuring-bind
	(define-opcode name code (&rest args) &body body)
      definition
    (declare (type integer code)
	     (type symbol name))
    (if (not (equal (symbol-name define-opcode)
		    "DEFINE-OPCODE"))
	(error "unknown instruction for define-opcode-set: ~a" define-opcode))
    (if (or (< code 0)
	    (> code 255))
	(error "opcode must be an integer between 0 and 255"))
    (values name code args body)))

(defmacro destructure-define-opcodes (opcode-var definitions template)
  `(case ,opcode-var
     ,@(loop
	  for definition in definitions
	  collect (multiple-value-bind
			(name code args body)
		      (destructuring-bind-for-define-opcode definition)
		    (eval `(render-template
			    (,name ,code ,args ,body)
			    ,template))))))

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
	  (destructure-define-opcodes ,opcode
				      ,body
				      (($2) (step-instruction ,nb-fn $3 $@4))))
	(lambda ()
	  (labels
	      ((disassemble-instruction (,disassembled ,opcodes)
		 (if ,opcodes
		     (let ((,new-opcodes nil))
		       (disassemble-instruction
			(format nil "~a ~a"
				,disassembled
				(destructure-define-opcodes
				 (first ,opcodes)
				 ,body
				 (($2)
				  (setf ,new-opcodes (subseq ,opcodes
							     (1+ (length '$3))))
				  (format nil "~a"
					  (append (list '$1)
						  (subseq (rest ,opcodes)
							  0
							  (length '$3)))))))
			,new-opcodes))
		     ,disassembled)))
	    (disassemble-instruction '() (funcall ,gc-fn))))))))
