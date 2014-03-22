(in-package :owlisp/llvm-ir)

(export '(initialize))



(defun initialize ()
  (setf *context* (LLVMGetGlobalContext))
  (setf *builder* (LLVMCreateBuilderInContext *context*))
  (setf *functions* (make-hash-table :test #'equalp))
  (setf *llvm-default-type* (LLVMPointerType
			     (LLVMInt8TypeInContext *context*)
			     0)))

(defun lookup-function (name)
  (gethash (owlisp:symbol->keyword name)
	   *functions*))

(defun store-function (name fn)
  (setf (gethash (owlisp:symbol->keyword name)
		 *functions*)
	fn))



(defmacro with-declaration-args (args llvmargsvar &body body)
  `(cffi:with-foreign-object
       (,llvmargsvar :pointer (length ,args))
     (reduce (lambda (current-index current-arg)
	       (declare (ignore current-arg))
	       (setf (cffi:mem-aref ,llvmargsvar :pointer current-index)
		     *llvm-boxvalue-type*)
	       (1+ current-index))
	     ,args
	     :initial-value 0)
     ,@body))

(defmacro with-calling-args (args llvmargsvar &body body)
  `(cffi:with-foreign-object
       (,llvmargsvar :pointer (length ,args))
     (reduce (lambda (current-index current-arg)
	       (let* ((boxed-llvm-value (box-i64-value (int->i64 current-arg))))
		 (setf (cffi:mem-aref ,llvmargsvar
				      :pointer
				      current-index)
		       boxed-llvm-value))
	       (1+ current-index))
	     ,args
	     :initial-value 0)
     ,@body))



(defun stringify-atom (atom)
  (format nil "~a" atom))

(defun symbols->llvm-pointer-symbols (symbols)
  (mapcar (lambda (symbol)
	    (intern (format nil "%~a_P" (stringify-atom symbol))))
	  symbols))

(defun calling-args->llvm (calling-args)
  (format nil "~{i8* ~a~^, ~}" calling-args))
