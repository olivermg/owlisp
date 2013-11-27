(in-package :owlisp/llvm-ir)

(export '())



(defparameter *context* (LLVMGetGlobalContext))
(defparameter *module* nil)
(defparameter *builder* (LLVMCreateBuilderInContext *context*))
(defparameter *functions* (make-hash-table :test #'equalp))
(defparameter *llvm-default-type* (LLVMInt32TypeInContext *context*))



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
		     *llvm-default-type*)
	       (1+ current-index))
	     ,args
	     :initial-value 0)
     ,@body))

(defmacro with-calling-args (args llvmargsvar &body body)
  `(cffi:with-foreign-object
       (,llvmargsvar :pointer (length ,args))
     (reduce (lambda (current-index current-arg)
	       (let ((llvm-value (LLVMConstInt *llvm-default-type*
					       current-arg
					       0)))
		 (setf (cffi:mem-aref ,llvmargsvar
				      :pointer
				      current-index)
		       llvm-value))
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
