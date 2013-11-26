(in-package :owlisp/llvm-ir)

(export '())



(defparameter *context* (LLVMGetGlobalContext))
(defparameter *module* nil)
(defparameter *builder* (LLVMCreateBuilderInContext *context*))
(defparameter *functions* (make-hash-table :test #'equalp))



(defun lookup-function (name)
  (gethash (owlisp:symbol->keyword name)
	   *functions*))

(defun store-function (name fn)
  (setf (gethash (owlisp:symbol->keyword name)
		 *functions*)
	fn))



(defun get-llvm-type ()
  (LLVMInt32TypeInContext *context*))

(defmacro with-declaration-args (args llvmargsvar &body body)
  `(cffi:with-foreign-object
       (,llvmargsvar :pointer ,(length args))
     (let ((llvm-type (get-llvm-type)))
       (reduce (lambda (current-index current-arg)
		 (declare (ignore current-arg))
		 (setf (cffi:mem-aref ,llvmargsvar :pointer current-index)
		       llvm-type)
		 (1+ current-index))
	       ,args
	       :initial-value 0))
     ,@body))



(defun stringify-atom (atom)
  (format nil "~a" atom))

(defun symbols->llvm-pointer-symbols (symbols)
  (mapcar (lambda (symbol)
	    (intern (format nil "%~a_P" (stringify-atom symbol))))
	  symbols))

(defun calling-args->llvm (calling-args)
  (format nil "~{i8* ~a~^, ~}" calling-args))
