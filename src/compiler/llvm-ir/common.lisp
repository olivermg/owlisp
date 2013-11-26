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

(defun declaration-args->llvm (args)
  (let ((llvmargs (cffi:foreign-alloc :pointer :count (length args))))
    (labels ((declaration-args->llvm/r (current-pos restargs)
	       (when restargs
		 (setf
		  (cffi:mem-aref llvmargs :pointer current-pos)
		  (get-llvm-type))
		 (declaration-args->llvm/r (+ 1 current-pos)
					   (cdr restargs)))))
      (declaration-args->llvm/r 0 args))
    llvmargs))



(defun stringify-atom (atom)
  (format nil "~a" atom))

(defun symbols->llvm-pointer-symbols (symbols)
  (mapcar (lambda (symbol)
	    (intern (format nil "%~a_P" (stringify-atom symbol))))
	  symbols))

(defun calling-args->llvm (calling-args)
  (format nil "~{i8* ~a~^, ~}" calling-args))
