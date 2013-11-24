(in-package :owlisp/llvm-ir)

(export '(compile-defpackage
	  compile-defun
	  compile-call))



(defparameter *context* (LLVMGetGlobalContext))
(defparameter *module* nil)
(defparameter *builder* (LLVMCreateBuilderInContext *context*))
(defparameter *functions* (make-hash-table :test #'equalp))



(defun compile-defpackage (name)
  (setf *module*
	(LLVMModuleCreateWithNameInContext (format nil "~a" name) *context*)))



(defun compile-defun (name args evaluated-body-forms)
  (let* ((fn-type (LLVMFunctionType (LLVMInt32TypeInContext *context*)
				    (cffi:null-pointer)
				    0
				    0))
	 (fn (LLVMAddFunction *module* (format nil "~a" name) fn-type))
	 (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
    (LLVMPositionBuilderAtEnd *builder* entry-block)
    (mapcar (lambda (form)
	      (if (functionp form)
		  (funcall form)
		  (format t "unknown form ~a~%" form)))
	    evaluated-body-forms)
    (setf (gethash name *functions*) fn))
  (LLVMPrintModuleToFile *module* "outfilebla" (cffi:null-pointer)))



(defun compile-call (name args)
  (lambda ()
    (let ((fn (gethash name *functions*)))
      (if fn
	  (LLVMBuildCall *builder*
			 fn
			 (cffi:null-pointer)
			 0
			 "")
	  (format t "call to unknown function ~a~%" name)))))
