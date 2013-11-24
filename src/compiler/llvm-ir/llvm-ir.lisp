(in-package :owlisp/llvm-ir)

(export '(compile-defpackage
	  compile-defun
	  compile-call))



(defparameter *context* (LLVMGetGlobalContext))
(defparameter *module* nil)
(defparameter *builder* (LLVMCreateBuilderInContext *context*))



(defun compile-defpackage (name)
  (setf *module*
	(LLVMModuleCreateWithNameInContext name *context*)))



(defun compile-defun (name args evaluated-body-forms)
  (let* ((fn-type (LLVMFunctionType (LLVMInt32TypeInContext *context*)
				    (cffi:null-pointer)
				    0
				    0))
	 (fn (LLVMAddFunction *module* name fn-type))
	 (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
    (LLVMPositionBuilderAtEnd *builder* entry-block))
  (LLVMPrintModuleToFile *module* "outfilebla" (cffi:null-pointer)))



(defun compile-call (name args)
  (LLVMBuildCall *builder* nil nil 0 ""))
