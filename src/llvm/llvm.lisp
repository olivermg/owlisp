(in-package :owlisp/llvm)

(export '(TARGET-INIT
	  TARGET-SET-MODULE
	  TARGET-CREATE-MODULE
	  TARGET-DUMP-MODULE
	  TARGET-CONSTANT
	  TARGET-REFERENCE
	  TARGET-DEFINE
	  TARGET-LEAVE-DEFINE
	  TARGET-CALL))



#|
(defun compile-defpackage (name)
  (setf *module*
	(LLVMModuleCreateWithNameInContext (string name) *context*)))

(defun compile-defun (name args evaluated-body-forms)
  (with-declaration-args args llvm-args
    (let* ((fn-type (LLVMFunctionType *llvm-boxvalue-type*
				      llvm-args
				      (length args)
				      0))
	   (fn (LLVMAddFunction *module*
				(string-downcase (string name))
				fn-type))
	   (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
      (LLVMPositionBuilderAtEnd *builder* entry-block)
      (let ((result-value nil))
	(mapcar (lambda (form)
		  (if (functionp form)
		      (setf result-value (funcall form))
		      (error 'owlisp:unknown-form
			     :name (pprint form))))
		evaluated-body-forms)
	(LLVMBuildRet *builder* result-value))
      (store-function name fn))))

(defun compile-call (name args)
  (lambda ()
    (with-calling-args args llvm-args
      (let ((fn (lookup-function name)))
	(if fn
	    (LLVMBuildCall *builder*
			   fn
			   llvm-args
			   2
			   "")
	    (error 'owlisp:unknown-form
		   :name name))))))

(defun write-compilation ()
  (LLVMPrintModuleToFile *module* +OUTPUTFILE+ (cffi:null-pointer)))
|#



(defun TARGET-INIT ()
  (llvm-init)
  (runtime-init)
  (RT-DECLARE-RUNTIME-FUNCTIONS *module*)
  (let* ((main-fn (TARGET-DEFINE "main"))
	 (main-fn-bb0 (LLVMAppendbasicblockincontext *context* main-fn "bb0")))
    (LLVMPositionbuilderatend *builder* main-fn-bb0)
    (rt-create-activation-frame)
    (push-position main-fn-bb0)))

(defun TARGET-SET-MODULE (module)
  (setf *module* module))

(defun TARGET-DUMP-MODULE ()
  (LLVMDumpmodule *module*))

(defun TARGET-CONSTANT (value)
  (let ((llvm-value (LLVMConstint (LLVMInt32type)
				  value
				  0)))
   (RT-BUILD-NEW-VALUE-INT llvm-value)))

(defun TARGET-REFERENCE (frameindex varindex)
  (RT-BUILD-GET-BINDING frameindex varindex))

(defun TARGET-DEFINE (fn-name)
  (llvm-define-function fn-name *fn-type*))

(defun TARGET-LEAVE-DEFINE (llvm-return-value)
  (llvm-build-return llvm-return-value))

(defun TARGET-CALL (fn &rest args)
  (let ((frame (add-activation-frame)))
    (RT-SET-BINDINGS frame args)
    (cffi:with-foreign-object
	(args :pointer 1)
      (setf (cffi:mem-aref args :pointer 0)
	    frame)
      (LLVMBuildCall *builder*
		     fn
		     args
		     1
		     (cffi:null-pointer)))))
