(in-package :owlisp/llvm)

(export '(TARGET-INIT
	  TARGET-FINISH
	  TARGET-SHUTDOWN
	  TARGET-SET-MODULE
	  TARGET-CREATE-MODULE
	  TARGET-DUMP-MODULE
	  TARGET-CONSTANT
	  TARGET-REFERENCE
	  TARGET-DEFINE
	  TARGET-LEAVE-DEFINE
	  TARGET-CALL
	  TARGET-ALTERNATIVE-PREDICATE
	  TARGET-ALTERNATIVE-PHI-VAR
	  TARGET-ALTERNATIVE-STORE-TO-PHI
	  TARGET-ALTERNATIVE-THEN
	  TARGET-ALTERNATIVE-ELSE
	  TARGET-ALTERNATIVE-MERGE))



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
  (let ((main-fn (TARGET-DEFINE-MAIN)))
    (rt-create-init-activation-frame)
    main-fn))

(defun TARGET-FINISH ()
  (TARGET-LEAVE-DEFINE 0))

(defun TARGET-SHUTDOWN ()
  (llvm-shutdown))

(defun TARGET-SET-MODULE (module)
  (setf *module* module))

(defun TARGET-DUMP-MODULE (&optional filepath)
  (if filepath
      (llvm-write-bitcode *module*
			  filepath)
      (llvm-dump-module *module*)))

(defun TARGET-CONSTANT (value)
  (RT-BUILD-NEW-VALUE value))

(defun TARGET-REFERENCE (frameindex varindex)
  (RT-BUILD-GET-BINDING frameindex varindex))

(defun TARGET-DEFINE (fn-name)
  (llvm-define-function fn-name *fn-type*))

(defun TARGET-LEAVE-DEFINE (llvm-return-value)
  (llvm-build-return llvm-return-value))

(defun TARGET-DEFINE-MAIN ()
  (llvm-define-function "main" *main-fn-type*))

(defun TARGET-CALL (fn &rest args)
  (apply #'rt-build-call fn args))

(defun TARGET-ALTERNATIVE-PREDICATE ()
  (llvm-get-current-basicblock))

(defun TARGET-ALTERNATIVE-PHI-VAR ()
  (rt-build-value-alloca))

(defun TARGET-ALTERNATIVE-STORE-TO-PHI (value phi-var)
  (llvm-build-store value
		    phi-var))

(defun TARGET-ALTERNATIVE-THEN ()
  (llvm-add-basicblock "then"))

(defun TARGET-ALTERNATIVE-ELSE ()
  (llvm-add-basicblock "else"))

(defun TARGET-ALTERNATIVE-MERGE (phi-var predicate-bb predicate then-bb else-bb)
  (llvm-set-current-basicblock predicate-bb)
  (rt-build-conditional-jump predicate then-bb else-bb)
  (llvm-merge-basicblocks phi-var then-bb else-bb))
