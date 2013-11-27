(in-package :owlisp/llvm-ir)

(export '(compile-defpackage
	  compile-defun
	  compile-call
	  write-compilation))



(defun compile-defpackage (name)
  (setf *module*
	(LLVMModuleCreateWithNameInContext (string name) *context*)))

(defun compile-defun (name args evaluated-body-forms)
  (with-declaration-args args llvm-args
    (let* ((fn-type (LLVMFunctionType *llvm-default-type*
				      llvm-args
				      (length args)
				      0))
	   (fn (LLVMAddFunction *module* (string name) fn-type))
	   (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
      (LLVMPositionBuilderAtEnd *builder* entry-block)
      (let ((result-value nil))
	(mapcar (lambda (form)
		  (if (functionp form)
		      (setf result-value (funcall form))
		      (format t "unknown form ~a~%" form)))
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
  (LLVMPrintModuleToFile *module* "outfilebla" (cffi:null-pointer)))
