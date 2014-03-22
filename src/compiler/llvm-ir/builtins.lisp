(cl:in-package :owlisp/llvm-ir)

(cl:export '(define-default-package
	     define-+
	     define-print))



(defun define-default-package ()
  (setf *module* (LLVMModuleCreateWithNameInContext "cl" *context*)))

(defun define-+ ()
  (with-declaration-args '(a b) llvm-args
    (let* ((fn-type (LLVMFunctionType *llvm-boxvalue-type*
				      llvm-args
				      2
				      0))
	   (fn (LLVMAddFunction *module* "owadd" fn-type))
	   (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
      (LLVMPositionBuilderAtEnd *builder* entry-block)
      (let* ((llvm-a (unbox-i64-value (LLVMGetParam fn 0)))
	     (llvm-b (unbox-i64-value (LLVMGetParam fn 1)))
	     (sum (LLVMBuildAdd *builder* llvm-a llvm-b ""))
	     (boxed-sum (box-i64-value sum)))
	(LLVMBuildRet *builder* boxed-sum))
      (store-function '+ fn))))

(defun define-print (msg)
  nil)
