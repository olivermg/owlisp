(cl:in-package :owlisp/llvm-ir)

(cl:export '(define-default-package
	     define-+
	     define-print))



(defun define-default-package ()
  (setf *module* (LLVMModuleCreateWithNameInContext "cl" *context*)))

(defun define-+ ()
  (with-declaration-args '(a b) llvm-args
    (let* ((fn-type (LLVMFunctionType *llvm-default-type*
				      llvm-args
				      2
				      0))
	   (fn (LLVMAddFunction *module* "owadd" fn-type))
	   (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
      (LLVMPositionBuilderAtEnd *builder* entry-block)
      (let* ((llvm-a-ptr (LLVMConstPointerCast (LLVMGetParam fn 0)
					       (LLVMPointerType (LLVMInt64Type) 0)))
	     (llvm-b-ptr (LLVMConstPointerCast (LLVMGetParam fn 1)
					       (LLVMPointerType (LLVMInt64Type) 0)))
	     (llvm-a (LLVMBuildLoad *builder* llvm-a-ptr ""))
	     (llvm-b (LLVMBuildLoad *builder* llvm-b-ptr ""))
	     (sum (LLVMBuildAdd *builder* llvm-a llvm-b "")))
	(LLVMBuildRet *builder* sum))
      (store-function '+ fn))))

(defun define-print (msg)
  nil)
