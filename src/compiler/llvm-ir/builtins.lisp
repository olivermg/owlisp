(cl:in-package :owlisp/llvm-ir)

(cl:export '(define-default-package
	     define-+
	     define-print))



(defun define-default-package ()
  (setf *module* (LLVMModuleCreateWithNameInContext "cl" *context*)))

(defun define-+ ()
  (let ((llvm-args (declaration-args->llvm '(a b))))
    (let* ((fn-type (LLVMFunctionType (get-llvm-type)
				      llvm-args
				      2
				      0))
	   (fn (LLVMAddFunction *module* "add" fn-type))
	   (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
      (LLVMPositionBuilderAtEnd *builder* entry-block)
      (let* ((llvm-a (LLVMGetParam fn 0))
	     (llvm-b (LLVMGetParam fn 1))
	     (sum (LLVMBuildAdd *builder* llvm-a llvm-b "")))
	(LLVMBuildRet *builder* sum))
      (store-function 'add fn))))

(defun define-print (msg)
  nil)
