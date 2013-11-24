(cl:in-package :owlisp/llvm-ir)

(cl:export '(define-default-package
	     define-+
	     define-print))



(defun define-default-package ()
  (setf *module* (LLVMModuleCreateWithNameInContext "cluser" *context*)))

(defun define-+ ()
  (let ((int-type (LLVMInt32TypeInContext *context*)))
    (cffi:with-foreign-object (param-types :pointer 2)
      (setf (cffi:mem-aref param-types :pointer 0) int-type)
      (setf (cffi:mem-aref param-types :pointer 1) int-type)
      (let* ((fn-type (LLVMFunctionType int-type
					param-types
					2
					0))
	     (fn (LLVMAddFunction *module* "add" fn-type))
	     (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
	(LLVMPositionBuilderAtEnd *builder* entry-block)
	(let* ((llvm-a (LLVMGetFirstParam fn))
	       (llvm-b (LLVMGetNextParam llvm-a))
	       (sum (LLVMBuildAdd *builder* llvm-a llvm-b "")))
	  (LLVMBuildRet *builder* sum))))))

(defun define-print (msg)
  nil)
