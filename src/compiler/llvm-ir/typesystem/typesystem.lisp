(in-package :owlisp/llvm-ir)

(export '())



(defun define-dyntype ()
  (let* ((i8 (LLVMInt8TypeInContext *context*))
	 (i8ptr (LLVMPointerType i8 0)))
    (cffi:with-foreign-object (struct-types :pointer 2)
      (setf (cffi:mem-aref struct-types :pointer 0)
	    i8)
      (setf (cffi:mem-aref struct-types :pointer 1)
	    i8ptr)
      (let ((struct-type (LLVMStructCreateNamed *context* "")))
	(LLVMStructSetBody struct-type struct-types 2 0)))))

(defun make-function ()
  )

(defun define-value->llvm-ptr (value)
  (let* ((int-type (LLVMInt64TypeInContext *context*))
	 (llvm-const (LLVMConstInt int-type value 0))
	 (llvm-global-ptr (LLVMAddGlobal *module* int-type "")))
    (LLVMSetInitializer llvm-global-ptr llvm-const)
    (LLVMBuildPointerCast *builder* llvm-global-ptr *llvm-default-type* "")))

(defun define-box (value)
  (let ((typeId 1)
	(llvm-value-ptr (value->llvm-ptr value)))))

(defun define-unbox (llvm-value)
  )
