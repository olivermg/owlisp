(in-package :owlisp/llvm-ir)

(export '(define-boxtype))



(defvar +TYPE-MAP+
  '((INTEGER . 1)
    (STRING . 2)))

(defun define-boxtype ()
  (let* ((i8 (LLVMInt8TypeInContext *context*))
	 (i8ptr (LLVMPointerType i8 0)))
    (cffi:with-foreign-object (struct-types :pointer 2)
      (setf (cffi:mem-aref struct-types :pointer 0)
	    i8)
      (setf (cffi:mem-aref struct-types :pointer 1)
	    i8ptr)
      (setf *llvm-boxvalue-type* (LLVMStructCreateNamed *context* "valuestruct"))
      (LLVMStructSetBody *llvm-boxvalue-type* struct-types 2 0))))

(defun lookup-type (type-name)
  (owlisp:lookup-alist-value +TYPE-MAP+ type-name))

(defun int->i8 (int)
  "compile-time"
  (let ((i8 (LLVMConstInt (LLVMInt8Type)
			  int
			  0)))
    i8))

(defun int->i64 (int)
  "compile-time"
  (let ((i64 (LLVMConstInt (LLVMInt64Type)
			   int
			   0)))
    i64))

(defun i64->i64* (i64)
  "compile-time"
  (let ((i64* (LLVMBuildAlloca *builder*
			       (LLVMInt64Type)
			       "")))
    (LLVMBuildStore *builder* i64 i64*)
    i64*))

(defun i64*->i8* (i64*)
  "compile-time"
  (let ((i8* (LLVMBuildPointerCast *builder*
				   i64*
				   (LLVMPointerType (LLVMInt8Type) 0)
				   "")))
    i8*))

(defun i8*->i64* (i8*)
  "compile-time"
  (let ((i64* (LLVMBuildPointerCast *builder*
				    i8*
				    (LLVMPointerType (LLVMInt64Type) 0)
				    "")))
    i64*))

(defun i64*->i64 (i64*)
  "compile-time"
  (let ((i64 (LLVMBuildLoad *builder*
			    i64*
			    "")))
    i64))

(defun box-i64-value (int)
  "compile-time"
  (let* ((type-id (int->i8 (lookup-type 'INTEGER)))
	 (payload-value (i64*->i8* (i64->i64* int)))
	 (box-struct-ptr1 (LLVMBuildInsertValue *builder*
						(LLVMGetUndef *llvm-boxvalue-type*)
						type-id
						0
						""))
	 (box-struct-ptr2 (LLVMBuildInsertValue *builder*
						box-struct-ptr1
						payload-value
						1
						"")))
    box-struct-ptr2))

(defun unbox-i64-value (boxed-value)
  "compile-time"
  (let* ((type-id (LLVMBuildExtractValue *builder*
					 boxed-value
					 0
					 ""))
	 (payload-value (i64*->i64
			 (i8*->i64*
			  (LLVMBuildExtractValue *builder*
						 boxed-value
						 1
						 "")))))
    payload-value))
