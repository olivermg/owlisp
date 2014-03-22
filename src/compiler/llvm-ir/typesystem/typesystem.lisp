(in-package :owlisp/llvm-ir)

(export '(define-boxtype
	  define-typemap
	  define-lookup-type))



(defvar +TYPE-MAP+
  '((INTEGER . 1)
    (STRING . 2)))



(defun lookup-type (type-name)
  (owlisp:lookup-alist-value +TYPE-MAP+ type-name))



(defun define-global-struct-type (struct-name llvm-types)
  (let ((typecount (length llvm-types)))
    (cffi:with-foreign-object (struct-element-types :pointer typecount)
      (loop
	 for current-type in llvm-types
	 for current-index from 0 to (- typecount 1)
	 do (setf (cffi:mem-aref struct-element-types :pointer current-index)
		  current-type))
      (let ((struct-type (LLVMStructCreateNamed *context* struct-name)))
	(LLVMStructSetBody struct-type struct-element-types typecount 0)
	struct-type))))

(defun define-boxtype ()
  (let ((types (list (LLVMInt8TypeInContext *context*)
		     (LLVMPointerType (LLVMInt8TypeInContext *context*) 0))))
    (setf *llvm-boxvalue-type*
	  (define-global-struct-type "boxed_value" types))))

(defun define-typemap ()
  (let ((types (list (LLVMInt64TypeInContext *context*)
		     (LLVMArrayType (LLVMInt8TypeInContext *context*) 0))))
    (setf *llvm-typemap*
	  (define-global-struct-type "type_map" types))))



(defun define-lookup-type ()
  "runtime"
  (let ((fn-type (LLVMFunctionType :LLVMFunctionTypeKind
				   (cffi:null-pointer)
				   0
				   0))
	)
    (LLVMGetTypeKind (LLVMInt8Type))))



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
