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




(defparameter *context*
  (LLVMGetGlobalcontext))

(defparameter *addressspace*
  0)

(defparameter *builder*
  (LLVMCreatebuilderincontext *context*))

(defparameter *value_t*
  (cffi:with-foreign-object
      (types :pointer 2)
    (setf (cffi:mem-aref types :pointer 0)
	  (LLVMInt8type))
    (setf (cffi:mem-aref types :pointer 1)
	  (LLVMInt32type))
    (LLVMStructtypeincontext *context*
			     types
			     2
			     0)))

(defparameter *value_p*
  (LLVMPointertype *value_t*
		   *addressspace*))

(defparameter *frame_t*
  (let* ((frame_t (LLVMStructcreatenamed *context*
					 "struct._frame_t"))
	 (frame_p (LLVMPointertype frame_t
				   *addressspace*)))
    (cffi:with-foreign-object
	(types :pointer 2)
      (setf (cffi:mem-aref types :pointer 0)
	    frame_p)
      (setf (cffi:mem-aref types :pointer 1)
	    (LLVMArraytype *value_p* 16))
      (LLVMStructsetbody frame_t
			 types
			 2
			 0)
      frame_t)))

(defparameter *frame_p*
  (LLVMPointertype *frame_t*
		   *addressspace*))

(defparameter *fn-type*
  (cffi:with-foreign-object
      (types :pointer 1)
    (setf (cffi:mem-aref types :pointer 0)
	  *frame_t*)
    (LLVMFunctiontype *value_t*
		      types
		      1
		      0)))

(defvar *module*)


(defun LLVM-SET-MODULE (module)
  (setf *module* module))

(defun LLVM-CREATE-MODULE (name)
  (LLVMModulecreatewithname name))

(defun LLVM-DUMP-MODULE (module)
  (LLVMDumpmodule module))

(defun LLVM-CONSTANT (value)
  (RT-NEW-VALUE value))

(defun LLVM-REFERENCE (frame frameindex varindex)
  (RT-GET-BINDING frame frameindex varindex))

(defun LLVM-ABSTRACTION (fn-name fn-type body)
  (LLVMAddfunction *module* fn-name fn-type))
