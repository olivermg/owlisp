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




#|
(defparameter *value_t*
  (LLVMStructtype (list (LLVMInt8type)
			(LLVMInt32type))
		  2
		  nil))

(defparameter *value_p*
  (LLVMPointertype *value_t* 0))

(defparameter *frame_t*
  (LLVMStructtype (list (LLVMPointertype *frame_t* 0)
			(LLVMArraytype *value_p* 16))
		  2
		  nil))

(defparameter *frame_p*
  (LLVMPointertype *frame_t* 0))

(defparameter *fn-type*
  ())
|#

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
