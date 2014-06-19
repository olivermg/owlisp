(in-package :owlisp/llvm-ir)

(export '(;compile-defpackage
	  ;compile-defun
	  ;compile-call
	  ;write-compilation

	  LLVM-INIT
	  LLVM-SET-MODULE
	  LLVM-CREATE-MODULE
	  LLVM-DUMP-MODULE
	  LLVM-CONSTANT
	  LLVM-REFERENCE
	  LLVM-DEFINE
	  LLVM-CALL))



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




(defvar *value_t*)
(defvar *value_p*)
(defvar *frame_t*)
(defvar *frame_p*)
(defvar *fn-type*)
(defvar *context*)
(defvar *addressspace*)
(defvar *builder*)
(defvar *module*)
(defvar *bb-position-stack*)
(defvar *activation-frame*)

(defun init-types ()

  (setf *value_t*
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

  (setf *value_p*
	(LLVMPointertype *value_t*
			 *addressspace*))

  (setf *frame_t*
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

  (setf *frame_p*
	(LLVMPointertype *frame_t*
			 *addressspace*))

  (setf *fn-type*
	(cffi:with-foreign-object
	    (types :pointer 1)
	  (setf (cffi:mem-aref types :pointer 0)
		*frame_t*)
	  (LLVMFunctiontype *value_t*
			    types
			    1
			    0))))

(defun push-position (bb)
  (setf *bb-position-stack*
	(cons bb *bb-position-stack*))
  (LLVMPositionbuilderatend *builder* bb)
  *bb-position-stack*)

(defun add-activation-frame ()
  (let ((new-frame (RT-NEW-FRAME *activation-frame*)))
    (setf *activation-frame* new-frame)
    new-frame))

(defun drop-activation-frame ()
  (setf *activation-frame*
	(rest *activation-frame*))
  (first *activation-frame*))



(defun LLVM-INIT (main-module-name)
  (setf *context* (LLVMGetglobalcontext))
  (setf *addressspace* 0)
  (setf *builder* (LLVMCreatebuilderincontext *context*))
  (setf *module* (LLVM-CREATE-MODULE main-module-name))
  (init-types)
  (setf *bb-position-stack* '())
  (setf *activation-frame* (RT-NEW-FRAME))
  (let* ((main-fn (LLVM-DEFINE "main"))
	 (main-fn-bb0 (LLVMAppendbasicblockincontext *context* main-fn "bb0")))
    (LLVMPositionbuilderatend *builder* main-fn-bb0)
    (push-position main-fn-bb0)))

(defun LLVM-SET-MODULE (module)
  (setf *module* module))

(defun LLVM-CREATE-MODULE (name)
  (LLVMModulecreatewithnameincontext name
				     *context*))

(defun LLVM-DUMP-MODULE (module)
  (LLVMDumpmodule module))

(defun LLVM-CONSTANT (value)
  (RT-NEW-VALUE value))

(defun LLVM-REFERENCE (frameindex varindex)
  (RT-GET-BINDING *activation-frame* frameindex varindex))

(defun LLVM-DEFINE (fn-name)
  (let* ((fn (LLVMAddfunction *module* fn-name *fn-type*))
	 (bb (LLVMAppendbasicblock fn "entry")))
    (push-position bb)
    fn))

(defun LLVM-CALL (fn &rest args)
  (let ((frame (add-activation-frame)))
    (RT-SET-BINDINGS frame args)
    (cffi:with-foreign-object
	(args :pointer 1)
      (setf (cffi:mem-aref args :pointer 0)
	    frame)
      (LLVMBuildCall *builder*
		     fn
		     args
		     1
		     (cffi:null-pointer)))))
