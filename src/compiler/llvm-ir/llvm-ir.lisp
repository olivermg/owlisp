(in-package :owlisp/llvm-ir)

(export '(compile-defpackage
	  compile-defun
	  compile-call
	  write-compilation))



(defun compile-defpackage (name)
  (setf *module*
	(LLVMModuleCreateWithNameInContext (string name) *context*)))

(defun compile-defun (name args evaluated-body-forms)
  (let ((int-type (LLVMInt32TypeInContext *context*)))
    (let* ((fn-type (LLVMFunctionType int-type
				      (cffi:null-pointer)
				      0
				      0))
	   (fn (LLVMAddFunction *module* (string name) fn-type))
	   (entry-block (LLVMAppendBasicBlockInContext *context* fn "entry")))
      (LLVMPositionBuilderAtEnd *builder* entry-block)
      (mapcar (lambda (form)
		(if (functionp form)
		    (funcall form)
		    (format t "unknown form ~a~%" form)))
	      evaluated-body-forms)
      (store-function name fn))))

(defun compile-call (name args)
  (lambda ()
    (let ((fn (lookup-function name)))
      (if fn
	  (LLVMBuildCall *builder*
			 fn
			 (cffi:null-pointer)
			 0
			 "")
	  (format t "call to unknown function ~a~%" name)))))

(defun write-compilation ()
  (LLVMPrintModuleToFile *module* "outfilebla" (cffi:null-pointer)))
