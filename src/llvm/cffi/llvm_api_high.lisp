(in-package :owlisp/llvm)



(defvar *context*)
(defvar *addressspace*)
(defvar *builder*)
(defvar *module*)
(defvar *bb-position-stack*)
;(defvar *activation-frame*)



(defun push-position (bb)
  (setf *bb-position-stack*
	(cons bb *bb-position-stack*))
  (LLVMPositionbuilderatend *builder* bb)
  *bb-position-stack*)

(defun pop-position ()
  (let ((left-bb (first *bb-position-stack*)))
    (setf *bb-position-stack*
	  (rest *bb-position-stack*))
    left-bb))

#|
(defun make-llvm-object (value)
  (lambda (action)
    (case action
      (:value value)
      (t (error "unknown action ~a" action)))))
|#

(defun llvm-init (&optional (main-module-name "main"))
  (setf *context* (LLVMGetglobalcontext))
  (setf *addressspace* 0)
  (setf *builder* (LLVMCreatebuilderincontext *context*))
  (setf *module* (llvm-create-module main-module-name))
  (setf *bb-position-stack* '())
  ;(setf *activation-frame* (RT-NEW-FRAME))
)

(defun llvm-create-module (name)
  (let ((module (LLVMModulecreatewithnameincontext name
						   *context*)))
    module))

(defun llvm-declare-structtype (name llvm-types)
  (let* ((new-struct (LLVMStructcreatenamed *context* name))
	 (new-struct-ptr (LLVMPointertype new-struct *addressspace*))
	 (element-types-count (length llvm-types)))
    (cffi:with-foreign-object
	(element-types :pointer element-types-count)
      (loop
	 for lt in llvm-types
	 for idx from 0 to (- element-types-count 1)
	 do (setf (cffi:mem-aref element-types :pointer idx)
		  (case (type-of lt)
		    (sb-sys:system-area-pointer lt) ; TODO: make this portable
		    (keyword (case lt
			       (:self new-struct-ptr)
			       (t (error "unknown type specifier keyword ~a" lt))))
		    (t (error "unknown type specifier ~a" lt) ))))
      (LLVMStructsetbody new-struct
			 element-types
			 element-types-count
			 0)
      (values new-struct
	      new-struct-ptr))))

(defun llvm-arraytype (llvm-type count)
  (LLVMArraytype llvm-type
		 count))

(defun llvm-inttype8 ()
  (LLVMInt8type))

(defun llvm-inttype32 ()
  (LLVMInt32type))

(defun llvm-declare-pointertype (llvm-type)
  (LLVMPointertype llvm-type
		   *addressspace*))

(defun llvm-declare-functiontype (llvm-param-types llvm-return-type)
  (let ((param-count (length llvm-param-types)))
    (cffi:with-foreign-object
	(llvm-param-types-obj :pointer param-count)
      (loop
	 for llvm-param-type in llvm-param-types
	 for idx from 0 to (- param-count 1)
	 do (setf (cffi:mem-aref llvm-param-types-obj :pointer idx)
		  llvm-param-type))
      (LLVMFunctiontype llvm-return-type
			llvm-param-types-obj
			param-count
			0))))

(defun llvm-declare-function (fn-name llvm-fn-type)
  (let* ((fn (LLVMAddFunction *module*
			      fn-name
			      llvm-fn-type)))
    (LLVMSetLinkage fn
		    :LLVMExternalLinkage)
    fn))

(defun llvm-define-function (fn-name llvm-fn-type)
  (let* ((fn (LLVMAddfunction *module* fn-name llvm-fn-type))
	 (bb (LLVMAppendbasicblock fn "entry")))
    (push-position bb)
    fn))

(defun llvm-build-return (llvm-return-value)
  (LLVMBuildret *builder*
		llvm-return-value)
  (pop-position))

(defun llvm-build-call (llvm-fn llvm-args)
  )



#|
(defun translate-to-llvm-struct-type (element-types)
  (declare (type sequence element-types))
  (let ((types-count (length element-types)))
    (cffi:with-foreign-object
	(llvm-element-types :pointer types-count)
      (loop
	 for element-type in element-types
	 for idx from 0 to (- types-count 1)
	 do (setf (cffi:mem-aref llvm-element-types :pointer idx)
		  (translate-to-llvm-type element-type)))
      (let ((llvm-struct (LLVMStructcreatenamed *context*
						"struct1")))
	(LLVMStructsetbody llvm-struct
			   llvm-element-types
			   types-count
			   0)
	llvm-struct))))

(defun get-type (type)
  (cond
    ((keywordp type) type)
    ((consp type) (if (numberp (car type))
		      :array
		      :struct))
    (t (error "unknown type ~a" type))))

(defun translate-to-llvm-type (type)
  (case (get-type type)
    (:char (LLVMInt8type))
    (:int (LLVMInt32type))
    (:pointer (LLVMPointertype (LLVMInt32type) 0))
    (:struct (translate-to-llvm-struct-type type))
    (:array (error "array not yet implemented"))))

(defun make-type (type)
  (make-llvm-object (translate-to-llvm-type type)))
|#
