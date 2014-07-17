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
  bb)

(defun pop-position ()
  (setf *bb-position-stack*
	(rest *bb-position-stack*))
  (let ((new-bb (first *bb-position-stack*)))
    (if new-bb
	(LLVMPositionbuilderatend *builder* new-bb))
    new-bb))

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

(defun llvm-shutdown ()
  (LLVMShutdown))

(defun llvm-create-module (name)
  (let ((module (LLVMModulecreatewithnameincontext name
						   *context*)))
    module))

(defun llvm-declare-structtype (name types)
  (let* ((llvm-types (ensure-llvm-representation types))
	 (new-struct (LLVMStructcreatenamed *context* name))
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

(defun llvm-arraytype (type count)
  (let ((llvm-type (ensure-llvm-representation type)))
    (LLVMArraytype llvm-type
		   count)))

(defun llvm-inttype1 ()
  (LLVMInt1type))

(defun llvm-inttype8 ()
  (LLVMInt8type))

(defun llvm-inttype32 ()
  (LLVMInt32type))

(defun llvm-voidtype ()
  (LLVMVoidtype))

(defun llvm-voidptrtype ()
  (llvm-declare-pointertype (llvm-voidtype)))

(defun llvm-declare-pointertype (type)
  (let ((llvm-type (ensure-llvm-representation type)))
    (LLVMPointertype llvm-type
		     *addressspace*)))

(defun llvm-declare-functiontype (param-types return-type)
  (let* ((llvm-param-types (ensure-llvm-representation param-types))
	 (llvm-return-type (ensure-llvm-representation return-type))
	 (param-count (length llvm-param-types)))
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

(defun llvm-declare-function (fn-name fn-type)
  (let* ((llvm-fn-type (ensure-llvm-representation fn-type))
	 (fn (LLVMAddFunction *module*
			      fn-name
			      llvm-fn-type)))
    (LLVMSetLinkage fn
		    :LLVMExternalLinkage)
    fn))

(defun llvm-define-function (fn-name fn-type)
  (let* ((llvm-fn-type (ensure-llvm-representation fn-type))
	 (fn (LLVMAddfunction *module* fn-name llvm-fn-type))
	 (bb (LLVMAppendbasicblock fn "entry")))
    (push-position bb)
    fn))

(defun llvm-build-return (return-value)
  (let ((current-fn (llvm-get-current-function))
	(llvm-return-value (ensure-llvm-representation return-value)))
    (LLVMBuildret *builder*
		  llvm-return-value)
    (pop-position)
    current-fn))

(defun llvm-build-call (fn args)
  (let* ((llvm-fn (ensure-llvm-representation fn))
	 (llvm-args (ensure-llvm-representation args))
	 (llvm-args-count (length llvm-args)))
    (cffi:with-foreign-object
	(llvm-args-foreign :pointer llvm-args-count)
      (loop
	 for arg in llvm-args
	 for idx from 0 to (- llvm-args-count 1)
	 do (setf (cffi:mem-aref llvm-args-foreign :pointer idx)
		  arg))
      (LLVMBuildcall *builder*
		     llvm-fn
		     llvm-args-foreign
		     llvm-args-count
		     ""))))

(defun llvm-set-current-basicblock (bb)
  (setf (first *bb-position-stack*)
	bb)
  (LLVMPositionbuilderatend *builder* bb))

(defun llvm-get-current-basicblock ()
  (first *bb-position-stack*))

(defun llvm-build-conditional-jump (predicate then-bb else-bb)
  (LLVMBuildcondbr *builder*
		   predicate
		   then-bb
		   else-bb))

(defun llvm-add-basicblock (&optional (name ""))
  (let ((bb (LLVMAppendbasicblock (llvm-get-current-function)
				  name)))
    (LLVMPositionbuilderatend *builder* bb)
    bb))

(defun llvm-merge-basicblocks (&rest incoming-basicblocks)
  (let ((merge-bb (llvm-add-basicblock "merge")))
    (loop
       for incoming-bb in incoming-basicblocks
       do (LLVMPositionbuilderatend *builder* incoming-bb)
	 (LLVMBuildbr *builder* merge-bb))
    (LLVMPositionbuilderatend *builder* merge-bb)
    merge-bb))

(defun llvm-get-param (fn &optional (param-index 0))
  (let ((llvm-fn (ensure-llvm-representation fn)))
    (LLVMGetparam llvm-fn
		  param-index)))

(defun llvm-get-current-function ()
  (LLVMGetbasicblockparent (first *bb-position-stack*)))

(defun llvm-in-main-function ()
  (= 1
     (length *bb-position-stack*)))

(defun llvm-const-int32 (value)
  (LLVMConstint (llvm-inttype32)
		value
		0))

(defun llvm-write-bitcode (module destfilepath)
  (LLVMWritebitcodetofile module
			  destfilepath))

(defun llvm-dump-module (module)
  (LLVMDumpmodule module))

(defun llvm-debug-type (type)
  (LLVMDumptype type))

(defun llvm-debug-value (value)
  (LLVMDumptype (LLVMTypeof value))
  (LLVMDumpvalue value))



(defun is-in-llvm-representation (value)
  (sb-sys:system-area-pointer-p value)) ; TODO: make this portable

(defun ensure-llvm-representation (value-or-valuesequence)
  (labels ((convert-fn (value)
	     (if (and (not (is-in-llvm-representation value))
		      (not (typep value 'keyword))
		      (not (null value)))
		 (cond
		   ((typep value 'integer) (llvm-const-int32 value))
		   (t (error "don't know how to convert value of type ~a to llvm representation"
			     (type-of value))))
		 value)))
    (if (consp value-or-valuesequence)
	(mapcar #'ensure-llvm-representation
		value-or-valuesequence)
	(convert-fn value-or-valuesequence))))



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
