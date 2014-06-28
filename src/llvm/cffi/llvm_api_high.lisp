(in-package :owlisp/llvm)



(defun make-llvm-object (value)
  (lambda (action)
    (case action
      (:value value)
      (t (error "unknown action ~a" action)))))

(defun llvm-structtype (name llvm-types)
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

(defun llvm-pointertype (llvm-type)
  (LLVMPointertype llvm-type
		   *addressspace*))

(defun llvm-functiontype (llvm-param-types llvm-return-type)
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
