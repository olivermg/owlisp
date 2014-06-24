(in-package :owlisp/llvm)



(defun make-llvm-object (value)
  (lambda (action)
    (case action
      (:value value)
      (t (error "unknown action ~a" action)))))

(defun llvm-inttype8 ()
  (LLVMInt8type))

(defun llvm-inttype32 ()
  (LLVMInt32type))

(defun llvm-structtype (name)
  (LLVMStructcreatenamed *context*
			 name))

(defun llvm-structtype-setbody (llvm-struct llvm-element-types)
  (let ((types-count (length llvm-element-types)))
    (cffi:with-foreign-object
	(llvm-element-types-obj :pointer types-count)
      (loop
	 for llvm-element-type in llvm-element-types
	 for idx from 0 to (- types-count 1)
	 do (setf (cffi:mem-aref llvm-element-types-obj :pointer idx)
		  llvm-element-type))
      (LLVMStructsetbody llvm-struct
			 llvm-element-types-obj
			 types-count
			 *addressspace*)
      llvm-struct)))

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
