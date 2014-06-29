(in-package :owlisp/llvm)



(defun init-types ()

  (setf *value_t*
	(llvm-declare-structtype "struct._value_t"
				 (list (llvm-inttype8)
				       (llvm-inttype32))))

  (setf *value_p*
	(llvm-declare-pointertype *value_t*))

  (setf *frame_t*
	(llvm-declare-structtype "struct._frame_t"
				 (list :self
				       (llvm-arraytype *value_p*
						       16))))

  (setf *frame_p*
	(llvm-declare-pointertype *frame_t*))

  (setf *fn-type*
	(llvm-declare-functiontype (list *frame_p*)
				   *value_p*)))

(defun runtime-init ()
  (init-types))

#|
(defun add-activation-frame ()
  (let ((new-frame (RT-NEW-FRAME *activation-frame*)))
    (setf *activation-frame* new-frame)
    new-frame))

(defun drop-activation-frame ()
  (setf *activation-frame*
	(rest *activation-frame*))
  (first *activation-frame*))
|#



(defun RT-NEW-VALUE-INT (value)
  (new_value_int value))

(defun RT-NEW-VALUE (value)
  (RT-NEW-VALUE-INT value))

(defun RT-FREE-VALUE (value)
  (free_value value))

(defun RT-VALUES-EQUAL (value1 value2)
  (values_equal value1 value2))

(defun RT-DUMP-VALUE (value)
  (dump_value value))


(defun RT-NEW-FRAME (&optional (parent-frame (cffi:null-pointer)))
  (new_frame parent-frame))

(defun RT-FREE-FRAME (frame)
  (free_frame frame))

(defun RT-SET-BINDING (frame frameindex varindex value)
  (set_binding frame frameindex varindex value))

(defun RT-GET-BINDING (frame frameindex varindex)
  (get_binding frame frameindex varindex))

(defun RT-DUMP-FRAME (frame)
  (dump_frame frame))

(defun RT-SET-BINDINGS (frame values)
  (loop
     for v in values
     for i from 0 to (- (length values) 1)
     do (RT-SET-BINDING frame 0 i v)))



(defun RT-DECLARE-RUNTIME-FUNCTIONS (module)
  (let ((fn-type (llvm-declare-functiontype (list *frame_p*)
					    *value_p*)))
    (setf *new_value_int*
	  (llvm-declare-function "new_value_int"
				 fn-type))))

(defun RT-BUILD-NEW-VALUE-INT (value)
  (cffi:with-foreign-object
      (args :pointer 1)
    (setf (cffi:mem-aref args :pointer 0)
	  value)
    (LLVMBuildCall *builder*
		   *new_value_int*
		   args
		   1
		   "vi")))

(defun RT-BUILD-GET-BINDING ()
  )
