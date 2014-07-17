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
				   *value_p*))

  (setf *main-fn-type*
	(llvm-declare-functiontype '()
				   (llvm-inttype32))))

(defun runtime-init ()
  (init-types))

#|
(defun rt-value_t-type ()
  *value_t*)

(defun rt-value_p-type ()
  *value_p*)

(defun rt-frame_t-type ()
  *frame_t*)

(defun rt-frame_p-type ()
  *frame_p*)
|#

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



#|
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
|#



(defun rt-declare-runtime-functions (module)
  (declare (ignore module))

  (let ((fn-type (llvm-declare-functiontype (list (llvm-inttype8)
						  (llvm-voidptrtype))
					    *value_p*)))
    (setf *new_value*
	  (llvm-declare-function "new_value"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype (list *frame_p*
						  (llvm-inttype32)
						  (llvm-inttype32))
					    *value_p*)))
    (setf *get_binding*
	  (llvm-declare-function "get_binding"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype (list *frame_p*)
					    *frame_p*)))
    (setf *new_frame*
	  (llvm-declare-function "new_frame"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype (list *frame_p*
						  (llvm-inttype32)
						  (llvm-inttype32)
						  *value_p*)
					    (llvm-voidtype))))
    (setf *set_binding*
	  (llvm-declare-function "set_binding"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype '()
					    (llvm-voidtype))))
    (setf *init_global_frame*
	  (llvm-declare-function "init_global_frame"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype '()
					    *frame_p*)))
    (setf *get_global_frame*
	  (llvm-declare-function "get_global_frame"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype '()
					    *frame_p*)))
    (setf *extend_global_frame*
	  (llvm-declare-function "extend_global_frame"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype '()
					    *frame_p*)))
    (setf *shrink_global_frame*
	  (llvm-declare-function "shrink_global_frame"
				 fn-type)))

  (let ((fn-type (llvm-declare-functiontype (list *value_p*)
					    (llvm-inttype1))))
    (setf *is_value_true*
	  (llvm-declare-function "is_value_true"
				 fn-type))))

#|
(defun rt-build-new-value-int (value)
  (let ((llvm-value (llvm-const-int32 value)))
    (llvm-build-call *new_value_int*
		     (list llvm-value))))
|#

(defun rt-build-new-value (value)
  (cond
    ((integerp value)
     (let ((value-foreign (cffi:foreign-alloc :long
					      :initial-element value)))
       (llvm-build-call *new_value*
			(list #x01
			      value-foreign))))))

(defun rt-build-new-frame (&optional (parent-frame nil))
  (llvm-build-call *new_frame*
		   (list parent-frame)))

(defun rt-build-get-binding (frameindex varindex)
  (let ((frame (rt-get-activation-frame))
	(llvm-frameindex (llvm-const-int32 frameindex))
	(llvm-varindex (llvm-const-int32 varindex)))
    #|
    (llvm-debug-value *get_binding*)
    (llvm-debug-value frame)
    (llvm-debug-value llvm-frameindex)
    (llvm-debug-value llvm-varindex)
    (break)
    |#
    (llvm-build-call *get_binding*
		     (list frame
			   llvm-frameindex
			   llvm-varindex))))

(defun rt-get-local-activation-frame ()
  (let ((current-fn (llvm-get-current-function)))
    (llvm-get-param current-fn 0)))

(defun rt-get-global-activation-frame ()
  (llvm-build-call *get_global_frame*
		   '()))

(defun rt-get-activation-frame ()
  (if (llvm-in-main-function)
      (rt-get-global-activation-frame)
      (rt-get-local-activation-frame)))

(defun rt-create-init-activation-frame ()
  (llvm-build-call *init_global_frame*
		   '()))

(defun rt-build-call (fn &rest args)
  (let* ((activation-frame (rt-get-activation-frame))
	 (extended-activation-frame (rt-build-new-frame activation-frame))
	 (args-count (length args)))
    (loop
       for arg in args
       for idx from 0 to (- args-count 1)
       do (llvm-build-call *set_binding*
			   (list extended-activation-frame
				 0
				 idx
				 arg)))
    (llvm-build-call fn
		     (list activation-frame))))

(defun rt-build-is-true (value)
  (llvm-build-call *is_value_true*
		   (list value)))

(defun rt-build-conditional-jump (predicate truedest falsedest)
  (let ((is-true (rt-build-is-true predicate)))
    (llvm-build-conditional-jump is-true
				 truedest
				 falsedest)))
