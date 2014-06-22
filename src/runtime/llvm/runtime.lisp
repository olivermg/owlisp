(in-package :owlisp/runtime)

(export '(RT-NEW-VALUE-INT
	  RT-NEW-VALUE
	  RT-FREE-VALUE
	  RT-VALUES-EQUAL
	  RT-DUMP-VALUE
	  RT-NEW-FRAME
	  RT-FREE-FRAME
	  RT-SET-BINDING
	  RT-GET-BINDING
	  RT-DUMP-FRAME
	  RT-SET-BINDINGS))



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

(defun pop-position ()
  (let ((left-bb (first *bb-position-stack*)))
    (setf *bb-position-stack*
	  (rest *bb-position-stack*))
    left-bb))

(defun add-activation-frame ()
  (let ((new-frame (RT-NEW-FRAME *activation-frame*)))
    (setf *activation-frame* new-frame)
    new-frame))

(defun drop-activation-frame ()
  (setf *activation-frame*
	(rest *activation-frame*))
  (first *activation-frame*))



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



(defun RT-DECLARE-RUNTIME-FUNCTIONS ()
  (let* ((fn-type (cffi:with-foreign-object
		      (types :pointer 1)
		    (setf (cffi:mem-aref types :pointer 0)
			  (LLVMInt32type))
		    (LLVMFunctiontype (LLVMPointertype *value_t*
						       *addressspace*)
				      types
				      1
				      0))))
   (LLVMAddFunction *module*
		    "new_value_int"
		    )))

(defun RT-BUILD-NEW-VALUE-INT (value)
  (LLVMBuildCall *builder*
		 ))
