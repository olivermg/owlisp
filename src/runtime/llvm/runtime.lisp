(in-package :owlisp/runtime)

(export '(RT-NEW-VALUE-INT
	  RT-NEW-VALUE
	  RT-FREE-VALUE
	  RT-VALUES-EQUAL
	  RT-NEW-FRAME
	  RT-FREE-FRAME
	  RT-SET-BINDING
	  RT-GET-BINDING))



(defun RT-NEW-VALUE-INT (value)
  (new_value_int value))

(defun RT-NEW-VALUE (value)
  (RT-NEW-VALUE-INT value))

(defun RT-FREE_VALUE (value)
  (free_value value))

(defun RT-VALUES-EQUAL (value1 value2)
  (values_equal value1 value2))


(defun RT-NEW-FRAME (&optional (parent-frame (cffi:null-pointer)))
  (new_frame parent-frame))

(defun RT-FREE-FRAME (frame)
  (free_frame frame))

(defun RT-SET-BINDING (frame frameindex varindex value)
  (set_binding frame frameindex varindex value))

(defun RT-GET-BINDING (frame frameindex varindex)
  (get_binding frame frameindex varindex))
