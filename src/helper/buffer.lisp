(in-package :owlisp/helper)

(export '(make-string-buffer))


(defun make-string-buffer ()
  (make-array '(0)
	      :element-type 'base-char
	      :fill-pointer 0
	      :adjustable t))
