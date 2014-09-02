(in-package #:owlisp)

(defun env.extend (symbols &optional parent)
  (lambda (symbol &optional (curfi 0))
    (let ((vi (position symbol symbols)))
      (if vi
	  `(,curfi . ,vi)
	  (if parent
	      (funcall parent symbol (1+ curfi))
	      (error "unknown symbol ~a" symbol))))))
