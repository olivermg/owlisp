(in-package :owlisp/environment)

(export '(env.d.extend))



(defun env.d.extend (names &optional parent)
  (format t "ENV.D.EXTEND: ~a~%" names)
  (let ((declarations (coerce (mapcar #'symbol-name names)
			      'vector)))
    (labels ((address-in-parent (name frameindex)
	       (if parent
		   (send-message parent :address
				 name
				 (1+ frameindex))
		   (error "unknown declaration")))
	     (address (name &optional (frameindex 0))
	       (format t "ENV.D.ADDRESS: ~a ~a~%" name frameindex)
	       (let ((varindex (position (symbol-name name)
					 declarations
					 :test #'string-equal)))
		 (if varindex
		     (cons frameindex varindex)
		     (address-in-parent name frameindex)))))
      (lambda (msg)
	(case msg
	  (:address #'address)
	  (t (error "unknown message")))))))
