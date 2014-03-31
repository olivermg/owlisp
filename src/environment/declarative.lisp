(in-package :owlisp/environment)

(export '(env.d.extend))



(defun env.d.extend (names &optional parent)
  (let ((declarations (coerce (mapcar #'symbol-name names)
			      'vector)))
    (labels ((address-in-parent (name frameindex)
	       (if parent
		   (send-message parent :address
				 name
				 (1+ frameindex))
		   (error "unknown declaration")))
	     (address (name &optional (frameindex 0))
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
