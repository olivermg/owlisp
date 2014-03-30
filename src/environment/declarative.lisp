(in-package :owlisp/environment)

(export '(env.d.extend))



(defun env.d.extend (names &optional parent)
  (let ((declarations (coerce names 'vector)))
    (labels ((address-in-parent (name frameindex)
	       (if parent
		   (send-message parent :address
				 name
				 (1+ frameindex))
		   (error "unknown declaration")))
	     (address (name &optional (frameindex 0))
	       (let ((varindex (position name declarations)))
		 (if varindex
		     (cons frameindex varindex)
		     (address-in-parent name frameindex)))))
      (lambda (msg)
	(case msg
	  (:address #'address)
	  (t (error "unknown message")))))))
