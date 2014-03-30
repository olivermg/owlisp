(in-package :owlisp/environment)

(export '(env.d.extend))



(defun env.d.extend (names &optional parent)
  (let ((declarations (coerce names 'vector)))
    (labels ((address (name &optional (frameindex 0))
	       (let ((varindex (position name declarations)))
		 (if varindex
		     (cons frameindex varindex)
		     (funcall (funcall parent :address)
			      name
			      (1+ frameindex))))))
      (lambda (msg)
	(case msg
	  (:address #'address))))))
