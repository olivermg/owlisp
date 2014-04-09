(in-package :owlisp/environment)

(export '(env.b.extend))



(defun env.b.extend (values &optional parent)
  (let ((bindings (coerce values 'vector)))
    (labels ((lookup-in-frame (varindex)
	       (if (< varindex (length bindings))
		   (svref bindings varindex)
		   (error "binding not found")))

	     (lookup-in-parent (address current-frameindex)
	       (if parent
		   (send-message parent :lookup
				 address
				 (1+ current-frameindex))
		   (error "binding not found")))

	     (lookup (address &optional (current-frameindex 0))
	       (let ((frameindex (first address)))
		 (if (>= frameindex 0)
		     (if (= current-frameindex frameindex)
			 (lookup-in-frame (rest address))
			 (lookup-in-parent address current-frameindex))
		     (error "binding not found"))))

	     (set-value (value varindex) ; TODO: allow address here instead of just varindex?
	       (setf (svref bindings varindex)
		     value))

	     (get-current-bindings ()
	       (coerce bindings 'list)))

      (lambda (msg)
	(case msg
	  (:lookup #'lookup)
	  (:set-value #'set-value)
	  (:get-current-bindings #'get-current-bindings)
	  (t (error "unknown message")))))))
