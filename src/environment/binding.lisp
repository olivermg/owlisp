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
			 (lookup-in-frame (cdr address))
			 (lookup-in-parent address current-frameindex))
		     (error "binding not found"))))

	     (set-value (value address)
	       (let ((frameindex (first address))
		     (varindex (cdr address)))
		 (if (= frameindex 0)
		     (setf (svref bindings varindex)
			   value)
		     (send-message parent :set-value
				   value
				   (cons (- frameindex 1)
					 varindex)))))

	     (set-current-bindings (new-bindings)
	       (setf bindings (coerce new-bindings 'vector)))

	     (get-current-bindings ()
	       (coerce bindings 'list)))

      (lambda (msg)
	(case msg
	  (:lookup #'lookup)
	  (:set-value #'set-value)
	  (:set-current-bindings #'set-current-bindings)
	  (:get-current-bindings #'get-current-bindings)
	  (t (error "unknown message")))))))
