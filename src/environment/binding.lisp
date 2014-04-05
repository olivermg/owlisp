(in-package :owlisp/environment)

(export '(env.b.extend))



(defun env.b.extend (values &optional parent)
  (format t "ENV.B.EXTEND: ~a~%" values)
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
	       (format t "ENV.B.LOOKUP: ~a ~a~%" address current-frameindex)
	       (let ((frameindex (first address)))
		 (if (>= frameindex 0)
		     (if (= current-frameindex frameindex)
			 (lookup-in-frame (rest address))
			 (lookup-in-parent address current-frameindex))
		     (error "binding not found")))))
      (lambda (msg)
	(case msg
	  (:lookup #'lookup)
	  (t (error "unknown message")))))))
