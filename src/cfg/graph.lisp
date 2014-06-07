(in-package :owlisp/cfg)

(export '(make-node
	  make-graph))



(defun make-node (&key content (parents '()) (children '()))

  (labels ((notify-others (others exists-action add-action)
	     (loop
		for o in others
		do (when (not (funcall o exists-action #'self))
		     (funcall o add-action #'self))))

	   (notify-parents (parents)
	     (notify-others parents :is-child? :add-children))

	   (notify-children (children)
	     (notify-others children :is-parent? :add-parents))

	   (print_ (&optional (indentation ""))
	     (format t "~aCONTENT:~a (~a)~%~a CHILDREN:~a~%~%"
		     indentation content #'self
		     indentation children))

	   (self (action &rest args)

	     (case action

	       ((:content) content)

	       ((:parents) parents)

	       ((:children) children)

	       ((:is-parent?) (member (car args)
				      parents))

	       ((:is-child?) (member (car args)
				     children))

	       ((:add-parents)
		(setf parents
		      (union args
			     parents))
		(notify-parents args))

	       ((:add-children)
		(setf children
		      (union args
			     children))
		(notify-children args))

	       ((:append-content)
		(setf content
		      (append content
			      args)))

	       ((:print)
		(apply #'print_
		       (if args
			   (list (car args)))))

	       (t (error "unknown action ~a" action)))))

    (notify-parents parents)
    (notify-children children)
    #'self))



(defun make-graph ()

  (let ((root-node (make-node)))

    (labels ((traverse-node (node node-action)
	       (declare (special visited))
	       (when (and node
			  (not (member node visited)))
		 (setf visited
		       (adjoin node visited))
		 (funcall node-action node)
		 (loop
		    for child in (funcall node :children)
		    collect (traverse-node child node-action))))

	     (traverse (node-action)
	       (let ((visited '()))
		 (declare (special visited))
		 (traverse-node root-node node-action))))

      #'(lambda (action &rest args)

	  (case action

	    ((:root-node) root-node)

	    ((:traverse) (traverse (car args)))

	    ((:print) (traverse #'(lambda (node)
				    (funcall node :print))))

	    (t (error "unknown action ~a" action)))))))
