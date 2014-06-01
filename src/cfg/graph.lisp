(in-package :owlisp/cfg)

(export '())



(defun make-node (&key (parents '()) (children '()))

  (labels ((notify-others (others exists-action add-action)
	     (loop
		for o in others
		do (when (not (funcall o exists-action #'self))
		     (funcall o add-action #'self))))

	   (notify-parents (parents)
	     (notify-others parents :is-child? :add-children))

	   (notify-children (children)
	     (notify-others children :is-parent? :add-parents))

	   (self (action &rest args)

	     (case action
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

	       (t (error "unknown action ~a" action)))))

    (notify-parents parents)
    (notify-children children)
    #'self))

(defun make-graph ()

  (let ((nodes '())
	(edges '())
	(id -1))

    (labels ((add-node (content)
	       (setf nodes
		     (cons (list (incf id)
				 content)
			   nodes)))

	     (add-edge (source-node-id dest-node-id)
	       (setf edges
		     (cons (list source-node-id
				 dest-node-id)
			   edges)))

	     (find-parents (node-id)
	       )

	     (find-children (node-id)
	       ))

     (lambda (action &rest args)
       (case action
	 ((:add-node (apply #'add-node args))
	  (:add-edge (apply #'add-edge args))))))))
