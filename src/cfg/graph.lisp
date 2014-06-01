(in-package :owlisp/cfg)

(export '())



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

	       (t (error "unknown action ~a" action)))))

    (notify-parents parents)
    (notify-children children)
    #'self))

