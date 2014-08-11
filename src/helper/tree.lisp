(in-package :owlisp/helper)

(export '())


(defun traverse (tree fn &key (order :pre) (onlyleaves nil))

  (labels ((visit ()
	     (when (or (not onlyleaves)
		       (atom tree))
	       (funcall fn tree))))

    (when (eq order :pre)
      (visit))

    (when (consp tree)
      (mapcar #'(lambda (subtree)
		  (traverse subtree fn
			    :order order
			    :onlyleaves onlyleaves))
	      tree))

    (when (eq order :post)
      (visit))))
