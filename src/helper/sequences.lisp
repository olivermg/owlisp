(in-package :owlisp)

(export '(duplicate-elements
	  interleave-lists
	  lookup-alist-value
	  make-keyvalue-map
	  insert-into-keyvalue-map
	  lookup-in-keyvalue-map
	  update-in-keyvalue-map
	  delete-from-keyvalue-map))



(defun duplicate-elements (sequence)
  (reduce
   (lambda (sum e)
     (append sum
	     (list e e)))
   sequence
   :initial-value '()))

(defun interleave-lists (l1 l2)
  (labels ((interleave-lists-sub (li l1 l2)
	     (let ((e1 (car l1))
		   (r1 (cdr l1)))
	       (if e1
		   (interleave-lists-sub (append li (list e1))
					 l2
					 r1)
		   li))))
    (interleave-lists-sub '() l1 l2)))

(defun lookup-alist-value (alist key &key (testfn #'eql))
  (cdr (assoc key alist :test testfn)))



(defun make-keyvalue-map ()
  (make-hash-table :test #'equalp))

(defun update-in-keyvalue-map (map key value)
  (setf (gethash key map)
	value)
  map)

(defun lookup-in-keyvalue-map (map key)
  (gethash key map))

(defun delete-from-keyvalue-map (map key)
  (remhash key map)
  map)
