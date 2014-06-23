(in-package :owlisp/cfg)

(export '())



(defun make-def-use-chain ()
  (let ((map '()))

    (lambda (action &rest args)
      (case action

	((:add-def (setf map
			 (acons (car args)
				nil
				map)))

	 (:add-use))))))
