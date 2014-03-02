(in-package :owlisp)

(export '(make-environment))



(defun make-environment ()
  (let ((global-env (make-keyvalue-map))
	(dynamic-env (make-keyvalue-map))
	(lexical-env (make-keyvalue-map))
	(environment (make-keyvalue-map)))
    (insert-into-keyvalue-map environment :global global-env)
    (insert-into-keyvalue-map environment :dynamic dynamic-env)
    (insert-into-keyvalue-map environment :lexical lexical-env)
    environment))
