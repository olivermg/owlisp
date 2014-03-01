(in-package :owlisp)

(export '(make-environment))



(defun make-environment ()
  (let ((dynamic-env (make-keyvalue-map))
	(lexical-env (make-keyvalue-map))
	(environment (make-keyvalue-map)))
    (insert-into-keyvalue-map environment :dynamic dynamic-env)
    (insert-into-keyvalue-map environment :lexical lexical-env)
    environment))
