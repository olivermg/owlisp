(in-package :owlisp)

(export '(make-environment
	  update-in-environment
	  lookup-in-environment))



(defun make-environment ()
  (make-keyvalue-map))

(defun update-in-environment (env key-or-keys value-or-values)
  (if (atom key-or-keys)
      (update-in-keyvalue-map env key-or-keys value-or-values)
      (progn
	(loop
	   for key in key-or-keys
	   for value in value-or-values
	   do (update-in-keyvalue-map env key value))
	env)))

(defun lookup-in-environment (env key)
  (lookup-in-keyvalue-map env key))

(defun delete-from-environment (env key)
  (delete-from-keyvalue-map env key))
