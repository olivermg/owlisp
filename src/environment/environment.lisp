(in-package :owlisp)

(export '(make-environment
	  update-in-environment
	  lookup-in-environment
	  update-current-package-in-environment))



(defstruct frame
  (lookup-table (make-keyvalue-map))
  parent)



(defun find-in-environment (env key)
  (if env
      (multiple-value-bind (value found)
	  (lookup-in-keyvalue-map (frame-lookup-table env)
				  key)
	(if found
	    value
	    (find-in-environment (parent-environment env)
				 key)))
      (error 'unknown-form
	     :name key)))

(defun set-in-environment (env key value)
  (update-in-keyvalue-map (frame-lookup-table env)
			  key
			  value)
  env)



(defun make-environment (&optional parent-env)
  (make-frame :parent parent-env))

(defun parent-environment (env)
  (frame-parent env))

(defun lookup-in-environment (env key &key (scope :lexical) (type :primitive))
  (find-in-environment env
		       (qualified-key scope type (get-current-package env) key)))

(defun update-in-environment (env key-or-keys value-or-values &key (scope :lexical) (type :primitive))
  (if (atom key-or-keys)
      (set-in-environment env
			  (qualified-key scope type (get-current-package env) key-or-keys)
			  value-or-values)
      (loop
	 for key in key-or-keys
	 for value in value-or-values
	 do (set-in-environment env
				(qualified-key scope type (get-current-package env) key)
				value)))
  env)



(defun qualified-key (scope type package unqualified-key)
  (intern (concatenate 'string
		       (symbol-name scope) ":"
		       (symbol-name type) ":"
		       (symbol-name package) ":"
		       (symbol-name unqualified-key))
	  'keyword))



(defun update-current-package-in-environment (env name)
  (declare (ignore env name)))

(defun get-current-package (env)
  (declare (ignore env))
  :cl-user)
