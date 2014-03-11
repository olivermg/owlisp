(in-package :owlisp)

(export '(make-environment
	  update-in-environment
	  lookup-in-environment
	  update-current-package-in-environment))



(defstruct owlisp-environment
  (current-package :cl-user)
  (map (make-keyvalue-map)))

(defun make-environment ()
  (make-owlisp-environment))

(defun qualified-key (scope type env unqualified-key)
  (intern (concatenate 'string
		       (symbol-name scope)
		       ":"
		       (symbol-name type)
		       ":"
		       (symbol-name (owlisp-environment-current-package env))
		       ":"
		       (symbol-name unqualified-key))
	  'keyword))

(defun update-in-environment (env key-or-keys value-or-values &key (scope :lexical) (type :primitive))
  (if (atom key-or-keys)
      (update-in-keyvalue-map (owlisp-environment-map env)
			      (qualified-key scope type env key-or-keys)
			      value-or-values)
      (loop
	 for key in key-or-keys
	 for value in value-or-values
	 do (update-in-keyvalue-map (owlisp-environment-map env)
				    (qualified-key scope type env key)
				    value)))
  env)

(defun update-current-package-in-environment (env name)
  (setf (owlisp-environment-current-package env)
	name))

(defun lookup-in-environment (env key &key (scope :lexical) (type :primitive))
  (let ((value (lookup-in-keyvalue-map (owlisp-environment-map env)
				       (qualified-key scope type env key))))
    (if value
	value
	(error 'unknown-form
	       :name key))))

(defun delete-from-environment (env key &key (scope :lexical) (type :primitive))
  (delete-from-keyvalue-map (owlisp-environment-map env)
			    (qualified-key scope type env key)))
