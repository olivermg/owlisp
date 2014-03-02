(in-package :owlisp)

(export '(make-environment))



(defun make-specific-environment ()
  (let ((primitives-namespace (make-keyvalue-map))
	(functions-namespace (make-keyvalue-map))
	(specific-environment (make-keyvalue-map)))
    (insert-into-keyvalue-map
     (insert-into-keyvalue-map
      (insert-into-keyvalue-map
       (insert-into-keyvalue-map
	(insert-into-keyvalue-map
	 (insert-into-keyvalue-map
	  specific-environment
	  :primitives primitives-namespace)
	 :functions functions-namespace)
	:lookup-primitive (lambda (name)
			    (lookup-keyvalue-map primitives-namespace name)))
       :lookup-function (lambda (name)
			  (lookup-keyvalue-map functions-namespace name)))
      :set-primitive (lambda (name value)
		       (insert-into-keyvalue-map primitives-namespace name value)))
     :set-function (lambda (name value)
		     (insert-into-keyvalue-map functions-namespace name value)))))

(defun lookup-value-in-specific-environment (env name &optional (type :primitive))
  (cond ((equalp type :primitive) [env lookup-primitive name])
	((equalp type :function) [env lookup-function name])))

(defun set-value-in-specific-environment (env name value &optional (type :primitive))
  (cond ((equalp type :primitive) [env set-primitive name value])
	((equalp type :function) [env set-function name value])))

(defun make-environment ()
  (let ((global-env (make-specific-environment))
	(dynamic-env (make-specific-environment))
	(lexical-env (make-specific-environment))
	(environment (make-keyvalue-map)))
    (insert-into-keyvalue-map
     (insert-into-keyvalue-map
      (insert-into-keyvalue-map
       (insert-into-keyvalue-map
	environment
	:global global-env)
       :dynamic dynamic-env)
      :lexical lexical-env)
     :lookup-global-primitive (lambda (name)
				))))
