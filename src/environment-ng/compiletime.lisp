(in-package :owlisp/environment)

(export '(enter
	  lookup
	  update!
	  leave))


(defclass environment ()
  ((parent :initarg :parent
	   :initform nil
	   :accessor env-parent)
   (bindings :initarg :bindings
	     :initform '()
	     :accessor env-bindings)))


(defgeneric enter (&key parent symbols))
(defgeneric lookup (env symbol))
(defgeneric update! (env symbol value))
(defgeneric leave (env))


(defmethod enter (&key parent symbols)
  (let ((bindings (mapcar #'(lambda (sym)
			      (cons sym nil))
			  symbols)))
    (make-instance 'environment
		   :parent parent
		   :bindings bindings)))

(defmethod lookup ((env null) symbol)
  (error "unbound symbol: ~a" symbol))
(defmethod lookup ((env environment) symbol)
  (let ((binding (assoc symbol
			(env-bindings env))))
    (if binding
	(cdr binding)
	(lookup (env-parent env) symbol))))

(defmethod update! ((env null) symbol value)
  (error "cannot update binding of symbol ~a to value ~a in nil environment!" symbol value))
(defmethod update! ((env environment) symbol value)
  (setf (env-bindings env)
	(acons symbol
	       value
	       (env-bindings env))))

(defmethod leave ((env null))
  (error "cannot leave nil environment!"))
(defmethod leave ((env environment))
  (env-parent env))
