(in-package :owlisp/analyzer)

(export '(make-constant*
	  constant*-p
	  constant*-value
	  make-symbol*
	  symbol*-p
	  symbol*-name
	  make-reference*
	  reference*-p
	  reference*-symbol
	  make-free-reference*
	  free-reference*-p
	  free-reference*-symbol
	  make-bound-reference*
	  bound-reference*-p
	  bound-reference*-symbol
	  make-abstraction*
	  abstraction*-p
	  abstraction*-args
	  abstraction*-body
	  make-closure*
	  closure*-p
	  closure*-args
	  closure*-body
	  closure*-env
	  make-application*
	  application*-p
	  application*-fn
	  application*-args))


(defstruct constant* value)
(defstruct symbol* name)
(defstruct reference* symbol)
(defstruct free-reference* symbol)
(defstruct bound-reference* symbol)
(defstruct abstraction* args body)
(defstruct closure* args body env) ; TODO: make passed closure a separate field instead of assuming it's the first arg
(defstruct application* fn args)
(defstruct environment* parent symbols)


(defgeneric lookup (env symbol))
(defgeneric invoke (obj &rest args))
(defgeneric dump (obj))


(defmethod lookup ((env environment*) (symbol symbol))
  (labels ((lookup-rec (env frameindex)
	     (if (null env)
		 (error "unknown symbol ~a" symbol)
		 (let ((pos (position symbol (environment*-symbols env))))
		   (if pos
		       (cons frameindex pos)
		       (lookup-rec (environment*-parent env)
				   (1+ frameindex)))))))
    (lookup-rec env 0)))

(defmethod lookup ((env environment*) (symbol symbol*))
  (lookup env
	  (symbol*-name symbol)))


(defmethod invoke ((obj list) &rest args)
  (cl:apply obj args))

(defmethod invoke ((obj closure*) &rest args)
  (cl:apply (closure*-body obj) obj args)) ; FIXME
