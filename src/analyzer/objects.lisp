(in-package #:owlisp/analyzer)

(export '(make-null*
	  null*-p

	  make-constant-int*
	  constant-int*-p
	  constant-int*-value

	  make-constant-string*
	  constant-string*-p
	  constant-string*-value

	  make-symbol*
	  symbol*-p
	  symbol*-name

	  make-if*
	  if*-p
	  if*-cond
	  if*-then
	  if*-else

	  make-reference*
	  reference*-p
	  reference*-frameindex
	  reference*-varindex
;	  reference*-symbol

	  make-function-reference*
	  function-reference*-p
	  function-reference*-frameindex
	  function-reference*-varindex

;	  make-free-reference*
;	  free-reference*-p
;	  free-reference*-symbol

;	  make-bound-reference*
;	  bound-reference*-p
;	  bound-reference*-symbol

	  make-abstraction*
	  abstraction*-p
	  abstraction*-name
	  abstraction*-args
	  abstraction*-body

	  make-bindings*
	  bindings*-p
	  bindings*-bindings
	  bindings*-body

#|
	  make-closure*
	  closure*-p
	  closure*-args
	  closure*-body
	  closure*-env
|#

	  make-application*
	  application*-p
	  application*-fn
	  application*-args

	  make-set*
	  set*-p
	  set*-variable
	  set*-value

	  make-setf*
	  setf*-p
	  setf*-location
	  setf*-value

#|
	  make-symbol-function*
	  symbol-function*-p
	  symbol-function*-symbol
|#
	  ))


(defstruct null*)
(defstruct constant-int* value)
(defstruct constant-string* value)
(defstruct symbol* name)
(defstruct if* cond then else)
(defstruct reference* frameindex varindex)
(defstruct function-reference* frameindex varindex)
;(defstruct free-reference* symbol)
;(defstruct bound-reference* symbol)
(defstruct abstraction* name args body)
(defstruct bindings* bindings body) ; TODO: include type of binding (e.g. function, var)?
;(defstruct closure* args body env) ; TODO: make passed closure a separate field instead of assuming it's the first arg
(defstruct application* fn args)
(defstruct set* variable value)
(defstruct setf* location value)
;(defstruct symbol-function* symbol)
;(defstruct environment* parent symbols)


#|
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
|#
