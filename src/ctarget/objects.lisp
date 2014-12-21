(in-package #:owlisp/c)

(export '(make-sequence/c
	  sequence/c-p
	  sequence/c-sequence

	  make-assignment/c
	  assignment/c-p
	  assignment/c-lvalue
	  assignment/c-value

	  make-extend-bindings/c
	  extend-bindings/c-p
	  extend-bindings/c-size

	  make-set-binding/c
	  set-binding/c-p
	  set-binding/c-frameindex
	  set-binding/c-varindex
	  set-binding/c-value

	  make-null/c
	  null/c-p

	  make-constant-int/c
	  constant-int/c-p
	  constant-int/c-value

	  make-constant-string/c
	  constant-string/c-p
	  constant-string/c-value

	  make-symbol/c
	  symbol/c-p
	  symbol/c-name

	  make-reference/c
	  reference/c-p
	  reference/c-frameindex
	  reference/c-varindex
;	  reference/c-symbol

	  make-function-reference/c
	  function-reference/c-p
	  function-reference/c-name

	  make-abstraction/c
	  abstraction/c-p
	  abstraction/c-name
	  abstraction/c-args
	  abstraction/c-body

	  make-application/c
	  application/c-fn
	  application/c-args

	  make-return/c
	  return/c-p
	  return/c-variable-name))


(defstruct sequence/c sequence)
(defstruct assignment/c lvalue value)
(defstruct extend-bindings/c size)
(defstruct set-binding/c frameindex varindex value)
(defstruct null/c)
(defstruct constant-int/c value)
(defstruct constant-string/c value)
(defstruct symbol/c name)
;(defstruct variable/c name)
;(defstruct reference/c symbol)
(defstruct reference/c frameindex varindex)
(defstruct function-reference/c name)
(defstruct abstraction/c name args body)
(defstruct application/c fn args)
(defstruct return/c variable-name)

(defgeneric get-return-var (obj))

(defmethod get-return-var ((obj list))
  (get-return-var (car (last obj))))

(defmethod get-return-var ((obj sequence/c))
  (get-return-var (sequence/c-sequence obj)))

(defmethod get-return-var ((obj assignment/c))
  (assignment/c-lvalue obj))
