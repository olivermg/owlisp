(in-package #:owlisp/c)

(export '(make-sequence/c
	  sequence/c-p
	  sequence/c-sequence
	  make-assignment/c
	  assignment/c-p
	  assignment/c-lvalue
	  assignment/c-value
	  make-constant/c
	  constant/c-p
	  constant/c-value
	  make-reference/c
	  reference/c-p
	  reference/c-symbol
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
(defstruct constant/c value)
;(defstruct variable/c name)
(defstruct reference/c symbol)
(defstruct abstraction/c name args body)
(defstruct application/c fn args)
(defstruct return/c variable-name)

(defgeneric get-return-var (obj))

(defmethod get-return-var ((obj sequence/c))
  (get-return-var (car (last (sequence/c-sequence obj)))))

(defmethod get-return-var ((obj assignment/c))
  (assignment/c-lvalue obj))
