(in-package #:owlisp/c)

(export '())


(defstruct sequence/c sequence)
(defstruct assignment/c lvalue value)
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
