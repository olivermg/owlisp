(in-package #:owlisp/c)

(export '())


(defstruct sequence/c sequence)
(defstruct assignment/c lvalue value)
(defstruct reference/c lvalue symbol)
(defstruct abstraction/c name args body)
(defstruct application/c fn args)
(defstruct return/c value)
