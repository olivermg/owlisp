(in-package :owlisp/llvm-ir)

(export '())



;(defvar +LLVM-LIBNAME+ "libLLVM-3.2")
(defvar +OUTPUTFILE+ "outfile")

(defparameter *context* nil)
(defparameter *module* nil)
(defparameter *builder* nil)
(defparameter *functions* nil)
(defparameter *llvm-default-type* nil)
