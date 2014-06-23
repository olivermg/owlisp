(in-package :owlisp/llvm)



(defun load-runtime-library ()
  (cffi:define-foreign-library libruntime
    (t (:default "libowlisp-rt")))
  (cffi:use-foreign-library libruntime))

(load-runtime-library)

