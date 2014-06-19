(in-package :owlisp/runtime)

(export '(load-runtime-library))



(defun load-runtime-library ()
  (cffi:define-foreign-library libowlisp-rt
    (t (:default "libowlisp-rt")))
  (cffi:use-foreign-library libowlisp-rt))

(load-runtime-library)
