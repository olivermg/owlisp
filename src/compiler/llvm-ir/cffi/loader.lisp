(in-package :owlisp/llvm-ir)

(export '(load-llvm-library))



(cffi:define-foreign-library libllvm
  (t (:default "libLLVM-3.2")))

(cffi:use-foreign-library libllvm)

(defun load-llvm-library ()
  (break)
  (cffi:define-foreign-library libllvm-runtime
    (t (:default "libLLVM-3.2")))
  (break)
  (cffi:use-foreign-library libllvm-runtime)
  (break))
