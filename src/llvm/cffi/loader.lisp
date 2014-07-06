(in-package :owlisp/llvm)



(defun load-llvm-library ()
  (cffi:define-foreign-library libllvm
    (t (:default "libLLVM-3.4")))
  (cffi:use-foreign-library libllvm))

(load-llvm-library)
