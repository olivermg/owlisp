(in-package :owlisp/llvm-ir)

(export '(load-llvm-library))



(defun load-llvm-library ()
  (cffi:define-foreign-library libllvm-runtime
    (t (:default "libLLVM-3.2")))
  (cffi:use-foreign-library libllvm-runtime))
