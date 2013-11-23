(in-package :owlisp/llvm-ir)

(export '())



(cffi:define-foreign-library libllvm
  (t (:default "libLLVM-3.2")))

(cffi:use-foreign-library libllvm)
