(in-package :owlisp/llvm-ir)

(export '(llvm-ir))

(defmacro llvm-ir (&body ir-text)
  (loop
     for ir-expr
     in ir-text
     do (format t "~a" ir-expr)))
