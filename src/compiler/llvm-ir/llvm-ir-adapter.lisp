(in-package :owlisp/llvm-ir)

(export '(i32))



(defmacro ir (&rest args)
  `(format nil "~{~a~^ ~}" ',args))



(defun i32 ()
  (ir |i32|))
