(in-package :owlisp/llvm-ir)

(export '(i32))



(defmacro ir (&rest args)
  `(format nil "~{~a~^ ~}" (list ,@args)))



;; native types

(defmacro i32 ()
  (ir '|i32|))



;; functions

(defmacro define_ (return-type name &rest parameter-types)
  (ir '|define| return-type name parameter-types))
