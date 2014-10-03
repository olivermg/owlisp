(in-package :owlisp/helper)

(export '(let/dynamic))


(defmacro let/dynamic ((&rest definitions) &body body)
  `(let (,@definitions)
     (declare (special ,@(mapcar #'car definitions)))
     ,@body))
