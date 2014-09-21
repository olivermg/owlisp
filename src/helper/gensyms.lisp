(in-package :owlisp/helper)

(export '(with-gensyms))

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (gensym)))
		 syms)
     ,@body))
