(in-package :owlisp)

(export '(symbol->keyword))



(defun symbol->keyword (symbol)
  (intern (symbol-name symbol)
	  :keyword))
