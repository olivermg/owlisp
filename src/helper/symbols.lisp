(in-package :owlisp/helper)

(export '(symbol->keyword
	  prefix-symbol))



(defun symbol->keyword (symbol)
  (intern (symbol-name symbol)
	  :keyword))

(defun prefix-symbol (symbol prefix)
  (intern (concatenate 'string
		       prefix
		       (symbol-name symbol))
	  (symbol-package symbol)))
