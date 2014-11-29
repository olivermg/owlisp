(in-package #:owlisp/analyzer)

(export '(make-symboltable
	  add-symbols
	  symbol-address))


(defstruct symboltable
  (parent nil)
  (symbols '())) ; TODO: improve implementation


(defun add-symbols (table symbols)
  (setf (symboltable-symbols table)
	(append (symboltable-symbols table)
		symbols)))

(defun symbol-address (table symbol &optional (frameindex 0))
  (if (not (null table))
      (let ((varindex (position symbol (symboltable-symbols table))))
	(if varindex
	    (values frameindex varindex)
	    (symbol-address (symboltable-parent table)
			    symbol
			    (1+ frameindex))))
      (error "unknown symbol ~a" symbol)))
