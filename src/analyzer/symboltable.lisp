(in-package #:owlisp/analyzer)

(export '(make-symboltable
	  add-symbols
	  find-symbol-address
	  make-symbol-address
	  symbol-address-p
	  symbol-address-frameindex
	  symbol-address-symbolindex))


(defstruct symboltable
  (parent nil)
  (symbols '())) ; TODO: improve implementation

(defstruct symbol-address
  frameindex
  symbolindex)


(defun add-symbols (table symbols)
  (setf (symboltable-symbols table)
	(append (symboltable-symbols table)
		symbols)))

(defun find-symbol-address (table symbol &optional (frameindex 0))
  (if (not (null table))
      (let ((symbolindex (position symbol (symboltable-symbols table))))
	(if symbolindex
	    (make-symbol-address :frameindex frameindex
				 :symbolindex symbolindex)
	    (find-symbol-address (symboltable-parent table)
				 symbol
				 (1+ frameindex))))
      (error "unknown symbol ~a" symbol)))
