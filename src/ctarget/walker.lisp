(in-package :owlisp/c)

(export '(*dumper-walker*))


(defparameter *dumper-walker*

  (with-dumper

      (make-walker

	(declare (ignore #'walk-sequence #'walk-sequence-last))

	(defrule
	    #'constant*-p
	    (obj nil)
	  (dump-constant (constant*-value obj)))

	(defrule
	    #'symbol*-p
	    (obj nil)
	  (declare (ignore obj))
	  (error "not implemented yet"))

	(defrule
	    #'reference*-p
	    (obj nil)
	  (dump-reference (reference*-symbol obj) ; TODO: change this to frame- & varindex
			  (reference*-symbol obj)))

	(defrule
	    #'abstraction*-p
	    (obj nil)
	  (dump-fndefinition-start (abstraction*-args obj))
	  (dump-fndefinition (abstraction*-body obj))
	  (dump-fndefinition-end 0))	; TODO: auto-return last value

	(defrule
	    #'closure*-p
	    (obj nil)
	  (dump-fndefinition-start (abstraction*-args obj)) ; TODO: make use of lexical environment
	  (dump-fndefinition (abstraction*-body obj))
	  (dump-fndefinition-end 0))	; TODO: auto-return last value

	(defrule
	    #'application*-p
	    (obj nil)
	  (dump-application (application*-fn obj)
			    (application*-args obj))))))
