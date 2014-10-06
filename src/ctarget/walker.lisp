(in-package :owlisp/c)

(export '(*dumper-walker*))


(defparameter *dumper-walker*

  (make-walker

    (defrule

	#'constant*-p

	(obj nil)

      (dump-constant (constant*-value obj)))

    (defrule

	#'symbol*-p

	(obj nil)

      (error "not implemented yet"))

    (defrule

	#'reference*-p

	(obj nil)

      (dump-reference (reference*-symbol obj) ; TODO: change this to frame- & varindex
		      (reference*-symbol obj)))

    (defrule

	#'abstraction*-p

	(obj nil)

      (dump-fndefinition-start ()))))
