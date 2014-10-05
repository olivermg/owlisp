(in-package :owlisp/c)

(export '(*dumper-walker*))


(defparameter *dumper-walker*

  (make-walker

    (defrule

	#'constant*-p

	(obj nil)

      (dump-constant (constant*-value obj)))))
