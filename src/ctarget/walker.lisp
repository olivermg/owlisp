(in-package :owlisp/c)

(export '(do-dump))


(defparameter *dumper-walker*

  (make-walker

    (declare (ignore #'walk-sequence))

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
      (dump-reference (reference*-symbol obj)))

    (defrule
	#'abstraction*-p
	(obj nil)
      (dump-fndefinition (abstraction*-args obj)
			 (walk-sequence-last
			  (abstraction*-body obj)))) ; TODO: auto-return last value

    (defrule
	#'closure*-p
	(obj nil)
      (dump-fndefinition (abstraction*-args obj)
			 (walk-sequence-last
			  (abstraction*-body obj)))) ; TODO: auto-return last value

    (defrule
	#'application*-p
	(obj nil)
      (dump-application (application*-fn obj)
			(application*-args obj)))))


(defun do-dump (expr)
  (funcall #'(lambda ()
	       (with-dumper
		 (funcall *dumper-walker*
			  expr)))))
