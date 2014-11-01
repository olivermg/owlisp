(in-package :owlisp/c/tests)

(def-suite owlisp/c/tests)

(in-suite owlisp/c/tests)


(defparameter *expr-constant*
  (make-constant* :value 123))

(defparameter *expr-reference*
  (make-reference* :symbol 'a))

(defparameter *expr-abstraction*
  (make-abstraction* :args '(a b)
		     :body *expr-reference*))

(defparameter *expr-application*
  (make-application* :fn *expr-abstraction*
		     :args '(11 22)))


(test constant1
  (let ((restructured (do-restructure *expr-constant*)))
    (is (equal 'owlisp/c::assignment/c
	       (type-of restructured)))
    (is (equalp (make-constant/c :value (constant*-value *expr-constant*))
		(assignment/c-value restructured)))))
