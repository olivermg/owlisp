(in-package :owlisp/c/tests)

(def-suite owlisp/c/tests)

(in-suite owlisp/c/tests)


(defparameter *expr-constant*
  (make-constant* :value 123))

(defparameter *expr-reference*
  (make-reference* :symbol 'a))

(defparameter *expr-abstraction*
  (make-abstraction* :args '(a b)
		     :body (list *expr-reference*)))

(defparameter *expr-application*
  (make-application* :fn *expr-abstraction*
		     :args '(11 22)))


(test constant
  (let ((restructured (do-restructure *expr-constant*)))
    (is (equal 'owlisp/c::assignment/c
	       (type-of restructured)))
    (is (equalp (make-constant/c :value (constant*-value *expr-constant*))
		(assignment/c-value restructured)))))

(test reference
  (let ((restructured (do-restructure *expr-reference*)))
    (is (equal 'owlisp/c::assignment/c
	       (type-of restructured)))
    (is (equalp (make-reference/c :symbol (reference*-symbol *expr-reference*))
		(assignment/c-value restructured)))))

(test abstraction
  (let ((restructured (do-restructure *expr-abstraction*)))
    (is (equal 'owlisp/c::assignment/c
	       (type-of restructured)))
    (let ((restructured-abstraction (assignment/c-value restructured)))
      (is (equal 'owlisp/c::abstraction/c
		 (type-of restructured-abstraction)))
      (is (equalp '(a b)
		  (abstraction/c-args restructured-abstraction)))
      (let ((body (abstraction/c-body restructured-abstraction)))
	(is (equal 'owlisp/c::assignment/c
		   (type-of (first body))))
	(is (equal 'owlisp/c::return/c
		   (type-of (second body))))))))
