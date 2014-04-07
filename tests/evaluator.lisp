(in-package :owlisp/evaluator/tests)

(def-suite owlisp/evaluator/tests)

(in-suite owlisp/evaluator/tests)



(test constant
  (is (= (evaluate-form 5
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 5)))

(test let
  (is (= (evaluate-form '(let ((a 22))
			  a)
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 22)))

(test lambda
  (is (= (evaluate-form '((lambda (a)
			    a) 44)
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 44)))

(test if-then
  (is (= (evaluate-form '(if 0 11 22)
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 11)))

(test if-else
  (is (= (evaluate-form '(if nil 11 22)
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 22)))

(test let-nested-lexical
  (is (= (evaluate-form '(let ((a 11))
			  (let ((b 22))
			    a))
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 11)))

(test lambda-nested-lexical
  (is (= (evaluate-form '(((lambda (a)
			      (lambda (b)
				a))
			   22)
			  11)
			(env.d.extend '())
			(env.b.extend '())
			(make-machine))
	 22)))
