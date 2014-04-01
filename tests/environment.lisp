(in-package :owlisp/environment/tests)

(def-suite owlisp/environment/tests)

(in-suite owlisp/environment/tests)



(defun make-env.d ()
  (env.d.extend '(a b c)
		(env.d.extend '(c d e))))

(defun make-env.b ()
  (env.b.extend '(1 2 3)
		(env.b.extend '(4 5 6))))



(test correct-address-a
  (let ((env (make-env.d)))
    (is (equal (send-message env :address 'a)
	       '(0 . 0)))))

(test correct-address-b
  (let ((env (make-env.d)))
    (is (equal (send-message env :address 'b)
	       '(0 . 1)))))

(test correct-address-c
  (let ((env (make-env.d)))
    (is (equal (send-message env :address 'c)
	       '(0 . 2)))))

(test correct-address-d
  (let ((env (make-env.d)))
    (is (equal (send-message env :address 'd)
	       '(1 . 1)))))

(test correct-address-e
  (let ((env (make-env.d)))
    (is (equal (send-message env :address 'e)
	       '(1 . 2)))))



(test correct-value-1
  (let ((env (make-env.b)))
    (is (= (send-message env :lookup '(0 . 0))
	   1))))

(test correct-value-2
  (let ((env (make-env.b)))
    (is (= (send-message env :lookup '(0 . 1))
	   2))))

(test correct-value-3
  (let ((env (make-env.b)))
    (is (= (send-message env :lookup '(0 . 2))
	   3))))

(test correct-value-4
  (let ((env (make-env.b)))
    (is (= (send-message env :lookup '(1 . 0))
	   4))))

(test correct-value-5
  (let ((env (make-env.b)))
    (is (= (send-message env :lookup '(1 . 1))
	   5))))

(test correct-value-6
  (let ((env (make-env.b)))
    (is (= (send-message env :lookup '(1 . 2))
	   6))))
