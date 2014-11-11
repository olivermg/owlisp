(in-package #:owlisp/compiler/tests)

(def-suite owlisp/compiler/tests)

(in-suite owlisp/compiler/tests)


(defparameter *expr1*
  '(lambda (a b) a))

(defparameter *expr2*
  '((lambda (a b) a) 11 22))

(defparameter *expr3*
  '((lambda (a b) a b) 11 22))

(defparameter *expr4*
  '((lambda (a b) (lambda (c d) c)) 11 22))


(test expr1
  (let ((compiled (compile-form *expr1*)))
    (print compiled)))

(test expr2
  (let ((compiled (compile-form *expr2*)))
    (print compiled)))
