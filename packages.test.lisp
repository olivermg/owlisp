(defpackage #:owlisp/environment/tests
  (:use cl fiveam owlisp owlisp/environment))

(defpackage #:owlisp/machines/tests
  (:use cl fiveam owlisp/machines))

(defpackage #:owlisp/evaluator/tests
  (:use cl fiveam owlisp/evaluator owlisp/environment owlisp/machines))
