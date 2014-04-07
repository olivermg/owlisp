(defpackage #:owlisp/environment/tests
  (:use cl fiveam owlisp owlisp/environment))

(defpackage #:owlisp/register/tests
  (:use cl fiveam owlisp/register))

(defpackage #:owlisp/evaluator/tests
  (:use cl fiveam owlisp/evaluator owlisp/environment owlisp/register))
