#|
(defpackage #:owlisp/environment/tests
  (:use cl fiveam owlisp owlisp/environment))

(defpackage #:owlisp/cfg/tests
  (:use cl fiveam owlisp/cfg))

(defpackage #:owlisp/machines/tests
  (:use cl fiveam owlisp/machines))

(defpackage #:owlisp/evaluator/tests
  (:use cl fiveam owlisp/evaluator owlisp/environment owlisp/machines))
|#

(defpackage #:owlisp/c/tests
  (:use cl fiveam owlisp/analyzer owlisp/c))

(defpackage #:owlisp/compiler/tests
  (:use cl fiveam owlisp/compiler))
