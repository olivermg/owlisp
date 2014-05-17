(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/environment
  (:use cl owlisp))

(defpackage #:owlisp/machines
  (:use cl owlisp owlisp/environment))

(defpackage #:owlisp/evaluator
  (:use cl owlisp owlisp/environment owlisp/machines)
  (:shadow apply))

(defpackage #:owlisp/llvm-ir
  (:use cl))

(defpackage #:owlisp/parrot
  (:use cl))
