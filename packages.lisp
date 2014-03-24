(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/evaluator
  (:use cl owlisp)
  (:shadow apply))

(defpackage #:owlisp/register
  (:use cl))

(defpackage #:owlisp/llvm-ir
  (:use cl))

(defpackage #:owlisp/parrot
  (:use cl))
