(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/environment
  (:use cl owlisp))

(defpackage #:owlisp/register
  (:use cl owlisp))

(defpackage #:owlisp/evaluator
  (:use cl owlisp owlisp/environment owlisp/register)
  (:shadow apply))

(defpackage #:owlisp/llvm-ir
  (:use cl))

(defpackage #:owlisp/parrot
  (:use cl))
