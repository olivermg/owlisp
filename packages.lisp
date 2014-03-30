(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/environment
  (:use cl owlisp))

(defpackage #:owlisp/evaluator
  (:use cl owlisp owlisp/environment)
  (:shadow apply))

(defpackage #:owlisp/register
  (:use cl owlisp))

(defpackage #:owlisp/llvm-ir
  (:use cl))

(defpackage #:owlisp/parrot
  (:use cl))
