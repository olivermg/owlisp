(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/environment
  (:use cl owlisp))

(defpackage #:owlisp/cfg
  (:use cl))

(defpackage #:owlisp/llvm-ir
  (:use cl))

(defpackage #:owlisp/runtime
  (:use cl owlisp/llvm-ir))

(defpackage #:owlisp/machines
  (:use cl owlisp owlisp/environment))

(defpackage #:owlisp/evaluator
  (:use cl owlisp owlisp/environment owlisp/cfg owlisp/machines owlisp/runtime)
  (:shadow apply))

(defpackage #:owlisp/parrot
  (:use cl))
