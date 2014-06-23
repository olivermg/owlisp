(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/environment
  (:use cl owlisp))

(defpackage #:owlisp/llvm
  (:use cl))

(defpackage #:owlisp/analyzer
  (:use cl owlisp owlisp/environment owlisp/llvm)
  (:shadow apply))
