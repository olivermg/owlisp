(defpackage #:owlisp
  (:use cl))

(defpackage #:owlisp/environment
  (:use cl owlisp))

(defpackage #:owlisp/llvm
  (:use cl))

(defpackage #:owlisp/helper
  (:use cl))

(defpackage #:owlisp/builtin/macros
  (:nicknames owl/m))

(defpackage #:owlisp/analyzer
  (:use cl owlisp owlisp/environment owlisp/llvm owlisp/helper)
  (:shadow apply compile))

(defpackage #:owlisp/c
  (:use cl owlisp/helper owlisp/analyzer))

(defpackage #:owlisp/compiler
  (:use cl owlisp owlisp/helper owlisp/analyzer owlisp/c))
