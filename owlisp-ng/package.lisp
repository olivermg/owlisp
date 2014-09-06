;;;; package.lisp

(defpackage #:owlisp/dumper
  (:use #:cl)
  (:nicknames #:owl/d))

(defpackage #:owlisp
  (:use #:cl #:owl/d)
  (:nicknames #:owl))
