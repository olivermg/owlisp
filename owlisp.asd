;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage #:owlisp-asd
  (:use :cl :asdf))

(in-package :owlisp-asd)

(defsystem owlisp
  :name "owlisp"
  :version "0.0.1"
  :maintainer "Oliver Wegner <void1976@gmail.com>"
  :author "Oliver Wegner <void1976@gmail.com"
  :licence "BSD"
  :description "owlisp"
  :depends-on ()
  :components ((:file "packages")
	       (:module src
			:components ((:file "compiler"))
			:depends-on ("packages"))))
