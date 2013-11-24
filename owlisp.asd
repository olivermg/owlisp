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
  :depends-on (:cffi)
  :components ((:file "packages")
	       (:module
		src
		:components
		((:file "owlisp")

		 (:module
		  helper
		  :components
		  ((:file "sequences")))

		 (:module
		  evaluator
		  :components
		  ((:file "evaluator"
			  :depends-on ("builtins"))
		   (:file "builtins"))
		  :depends-on ("owlisp"
			       "helper"
			       "compiler"
			       "interpreter"))

		 (:module
		  compiler
		  :components
		  ((:file "compiler"
			  :depends-on ("llvm-ir"))
		   (:module
		    llvm-ir
		    :components
		    ((:module
		      cffi
		      :components
		      ((:file "loader")))
		     (:file "common"
			    :depends-on ("cffi"))
		     (:file "llvm-ir"
			    :depends-on ("builtins"
					 "common"))
		     (:file "builtins"
			    :depends-on ("common")))))
		  :depends-on ("owlisp"
			       "helper"))

		 (:module
		  interpreter
		  :components ((:file "interpreter"))
		  :depends-on ("owlisp"
			       "helper")))

		:depends-on ("packages"))))
