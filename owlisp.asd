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
    :depends-on (:cffi
		 :cl-ppcre)
    :components ((:file "packages")
		 (:module
		  src
		  :components
		  ((:file "owlisp")

		   (:module
		    helper
		    :components
		    ((:file "syntax-funcalls"
			    :depends-on ("sequences"))
		     (:file "sequences")
		     (:file "symbols")
		     (:file "message-passing")))

		   (:module
		    environment
		    :components
		    ((:file "environment")
		     (:file "declarative")
		     (:file "binding"))
		    :depends-on ("owlisp"
				 "helper"))

		   (:module
		    evaluator
		    :components
		    ((:file "evaluator"
			    :depends-on ("builtins"))
		     (:file "builtins"))
		    :depends-on ("owlisp"
				 "helper"
				 "environment"
				 "compiler"
				 "interpreter"
				 "machines"))

		   (:module
		    compiler
		    :components
		    ((:file "compiler"
			    :depends-on ("llvm-ir"
					 "parrot"))
		     (:module
		      llvm-ir
		      :components
		      ((:file "globals")
		       (:module
			cffi
			:components
			((:file "loader")
			 (:file "llvmcffi"
				:depends-on ("loader")))
			:depends-on ("globals"))
		       (:file "common"
			      :depends-on ("cffi"))
		       (:module
			typesystem
			:components
			((:file "typesystem"))
			:depends-on ("common"))
		       (:file "llvm-ir"
			      :depends-on ("builtins"
					   "common"))
		       (:file "builtins"
			      :depends-on ("common"))))
		     (:module
		      parrot
		      :components
		      ((:file "instructions"))))
		    :depends-on ("owlisp"
				 "helper"))

		   (:module
		    interpreter
		    :components ((:file "interpreter"))
		    :depends-on ("owlisp"
				 "helper"
				 "environment"
				 "machines"))

		   (:module
		    machines
		    :components ((:file "machines")
				 (:file "register"
					:depends-on ("machines"))
				 (:file "secd"
					:depends-on ("machines")))
		    :depends-on ("helper"
				 "environment")))

		  :depends-on ("packages"))))

(defsystem owlisp-tests
  :name "owlisp-tests"
  :version "0.0.1"
  :maintainer "Oliver Wegner <void1976@gmail.com>"
  :author "Oliver Wegner <void1976@gmail.com>"
  :licence "BSD"
  :description "owlisp-tests"
  :depends-on (:owlisp
	       :fiveam)
  :components ((:file "packages.test")
	       (:module
		tests
		:components
		((:file "environment")
		 (:file "register")
		 (:file "evaluator"))
		:depends-on ("packages.test"))))
