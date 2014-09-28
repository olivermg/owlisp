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
		 :apply-argv)

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
		     (:file "message-passing")
		     (:file "tree")
		     (:file "gensyms")
		     (:file "defun")
		     (:file "codewalker"
			    :depends-on ("gensyms"))))

		   (:module
		    environment
		    :components
		    ((:file "environment")
		     (:file "declarative")
		     (:file "binding"))
		    :depends-on ("owlisp"
				 "helper"))

		   (:module
		    environment-ng
		    :components
		    ((:file "compiletime")))

		   (:module
		    analyzer
		    :components
		    ((:file "predicates")
		     (:file "objects")
		     (:file "analyzer"
			    :depends-on ("predicates"
					 "cps"))
		     (:file "interpreter"
			    :depends-on ("analyzer"))
		     (:file "macroexpand-all"
			    :depends-on ("predicates"))
		     (:file "transform"
			    :depends-on ("predicates" "objects"))
		     (:file "cps"
			    :depends-on ("predicates" "objects"))
		     (:file "environmentalize"
			    :depends-on ("predicates" "objects"))
		     (:file "closure"
			    :depends-on ("predicates" "objects"))
		     (:file "parameters"
			    :depends-on ("predicates")))
		    :depends-on ("owlisp"
				 "helper"
				 "environment"
				 "llvm"))

		   (:module
		    llvm
		    :components
		    ((:file "llvm"
			    :depends-on ("globals"
					 "runtime"
					 "cffi"))
		     (:file "runtime"
			    :depends-on ("globals"))
		     (:file "globals")

		     (:module
		      cffi
		      :components
		      ((:file "llvm_api_high"
			      :depends-on ("llvm_api_low"))
		       (:file "llvm_api_low"
			      :depends-on ("loader"))
		       (:file "loader"))))

		    :depends-on ("owlisp"
				 "helper"))

		   (:module
		    ctarget
		    :components
		    ((:file "globals")
		     (:file "constant"
			    :depends-on ("globals"))
		     (:file "abstraction"
			    :depends-on ("globals")))))

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
