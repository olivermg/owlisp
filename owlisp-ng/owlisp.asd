;;;; owlisp.asd

(asdf:defsystem #:owlisp
  :serial t
  :description "Describe owlisp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
	       (:file "cdumper"
		      :depends-on ("package"))
	       (:file "environment"
		      :depends-on ("package"))
               (:file "compiler"
		      :depends-on ("package"))))

