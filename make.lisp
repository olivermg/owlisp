(asdf:load-system :owlisp)

(sb-ext:save-lisp-and-die "/home/oliver/git/owlisp/owlisp-frontend"
			  :toplevel #'owlisp/analyzer:main
			  :executable t)
