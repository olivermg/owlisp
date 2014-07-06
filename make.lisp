(asdf:load-system :owlisp)

(sb-ext:save-lisp-and-die "build/owlisp-frontend"
			  :toplevel #'owlisp/analyzer:toplevel
			  :executable t)
