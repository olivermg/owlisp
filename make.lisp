(asdf:load-system :owlisp)

(sb-ext:save-lisp-and-die "build/owlisp"
			  :toplevel #'owlisp/analyzer:toplevel
			  :executable t)
