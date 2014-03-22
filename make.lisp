(asdf:load-system :owlisp)

(sb-ext:save-lisp-and-die "build/owlispc"
			  :toplevel #'owlisp/evaluator:owlisp-toplevel
			  :executable t)
