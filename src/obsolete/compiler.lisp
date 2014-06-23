(in-package :owlisp)

(export '(;load-libraries
	  ;initialize
	  ;evaluate-defpackage
	  ;evaluate-defun
	  ;evaluate-call
	  ;write-compilation
	  ))


#|
(defun load-libraries ()
  (owlisp/parrot:load-libraries))

(defun initialize ()
  (owlisp/parrot:initialize))

(defun evaluate-defpackage (name)
  (owlisp/parrot:compile-defpackage name))

(defun evaluate-defun (name args body)
  (owlisp/parrot:compile-defun name args body))

(defun evaluate-call (name args)
  (owlisp/parrot:compile-call name args))

(defun write-compilation ()
  (owlisp/parrot:write-compilation))
|#
