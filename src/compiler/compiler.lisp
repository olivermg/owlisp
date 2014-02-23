(in-package :owlisp)

(export '(load-libraries
	  initialize
	  compile-defpackage
	  compile-defun
	  compile-call
	  write-compilation))



(defun load-libraries ()
  (owlisp/parrot:load-libraries))

(defun initialize ()
  (owlisp/parrot:initialize))

(defun compile-defpackage (name)
  (owlisp/parrot:compile-defpackage name))

(defun compile-defun (name args body)
  (owlisp/parrot:compile-defun name args body))

(defun compile-call (name args)
  (owlisp/parrot:compile-call name args))

(defun write-compilation ()
  (owlisp/parrot:write-compilation))
