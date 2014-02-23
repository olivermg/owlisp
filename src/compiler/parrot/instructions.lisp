(in-package :owlisp/parrot)

(export '(load-libraries
	  initialize
	  compile-defpackage
	  compile-defun
	  compile-call
	  write-compilation
	  define-default-package
	  define-+))



(defun load-libraries ()
  (print "load-libraries"))

(defun initialize ()
  (print "initialize"))

(defun compile-defpackage (name)
  (format t "defpackage -~a-" name))

(defun compile-defun (name args body)
  (format t "defun -~a ~a ~a-" name args body))

(defun compile-call (name args)
  (format t "call -~a ~a-" name args))

(defun write-compilation ()
  (print "write-compilation"))

(defun define-default-package ()
  (print "define-default-package"))

(defun define-+ ()
  (print "define-+"))
