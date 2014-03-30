(in-package :owlisp)

(export '(send-message))



(defun send-message (object message &rest args)
  (apply (funcall object message) args))
