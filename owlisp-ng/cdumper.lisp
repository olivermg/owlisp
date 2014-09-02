(in-package #:owlisp)


(defparameter *cdumper-stream* *standard-output*)


(defun dump (text)
  (format *cdumper-stream* text))
