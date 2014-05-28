(in-package :owlisp)

;(use-package :cl-ppcre)

(export '())



(defmacro call-map-function-macro (map action &rest args)
  (let ((action-keyword (find-symbol (symbol-name action) 'keyword)))
    `(funcall (lookup-keyvalue-map ,map ,action-keyword)
	      ,@args)))

(defun call-map-function (stream char)
  (declare (ignore char))
  (let* ((delimited-list (read-delimited-list #\] stream t))
	 (map (car delimited-list))
	 (fn (cadr delimited-list))
	 (fn-args (cddr delimited-list)))
    `(call-map-function-macro ,map ,fn ,@fn-args)))

;(set-macro-character #\[ #'call-map-function)
;(set-macro-character #\] (get-macro-character #\)))

