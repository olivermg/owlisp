(in-package :owlisp)

;(use-package :cl-ppcre)

(export '())



(defun get-function-from-map (map functionname)
  (lookup-keyvalue-map map functionname))

(defun find-function (designator)
  (let* ((split-designator (cl-ppcre:split
			    "=>"
			    (symbol-name designator))))
    (cond ((> (length split-designator)
	      1)
	   (get-function-from-map (eval (find-symbol (car split-designator)
						     (symbol-package designator)))
				  (eval (find-symbol (cadr split-designator)
						     'keyword))))
	  (t `#',designator))))

(defun call (stream char)
  (declare (ignore char))
  (let* ((delimited-list (read-delimited-list #\] stream t))
	 (fn (car delimited-list))
	 (fn-args (cdr delimited-list)))
    `(apply (find-function ',fn) ',fn-args)))

(set-macro-character #\[ #'call)
(set-macro-character #\] (get-macro-character #\)))
