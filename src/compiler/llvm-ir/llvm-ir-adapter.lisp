(in-package :owlisp/llvm-ir)

(export '(i32))



(defun symbol-starts-with (symbol prefix)
  (string-equal (subseq (symbol-name symbol)
			0
			(length prefix))
		prefix))

(defun unprefixed-symbol (symbol prefix)
  (intern
   (string-upcase
    (subseq
     (symbol-name symbol)
     (length prefix)))
   (symbol-package symbol)))

(defun upcase-expr (expr)
  (cond ((consp expr) (mapcar #'upcase-expr
			      expr))
	((symbolp expr) (intern (string-upcase (symbol-name expr))
				(symbol-package expr)))
	(t expr)))

(defun read-llvm-expr (stream char)
  (declare (ignore char))
  (let ((prev-case (readtable-case *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (let* ((expr (read-delimited-list #\] stream))
	   (result (loop
		      for e in expr
		      collect (cond
				((consp e)
				 (cons "~A" (upcase-expr e)))
				((symbol-starts-with e ".")
				 (cons "~A" (unprefixed-symbol e ".")))
				(t
				 (cons (symbol-name e) nil))))))
      (setf (readtable-case *readtable*) prev-case)
      `(format nil
	       ,(format nil "~{~a~^ ~}"
			(mapcar #'(lambda (e)
				    (car e))
				result))
	       ,@(remove-if #'null
			    (mapcar #'(lambda (e)
					(cdr e))
				    result))))))

(set-macro-character
 #\[
 #'read-llvm-expr)

(set-macro-character
 #\]
 (get-macro-character #\)))


;; native types

(defun i32 ()
  [i32])



#|
;; functions

(defmacro define (return-type name &rest parameter-types)
  [.return-type .name .parameter-types])
|#
