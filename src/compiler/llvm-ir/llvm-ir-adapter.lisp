(in-package :owlisp/llvm-ir)

(export '(i32))



(defun read-llvm-expr (stream char)
  (declare (ignore char))
  (let ((prev-case (readtable-case *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (let* ((expr (read-delimited-list #\] stream))
	   (result (loop
		      for e in expr
		      collect (if (string-equal (subseq (symbol-name e) 0 1) ".")
				  (cons "~A"
					(intern (string-upcase (subseq (symbol-name e) 1))))
				  (cons (symbol-name e) nil)))))
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


#|
;; native types

(defun i32 ()
  [i32])



;; functions

(defmacro define (return-type name &rest parameter-types)
  [.return-type .name .parameter-types])
|#
