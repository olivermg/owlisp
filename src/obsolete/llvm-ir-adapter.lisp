(in-package :owlisp/llvm-ir)

(export '())



(defun make-llvm-code-generator ()
  (let ((target-code '())
	(register-index 0))

    (macrolet ((emit (format-str &rest values)
		 `(setf target-code
			(append target-code
				(list (format nil
					      ,(concatenate 'string format-str "~%")
					      ,@values))))))

     (labels ((register-assign (type value)
		(emit "  %~a = ~a ~a" (incf register-index) type value)
		register-index)

	      (constant (value)
		))

       (lambda (action &rest args)
	 (case action
	   ((:constant (apply #'constant args)))))))))


;; native types

(defun i32 ()
  "i32")

(defun i64 ()
  "i64")


;; registers

(defun constant (value)
  )


;; functions

(defun define (return-type name &rest parameter-types)
  (format nil "define ~a @~a(~{~a~^, ~})"
	  return-type name parameter-types))
