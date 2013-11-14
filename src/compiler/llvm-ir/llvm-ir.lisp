(in-package :owlisp/llvm-ir)

(export '(compile-defun
	  compile-call))



(defun compile-defun (name args evaluated-body-forms)
  (let ((llvm-args (format nil "~{i8* ~a~^, ~}" args)))
    (concatenate 'string
		 (format nil "define void @~a(~a) {~%" name llvm-args)
		 (format nil "entry:~%")
		 (format nil "~a"
			 (reduce (lambda (acc form)
				   (concatenate 'string acc form))
				 evaluated-body-forms))
		 (format nil "~tret~%")
		 (format nil "}~%"))))

(defun compile-call (name args)
  (let ((llvm-args (format nil "~{i8* ~a~^, ~}" args)))
    (format nil "~tcall void ~a(~a)~%" name llvm-args)))
