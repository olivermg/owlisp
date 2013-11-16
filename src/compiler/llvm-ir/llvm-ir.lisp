(in-package :owlisp/llvm-ir)

(export '(compile-defun
	  compile-call))



(defun compile-defun (name args evaluated-body-forms)
  (let ((llvm-args (format nil "~{i8* ~a~^, ~}" args)))
    (concatenate 'string
		 (format nil "define fastcc void @~a(~a) {~%" name llvm-args)
		 (format nil "entry:~%")
		 (format nil "~a"
			 (reduce (lambda (acc form)
				   (concatenate 'string acc form))
				 evaluated-body-forms))
		 (format nil "~tret void~%")
		 (format nil "}~%"))))

(defun duplicate-elements (sequence)
  (reduce
   (lambda (sum e)
     (append sum
	     (list e e)))
   sequence
   :initial-value '()))

(defun compile-call (name args)
  (let ((llvm-pconv (concatenate 'string
				 (format nil "~{~t%p~a = alloca i8~%~}" args)
				 (format nil "~{~tstore i8 ~a, i8* %p~a~%~}" (duplicate-elements args))))
	(llvm-pargs (format nil "~{i8* %p~a~^, ~}" args)))
    (concatenate 'string
		 llvm-pconv
		 (format nil "~tcall fastcc void @~a(~a)~%" name llvm-pargs))))
