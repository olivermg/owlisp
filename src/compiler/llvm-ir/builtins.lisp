(cl:in-package :owlisp/llvm-ir/builtins)

(cl:export '(define-+
	     define-print))



(cl:defun define-+ ()
  (cl:concatenate 'cl:string
		  (cl:format cl:nil "define fastcc i8* @ADD(i8* %ap, i8* %bp) {~%")
		  (cl:format cl:nil "entry:~%")
		  (cl:format cl:nil "~t%a = load i8* %ap~%")
		  (cl:format cl:nil "~t%b = load i8* %bp~%")
		  (cl:format cl:nil "~t%sum = add i8 %a, %b~%")
		  (cl:format cl:nil "~t%sump = alloca i8~%")
		  (cl:format cl:nil "~tstore i8 %sum, i8* %sump~%")
		  (cl:format cl:nil "~tret i8* %sump~%")
		  (cl:format cl:nil "}~%")))

(cl:defun define-print ()
  (cl:concatenate 'cl:string
		  (cl:format cl:nil "define fastcc i8* @PRINT(i8* %msgp) {~%")
		  (cl:format cl:nil "}~%")))
