(in-package :owlisp/llvm-ir/builtins)

(cl:export '(define-+))



(cl:defun define-+ ()
  (cl:concatenate 'cl:string
		  (cl:format cl:nil "define fastcc i8* @add(i8* %ap, i8* %bp) {~%")
		  (cl:format cl:nil "entry:~%")
		  (cl:format cl:nil "~t%a = load i8* %ap~%")
		  (cl:format cl:nil "~t%b = load i8* %bp~%")
		  (cl:format cl:nil "~t%sum = add i8 %a, %b~%")
		  (cl:format cl:nil "~t%sump = alloca i8~%")
		  (cl:format cl:nil "~tstore i8 %sum, i8* %sump~%")
		  (cl:format cl:nil "~tret i8 %sump~%")
		  (cl:format cl:nil "}~%")))
