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

(defun interleave-lists (l1 l2)
  (labels ((interleave-lists-sub (li l1 l2)
	     (let ((e1 (car l1))
		   (r1 (cdr l1)))
	       (if e1
		   (interleave-lists-sub (append li (list e1))
					 l2
					 r1)
		   li))))
    (interleave-lists-sub '() l1 l2)))

(defun compile-call/i (name args)
  (let* ((context (LLVMGetGlobalContext))
	 (module (LLVMModuleCreateWIthNameInContext context))
	 (builder (LLVMCreateBuilderInContext context)))))

(defun compile-call (name args)
  (let* ((pointer-args (symbols->llvm-pointer-symbols args))
	 (llvm-pconv (concatenate 'string
				  (format nil "~{~t~a = alloca i8~%~}"
					  pointer-args)
				  (format nil "~{~tstore i8 ~a, i8* ~a~%~}"
					  (interleave-lists args pointer-args)))))
    (concatenate 'string
		 llvm-pconv
		 (format nil "~tcall fastcc void @~a(~a)~%" name (calling-args->llvm args)))))
