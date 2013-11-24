(in-package :owlisp/llvm-ir)

(export '())



(defun args->llvmargs (args)
    (labels ((args->llvmargs/r (llvmargs args)
	       args))))



(defun stringify-atom (atom)
  (format nil "~a" atom))

(defun symbols->llvm-pointer-symbols (symbols)
  (mapcar (lambda (symbol)
	    (intern (format nil "%~a_P" (stringify-atom symbol))))
	  symbols))

(defun calling-args->llvm (calling-args)
  (format nil "~{i8* ~a~^, ~}" calling-args))
