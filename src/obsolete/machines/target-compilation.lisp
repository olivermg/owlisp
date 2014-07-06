(in-package :owlisp/machines)

(export '())



#|
(defmacro target-compile (next-byte-fn (&rest args) &body body)
  (let ((param-bytes (gensym)))
    `(let ((,param-bytes ,(read-args next-byte-fn (length args))))
       (apply #'(lambda ,args ,@body) ,param-bytes))))
|#

(defmacro define-target-compilation-set (next-byte-fn &body body)
  (let ((opcode (gensym))
	(nb-fn (gensym)))

    `(let ((,nb-fn ,next-byte-fn))

       (lambda (,opcode)
	 (destructure-define-opcodes ,opcode
				     ,body
				     (($2) (step-instruction ,nb-fn $3 $@4)))))))
