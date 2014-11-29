(in-package :owlisp/c)

(export '(do-dump))


(defmacro with-dumper (&body body)
  `(let* ((*varname-index* 0)
	  (*procedurename-index* 0)
	  (*buffers* '())
	  (*current-buffer* '()))
     (declare (special *varname-index* *procedurename-index* *buffers* *current-buffer*))
     (new-buffer)
     (dump "int main() {~%~areturn 0;~%}~%~%"
	   (express "~a" (progn ,@body)))
     (apply #'concatenate
	    'string
	    *buffers*)))

(defun express (formatstr &rest args)
  (apply #'format nil formatstr args))

(defun dump (formatstr &rest args)
  (declare (special *current-buffer*))
  (apply #'format (car *current-buffer*) formatstr args))

(defun formal-args (args)
  (format nil "~{int ~a~^, ~}" args))

(defun formal-prototype (args)
  (format nil "~{int~*~^, ~}" args))

(defun new-buffer ()
  (declare (special *buffers* *current-buffer*))
  (let ((newbuf (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (setf *buffers*
	  (cons newbuf
		*buffers*))
    (setf *current-buffer*
	  (cons newbuf
		*current-buffer*))))

(defun pop-buffer ()
  (declare (special *buffers* *current-buffer*))
  (setf *current-buffer*
	(cdr *current-buffer*)))


(defun next-varname ()
  (declare (special *varname-index*))
  (intern
   (format nil "VAR~d" (incf *varname-index*))))

(defun next-procedurename ()
  (declare (special *procedurename-index*))
  (intern
   (format nil "PROC~d" (incf *procedurename-index*))))


(defparameter *dumper-walker*

  (make-walker
    (declare (ignore #'walk-sequence-last))

    (defrule
	#'assignment/c-p
	(obj nil)
      (express "Object* ~a = ~a;~%"
	       (assignment/c-lvalue obj)
	       (walk (assignment/c-value obj))))

    (defrule
	#'constant/c-p
	(obj nil)
      (express "newint( ~a )"
	       (constant/c-value obj)))

    (defrule
	#'symbol/c-p
	(obj nil)
      (declare (ignore obj))
      (error "not implemented yet"))

    (defrule
	#'reference/c-p
	(obj nil)
      (express "lookup_i( env, ~a, ~a )"
	       (reference/c-frameindex obj)
	       (reference/c-varindex obj)))

    (defrule
	#'abstraction/c-p
	(obj nil)
      (new-buffer)
      (dump "Object* ~a( Env* env ) {~%~a}~%~%"
	    (abstraction/c-name obj)
	    (walk (abstraction/c-body obj)))
      (pop-buffer)
      (express "newproc( &~a )"
	       (abstraction/c-name obj)))

    #|
    (defrule
	#'closure*-p
	(obj nil)
      (new-buffer)
      (let ((procname (next-procedurename))
	    (walked-body (walk-sequence (abstraction*-body obj))))
	(pop-buffer)
	`(dump "int ~a(~a) {~%~a}~%"
	       ,procname
	       ,(abstraction*-args obj)
	       ,walked-body)))	; TODO: auto-return last value
    |#


    (defrule
	#'application/c-p
	(obj nil)
      (let ((args (application/c-args obj)))
	(express "invoke_obj( ~a, ~a, ~{~a~^, ~} );~%" ; TODO: fix for no arguments
		 (application/c-fn obj)
		 (length args)
		 args)))

    (defrule
	#'sequence/c-p
	(obj nil)
      (express "~{~a~}"
	       (walk-sequence (sequence/c-sequence obj))))

    (defrule
	#'return/c-p
	(obj nil)
      (express "return ~a;~%"
	       (return/c-variable-name obj)))

    (defrule
	#'(lambda (obj)
	    (declare (ignore obj))
	    t)
	(obj nil)
      (error "compiler-walker: unknown object ~a" obj))))


(defun do-dump (expr)
  (funcall #'(lambda ()
	       (with-dumper
		 (funcall *dumper-walker*
			  expr)))))
