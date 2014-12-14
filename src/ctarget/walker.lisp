(in-package :owlisp/c)

(export '(do-dump))


(defmacro with-dumper (&body body)
  `(let ((*varname-index* 0)
	 (*procedurename-index* 0)
	 (*buffers* '())
	 (*current-buffer* '())
	 (*header-buffer* (make-string-buffer))
	 (*declared-functions* (make-hash-table :test #'eql)))
     (declare (special *varname-index*
		       *procedurename-index*
		       *buffers*
		       *current-buffer*
		       *header-buffer*
		       *declared-functions*))
;     (dump-header "#include <owlisp/owlisprt.h>~%")
;     (new-buffer)
     (progn ,@body) ; NOTE: we don't dump the result of this into any buffer, because we don't want global scope expressions to be dumped to C, as this is not allowed in C.
     (concatenate 'string
		  *header-buffer*
		  (format nil "~%")
		  (apply #'concatenate
			 'string
			 *buffers*))))

(defun express (formatstr &rest args)
  (apply #'format nil formatstr args))

(defun dump (formatstr &rest args)
  (declare (special *current-buffer*))
  (apply #'format (car *current-buffer*) formatstr args))

(defun dump-header (formatstr &rest args)
  (declare (special *header-buffer*))
  (apply #'format
	 *header-buffer*
	 (format nil "~a~%" formatstr)
	 args))

(defun function-declared-p (fn-name)
  (declare (special *declared-functions*))
  (gethash fn-name *declared-functions*))

(defun add-declared-function (fn-name)
  (declare (special *declared-functions*))
  (setf (gethash fn-name *declared-functions*)
	t))

#|
(defun formal-args (args)
  (format nil "~{int ~a~^, ~}" args))

(defun formal-prototype (args)
  (format nil "~{int~*~^, ~}" args))
|#

(defun new-buffer ()
  (declare (special *buffers* *current-buffer*))
  (let ((newbuf (make-string-buffer)))
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

#|
(defun top-buffer? ()
  (declare (special *current-buffer*))
  (= (length *current-buffer*) 1))
|#


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
	#'function-reference/c-p
	(obj nil)
      (let ((fname (prefix-symbol (function-reference/c-name obj) ; TODO: unify prefix-name creation
				  "_U_")))
	(when (not (function-declared-p fname))
	  (dump-header "Object* ~a( Env* );" fname)
	  (add-declared-function fname))
	(express "newproc( &~a )" fname)))

    (defrule
	#'abstraction/c-p
	(obj nil)
      (new-buffer)
      (dump "Object* ~a( Env* env ) {~%~a}~%~%"
	    (prefix-symbol (abstraction/c-name obj)
			   "_U_")
	    (walk (abstraction/c-body obj)))
      (pop-buffer)
      (express "newproc( &~a )"
	       (prefix-symbol (abstraction/c-name obj)
			      "_U_")))

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
	(express "invoke_obj( ~a, ~a~{, ~a~} );~%" ; TODO: fix for no arguments
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
