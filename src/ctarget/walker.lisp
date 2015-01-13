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
	obj
      (express "Object* ~a = ~a;~%"
	       (assignment/c-lvalue obj)
	       (walk (assignment/c-value obj))))

    (defrule
	#'set-binding/c-p
	obj
      (let ((frameindex (set-binding/c-frameindex obj))
	    (varindex (set-binding/c-varindex obj))
	    (value (set-binding/c-value obj)))
	(express "set_i( env, ~a, ~a, ~a );~%" frameindex varindex value)))

    (defrule
	#'extend-bindings/c-p
	obj
      (express "env = (Env*)newenv_e( env, ~a );~%" (extend-bindings/c-size obj)))

    (defrule
	#'null/c-p
	obj
      (declare (ignore obj))
      (express "NULL"))

    (defrule
	#'constant-int/c-p
	obj
      (express "newint( ~a )"
	       (constant-int/c-value obj)))

    (defrule
	#'constant-string/c-p
	obj
      (express "newstring( \"~a\" )"
	       (constant-string/c-value obj)))

    (defrule
	#'symbol/c-p
	obj
      (declare (ignore obj))
      (error "not implemented yet"))

    (defrule
	#'if/c-p
	obj
      (let ((lvalue (if/c-lvalue obj)))
	(express "Object* ~a;~%if (~a) {~%~a = ~a;~%} else {~%~a = ~a;~%}~%"
		 lvalue
		 (if/c-cond obj)
		 lvalue
		 (if/c-then obj)
		 lvalue
		 (if/c-else obj))))

    (defrule
	#'reference/c-p
	obj
      (express "lookup_i( env, ~a, ~a )"
	       (reference/c-frameindex obj)
	       (reference/c-varindex obj)))

    (defrule ; TODO: insert abstraction layer here, since we need to be able to (setf (symbol-function fn) ...)
	#'function-reference/c-p
	obj
      (express "lookup_i( fn_env, ~a, ~a )"
	       (function-reference/c-frameindex obj)
	       (function-reference/c-varindex obj)))

    (defrule
	#'abstraction/c-p
	obj
      (new-buffer)
      (dump "Object* ~a( Env* env ) {~%~a}~%~%"
	    (prefix-symbol (abstraction/c-name obj)
			   "_U_")
	    (walk (abstraction/c-body obj)))
      (pop-buffer)
      (express "newclosure_i( env, &~a )"
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
	obj
      (let ((args (application/c-args obj)))
	(express "invoke_obj( ~a, ~a~{, ~a~} )"
		 (application/c-fn obj)
		 (length args)
		 args)))

    (defrule
	#'sequence/c-p
	obj
      (express "~{~a~}"
	       (walk-sequence (sequence/c-sequence obj))))

    (defrule
	#'return/c-p
	obj
      (express "return ~a;~%"
	       (return/c-variable-name obj)))

    (defrule
	#'(lambda (obj)
	    (declare (ignore obj))
	    t)
	obj
      (error "compiler-walker: unknown object ~a" obj))))


(defun do-dump (expr)
  (funcall #'(lambda ()
	       (with-dumper
		 (funcall *dumper-walker*
			  expr)))))
