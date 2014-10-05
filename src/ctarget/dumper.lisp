(in-package #:owlisp/c)

(export '(with-dumper
	  dump-reference
	  dump-constant
	  dump-fndefinition-start
	  dump-fndefinition-end
	  dump-fndefinition
	  dump-application))


(defmacro with-dumper (&body body)
  `(let* ((*varname-index* 0)
	  (*procedurename-index* 0)
	  (*buffers* '())
	  (*current-buffer* '()))
     (declare (special *varname-index* *procedurename-index* *buffers* *current-buffer*))
     (new-buffer)
     (dump-fndefinition-start "main")
     ,@body
     (dump-fndefinition-end (dump-constant 0))
     (apply #'concatenate
	    'string
	    *buffers*)))


(defun dump (formatstr &rest args)
  (declare (special *current-buffer*))
  (apply #'format (car *current-buffer*) formatstr args))

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


(defun dump-reference (frameindex varindex)
  (let ((varname (next-varname)))
    (dump "int ~a = lookup( ~a, ~a );~%" varname frameindex varindex)
    varname))

(defun dump-constant (value)
  (let ((varname (next-varname)))
    (dump "int ~a = ~a;~%" varname value)
    varname))

(defun dump-fndefinition-start (args &optional (procname (next-procedurename)))
  (new-buffer)
  (dump "int ~a() {~%" procname)
  procname)

(defun dump-fndefinition-end (result)
  (dump "return ~a;~%}~%" result)
  (pop-buffer)
  nil)

(defun dump-fndefinition (body &optional (procname (next-procedurename)))
  (new-buffer)
  (dump "int ~a() {~%~a}~%" procname body)
  (pop-buffer)
  procname)

(defun dump-application (fnname args)
  (let ((resultname (next-varname)))
    (dump "int ~a = invoke( \"~a\", ~{~a~^,~} );~%" resultname fnname args)
    resultname))

(defun dump-environment (env)
  )

(defun dump-closure-definition (procname args env)
  )
