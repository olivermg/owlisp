(in-package #:owlisp/dumper)

(export '(with-dumper
	  dump-assignment
	  dump-fndefinition))


(defmacro with-dumper (&body body)
  `(let* ((*varname-index* 0)
	  (*procedurename-index* 0)
	  (*buffers* '())
	  (*current-buffer* '()))
     (declare (special *varname-index* *procedurename-index* *buffers* *current-buffer*))
     (new-buffer)
     ,@body
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


(defun dump-assignment (value)
  (let ((varname (next-varname)))
    (dump "int ~a = ~a;~%" varname value)
    varname))

(defun dump-fndefinition (body)
  (let ((procname (next-procedurename)))
    (new-buffer)
    (dump "int ~a() {~%}~%" procname)
    (pop-buffer)
    procname))
