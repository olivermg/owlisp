(in-package #:owlisp)


(defmacro with-dumper (&body body)
  `(labels ((new-buf ()
	      (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
     (let* ((*varname-index* 0)
	    (*procedurename-index* 0)
	    (*dumper-buffers* (list (new-buf)))
	    (*current-buffer* (car *dumper-buffers*)))
       (declare (special *varname-index* *procedurename-index* *dumper-buffers* *current-buffer*))
       ,@body
       (apply #'concatenate
	      'string
	      *dumper-buffers*))))


(defun dump (formatstr &rest args)
  (declare (special *dumper-buffer*))
  (setf *dumper-buffer*
	(concatenate 'string
		     *dumper-buffer*
		     (apply #'format nil formatstr args))))

(defun dump-global (formatstr &rest args)
  (declare (special *dumper-buffer*))
  (setf *dumper-buffer*
	(concatenate 'string
		     (apply #'format nil formatstr args)
		     *dumper-buffer*)))


(defun ow.next-varname ()
  (intern
   (format nil "VAR~d" (incf *varname-index*))))

(defun ow.next-procedurename ()
  (intern
   (format nil "PROC~d" (incf *procedurename-index*))))


(defun dump-assignment (value)
  (let ((varname (ow.next-varname)))
    (dump "int ~a = ~a;~%" varname value)
    varname))

(defun dump-fndefinition (body)
  (let ((procname (ow.next-procedurename)))
    (dump-global "int ~a() {~%}~%" procname)
    procname))
