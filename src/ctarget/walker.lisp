(in-package :owlisp/c)

(export '(do-dump))


(defmacro with-dumper (&body body)
  `(let* ((*varname-index* 0)
	  (*procedurename-index* 0)
	  (*buffers* '())
	  (*current-buffer* '()))
     (declare (special *varname-index* *procedurename-index* *buffers* *current-buffer*))
     (new-buffer)
     (dump "int main() {~%~a}~%~%" (express "~a" (progn ,@body)))
     (apply #'concatenate
	    'string
	    *buffers*)))

(defun express (formatstr &rest args)
  (apply #'format nil formatstr args))

(defun dump (formatstr &rest args)
  (declare (special *current-buffer*))
  (apply #'format (car *current-buffer*) formatstr args))

(defun dump-formal-args (args)
  (format nil "~{int ~a~^, ~}" args))

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

    (declare (ignore))

    (defrule
	#'constant*-p
	(obj nil)
      (let ((varname (next-varname)))
	(express "int ~a = ~a;~%"
		 varname
		 (constant*-value obj))))

    (defrule
	#'symbol*-p
	(obj nil)
      (declare (ignore obj))
      (error "not implemented yet"))

    (defrule
	#'reference*-p
	(obj nil)
      (let ((varname (next-varname)))
	(express "int ~a = lookup( ~a );~%"
		 varname
		 (reference*-symbol obj))))

    (defrule
	#'abstraction*-p
	(obj nil)
      (new-buffer)
      (let ((procname (next-procedurename))
	    (walked-body (apply #'concatenate
				'string
				(walk-sequence (abstraction*-body obj)))))
	(dump "int ~a(~a) {~%~a}~%~%"
	      procname
	      (dump-formal-args (abstraction*-args obj))
	      walked-body)
	(pop-buffer)
	(express "int (~a_ref*) = ~a;~%" procname procname)))

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
	       ,walked-body)))		; TODO: auto-return last value

    (defrule
	#'application*-p
	(obj nil)
      (let ((resultname (next-varname)))
	`(dump "int ~a = invoke( \"~a\", ~{~a~^,~} );~%"
	       ,resultname
	       ,(application*-fn obj)
	       ,(application*-args obj))))

    (defrule
	#'(lambda (obj)
	    (declare (ignore obj))
	    t)
	(obj nil)
      (error "dumper-walker: unknown object ~a" obj))))


(defun do-dump (expr)
  (funcall #'(lambda ()
	       (with-dumper
		 (funcall *dumper-walker*
			  expr)))))
