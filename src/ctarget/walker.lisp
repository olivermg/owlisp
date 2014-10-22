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

    (declare (ignore))

    (let ((*previous-var* ""))
      (declare (special *previous-var*))

      (defrule
	  #'constant*-p
	  (obj nil)
	(let ((varname (next-varname)))
	  (setf *previous-var* varname)
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
	  (setf *previous-var* varname)
	  (express "int ~a = lookup(\"~a\");~%"
		   varname
		   (reference*-symbol obj))))

      (defrule
	  #'abstraction*-p
	  (obj nil)
	(new-buffer)
	(let* ((procname (next-procedurename))
	       (procname-ptr (format nil "~a_P" procname))
	       (walked-body-list (walk-sequence (abstraction*-body obj)))
	       (walked-body (apply #'concatenate
				   'string
				   walked-body-list)))
	  (dump "int ~a(~a) {~%~areturn ~a;~%}~%~%"
		procname
		(formal-args (abstraction*-args obj))
		walked-body
		*previous-var*)
	  (pop-buffer)
	  (setf *previous-var* procname-ptr)
	  (express "int (*~a)(~a) = &~a;~%"
		   procname-ptr
		   (formal-prototype (abstraction*-args obj))
		   procname)))

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

      (defrule
	  #'application*-p
	  (obj nil)
	(let* ((resultname (next-varname))
	       (walked-fn (express (walk (application*-fn obj))))
	       (walked-fn-var *previous-var*))
	  (setf *previous-var* resultname)
	  (express "~aint ~a = ~a(~{~a~^, ~});~%"
		   walked-fn
		   resultname
		   walked-fn-var
		   (application*-args obj))))

      (defrule
	  #'(lambda (obj)
	      (declare (ignore obj))
	      t)
	  (obj nil)
	(error "dumper-walker: unknown object ~a" obj)))))


(defun do-dump (expr)
  (funcall #'(lambda ()
	       (with-dumper
		 (funcall *dumper-walker*
			  expr)))))
