(in-package :owlisp/machines)

(export '())



(defun make-default-machine-secd ()

  (let ((stack '())
	(env '())
	(code '())
	(dump '()))

    (define-opcode-set

	#'(lambda () 1)
	#'(lambda () 1)

      (define-opcode NIL #x10 ()
		     (setf stack
			   (cons nil stack)))

      (define-opcode LDC #x11 (value)
		     (setf stack
			   (cons value stack)))

      (define-opcode LD #x12 (address.frame address.var)
		     (send-message env :lookup
				   (cons address.frame address.var))))

    (lambda (action)
      (case action
	(:run #'(lambda ()))))))
