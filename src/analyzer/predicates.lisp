(in-package :owlisp)

(export '(compiletime-macro-p
	  primitive-p
	  null-p
	  constant-int-p
	  constant-string-p
;	  self-evaluating-p
	  reference-p
	  quote-p
	  let-p
	  setf-p
	  symbol-function-p
	  lambda-p
	  defun-p
	  defmacro-p
	  funcall-p
	  function-p
	  if-p
	  application-p
	  true-p))


(defun symbol-name-equals (symbol name)
  (string-equal (symbol-name symbol)
		(symbol-name name)))

(defun is-tagged-list (expr tag)
  (if (consp expr)
      (let ((head (first expr)))
	(if (symbolp head)
	    (symbol-name-equals head tag)))))

#|
(defun primitive-procedure-p (expr)
  (is-tagged-list expr :primitive-procedure))
|#

(defun compiletime-macro-p (expr)
  (macro-function expr))

(defun primitive-p (expr)
  (let ((primitives '(owlisp/environment:lookup cl:funcall cl:car cl:cdr cl:print cl:+ cl:- cl:* cl:/ cl:format cl:apply)))
    (if (position expr primitives)
	t
	nil)))

#|
(defun self-evaluating-p (expr)
  (or
   (numberp expr)
   (stringp expr)
   (null expr)))
|#

(defun null-p (expr)
  (null expr))

(defun constant-int-p (expr)
  (numberp expr))

(defun constant-string-p (expr)
  (stringp expr))

(defun reference-p (expr)
  (symbolp expr))

(defun quote-p (expr)
  (is-tagged-list expr :quote))

(defun let-p (expr)
  (is-tagged-list expr :let))

(defun setf-p (expr)
  (is-tagged-list expr :setf))

(defun symbol-function-p (expr)
  (is-tagged-list expr :symbol-function))

(defun lambda-p (expr)
  (is-tagged-list expr :lambda))

(defun defun-p (expr)
  (is-tagged-list expr :defun))

(defun defmacro-p (expr)
  (is-tagged-list expr :defmacro))

(defun funcall-p (expr)
  (is-tagged-list expr :funcall))

(defun function-p (expr)
  (is-tagged-list expr :function))

(defun if-p (expr)
  (is-tagged-list expr :if))

(defun application-p (expr)
  (consp expr))

(defun true-p (expr)
  (declare (ignore expr))
  t)
