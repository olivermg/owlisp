(in-package :owlisp/tests)

(def-suite owlisp/tests/environment)

(in-suite owlisp/tests/environment)



(defparameter *key1* :aaa)
(defparameter *key2* :bbb)
(defparameter *key3* :xxx)
(defparameter *key4* :yyy)
(defparameter *keylist1* (list *key3* *key4*))
(defparameter *value1* '(1 2 3))
(defparameter *value2* "mystring")
(defparameter *value3* '(3 1 5))
(defparameter *value4* "superstring")
(defparameter *valuelist1* (list *value3* *value4*))



(defun make-bound-environment ()
  (owlisp::set-in-environment (owlisp::make-environment)
			      *key1*
			      *value1*))

(defun make-bound-environment-and-overwrite-binding ()
  (owlisp::set-in-environment (make-bound-environment)
			      *key1*
			      *value2*))

(defun make-bound-and-derived-environment ()
  (owlisp::make-environment (make-bound-environment)))

(defun make-derived-environment-and-bind-another ()
  (owlisp::set-in-environment (owlisp::make-environment (make-bound-and-derived-environment))
			      *key2*
			      *value2*))

(defun make-derived-environment-and-bind-same ()
  (owlisp::set-in-environment (owlisp::make-environment (make-bound-and-derived-environment))
			      *key1*
			      *value2*))



(test make-environment-returns-not-null
  (is (not (null (owlisp::make-environment)))))

(test can-bind
  "can we create and retrieve a binding?"
  (is (eq (owlisp::find-in-environment (make-bound-environment)
				       *key1*)
	  *value1*)))

(test can-rebind
  "can we redefine a binding?"
  (is (eq (owlisp::find-in-environment (make-bound-environment-and-overwrite-binding)
				       *key1*)
	  *value2*)))

(test can-retrieve-from-parent-environment
  "can we retrieve a binding from a parent frame?"
  (is (eq (owlisp::find-in-environment (make-bound-and-derived-environment)
				       *key1*)
	  *value1*)))

(test can-bind-new-value-in-derived-environment
  "can we create a binding in a derived frame?"
  (is (eq (owlisp::find-in-environment (make-derived-environment-and-bind-another)
				       *key2*)
	  *value2*)))

(test can-retrieve-same-value-from-parent-environment
  "can we retrieve a binding from a parent frame after creating another binding?"
  (is (eq (owlisp::find-in-environment (make-derived-environment-and-bind-another)
				       *key1*)
	  *value1*)))

(test can-bind-and-retrieve-shadowed-value
  "can we shadow a binding in a derived frame?"
  (is (eq (owlisp::find-in-environment (make-derived-environment-and-bind-same)
				       *key1*)
	  *value2*)))

(test shadowing-does-not-destroy-value-in-parent-environment
  "can we shadow a binding and still maintain the original binding in the parent frame?"
  (is (eq (owlisp::find-in-environment (owlisp::parent-environment
					(make-derived-environment-and-bind-same))
				       *key1*)
	  *value1*)))

(test retrieve-unknown-binding-fails
  "does trying to retrieve an unknown binding throw an error condition?"
  (signals error (owlisp::find-in-environment (make-derived-environment-and-bind-another)
					      :unknown)))

(test binding-qualified-variables-works
  "can we bind with higher level functions that respect scope & type?"
  (is (eq (owlisp::lookup-in-environment (owlisp::update-in-environment (make-bound-environment)
									*key2*
									*value2*)
					 *key2*)
	  *value2*)))

(test create-multiple-bindings
  "can we bind multiple bindings at once?"
  (let ((env (owlisp::update-in-environment (make-bound-environment)
					    *keylist1*
					    *valuelist1*)))
    (is (eq (owlisp::lookup-in-environment env *key3*)
	    *value3*))
    (is (eq (owlisp::lookup-in-environment env *key4*)
	    *value4*))))
