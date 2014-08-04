(in-package :owlisp/environment)

(export '())


(defclass activation-record ()
  ((static-link :initarg :static-link)
   (dynamic-link :initarg :dynamic-link)
   (local-vars :initarg :local-vars)))

(defgeneric lookup (address))

(defmethod lookup ((address activation-record))
  )
