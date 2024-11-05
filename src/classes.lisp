(in-package :reference-generator)

(defclass documentation-element ()
  ((name
    :initarg :name
    :initform NIL
    :accessor name)
   (description
    :initarg :description
    :initform NIL
    :accessor description)
   (link
    :initarg :link
    :initform NIL
    :accessor link)))
