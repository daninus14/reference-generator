(in-package :reference-generator)


(defun get-non-cl-package-symbols (package))

(defun get-package-classes (package))

(defun get-class-related-symbols (class))
;; class slots and their docs
;; class method sepcializers
;; superclasses
;; metaclasses
;; subclasses?


;; examples from tina with tables
;; for package
;; - classes and slots 
;; - functions
;; - generics and methods
;; - variables and parameters
;; - structs
;; - conditions
;;  -


(defmacro with-page ((&key title) &body body)
  `(with-html
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))



(defun section ()
  (with-page (:title "Home page")
    (:header
     (:h1 "Home page"))
    (:section
     ("~A, here is *your* shopping list: " *user-name*)
     (:ol (dolist (item *shopping-list*)
            (:li (1+ (random 10)) item))))
    (:footer ("Last login: ~A" *last-login*))))

(defgeneric documentation-table (elements &optional title)
  (:documentation "Will render a 2 column table where each row is an item of the elements list provided. The first cell of each row is the symbol name and the second is the documentation string truncated at 200 characters. The symbol name is itself a link to a more detailed documentation."))

(defmethod documentation-table (elements &optional title)
  (with-html
    (:section
     title
     (:table (:tbody (loop for el in elements
                           do (doc-row el)))))))

(defgeneric doc-row (el)
  (:documentation "See documentation-table for details."))

(defmethod doc-row (el)
  (princ el))

(defmethod doc-row ((el documentation-element))
  (with-html
    (:tr
     (if (link el)
         (:td (:a :href (link el) (name el)))
         (:td (name el)))
     (:td (description el)))))



