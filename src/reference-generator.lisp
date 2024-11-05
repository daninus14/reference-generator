(in-package :reference-generator)


;; (defun get-non-cl-package-symbols (package))

;; (defun get-package-classes (package))

;; (defun get-class-related-symbols (class))
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
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css")
       (:title ,title))
      (:body ,@body))))



;; (defun section ()
;;   (with-page (:title "Home page")
;;     (:header
;;      (:h1 "Home page"))
;;     (:section
;;      ("~A, here is *your* shopping list: " *user-name*)
;;      (:ol (dolist (item *shopping-list*)
;;             (:li (1+ (random 10)) item))))
;;     (:footer ("Last login: ~A" *last-login*))))

(defgeneric documentation-table (elements &optional title)
  (:documentation "Will render a 2 column table where each row is an item of the elements list provided. The first cell of each row is the symbol name and the second is the documentation string truncated at 200 characters. The symbol name is itself a link to a more detailed documentation."))

(defmethod documentation-table (elements &optional title)
  (with-html
    (:div
     :class "cell"
     (:h3 :class "subtitle" title)
     (:div :class "content"
           (:table
            :class "table is-hoverable is-fullwidth is-striped is-size-6"
            (:tbody (loop for el in elements
                          do (doc-row el))))))))

(defun doc-card (el)
  (with-html
    (:div :class "card"
          (:header :class "card-header" (name el))
          (:div :class "card-content"
                (:div :class "content" (description el))))))

(defun doc-card-list (elements &optional title)
  (with-html
    (:div :class "column"
          (:h3 :class "subtitle" title)))  

  (with-html
    (loop for el in elements
          do (:div :class "column"
                   (doc-card el)))))

(defun package-section (package)
  (with-html
    (:div
     :class "grid"
     ;; (org.shirakumo.definitions:designator package)
     (documentation-table (document-unique-classes package) "Classes")
     (documentation-table (document-unique-variables package) "Variables")
     (documentation-table (document-unique-conditions package) "Conditions")
     (documentation-table (document-unique-generic-functions package) "Generic Functions")
     (documentation-table (document-unique-functions package) "Functions"))))

(defun package-section-cards (package)
  (with-html
    (:div
     :class "columns"
     ;; (org.shirakumo.definitions:designator package)
     (doc-card-list (document-unique-classes package) "Classes")
     (doc-card-list (document-unique-variables package) "Variables")
     (doc-card-list (document-unique-conditions package) "Conditions")
     (doc-card-list (document-unique-generic-functions package) "Generic Functions")
     (doc-card-list (document-unique-functions package) "Functions"))))

(defgeneric doc-row (el)
  (:documentation "See documentation-table for details."))

(defmethod doc-row (el)
  (princ el))

(defmethod doc-row ((el documentation-element))
  (with-html
    (:tr
     (if (link el)
         (:td (:p :class "is-size-7" (:a :href (link el) (name el))))
         (:td (:p :class "is-size-7" (name el))))
     (:td (:p :class "is-size-7" (description el))))))

(defgeneric generate-reference (system))

(defmethod generate-reference ((system symbol))
  (generate-reference (asdf:find-system system)))

;; (defmethod generate-reference ((system asdf:system))
;;   )

(defun generate-system-docs-string (system-name)
  (with-output-to-string (out)
    (let ((*html* out)
          (*html-style* :tree))
      (with-page
          (:title (symbol-name system-name))
        (loop for package in (packages-for-system system-name)
              do (package-section package))))))

(defun generate-system-docs-string-cards (system-name)
  (with-output-to-string (out)
    (let ((*html* out)
          (*html-style* :tree))
      (with-page
          (:title (symbol-name system-name))
        (loop for package in (packages-for-system system-name)
              do (package-section-cards package))))))

(defun generate-system-docs (system-name)
  (write-docs-index-html (generate-system-docs-string system-name)
                         :if-exists :supersede))

(defun generate-system-docs-cards (system-name)
  (write-docs-index-html (generate-system-docs-string-cards system-name)
                         :if-exists :supersede))
