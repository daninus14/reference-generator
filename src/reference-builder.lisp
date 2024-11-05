(in-package :reference-generator)

(defun make-documentation-element (symbol type)
  "Create a documentation-element instance for SYMBOL with TYPE description."
  (make-instance 'documentation-element
                 :name symbol
                 :description (or (documentation symbol type) "No documentation available.")))

(defun document-class-symbols (package)
  "Return a list of documentation-element instances for class symbols in PACKAGE."
  (mapcar (lambda (sym) (make-documentation-element sym 'type))
          (class-symbols-in-package package)))

(defun document-variable-symbols (package)
  "Return a list of documentation-element instances for variable symbols in PACKAGE."
  (mapcar (lambda (sym) (make-documentation-element sym 'variable))
          (variable-symbols-in-package package)))

(defun document-condition-symbols (package)
  "Return a list of documentation-element instances for condition symbols in PACKAGE."
  (mapcar (lambda (sym) (make-documentation-element sym 'type))
          (condition-symbols-in-package package)))

(defun document-generic-function-symbols (package)
  "Return a list of documentation-element instances for generic function symbols in PACKAGE."
  (mapcar (lambda (sym) (make-documentation-element sym 'function))
          (generic-function-symbols-in-package package)))

;;; unique package symbols

(defun document-unique-functions (package)
  "Document unique function symbols in PACKAGE."
  (let ((unique-functions (unique-functions-in-package package)))
    (mapcar (lambda (func-symbol)
              (make-instance 'documentation-element
                             :name (symbol-name func-symbol)
                             :description "Unique function symbol."))
            unique-functions)))

(defun document-unique-classes (package)
  "Document unique class symbols in PACKAGE."
  (let ((unique-classes (unique-class-symbols-in-package package)))
    (mapcar (lambda (class-symbol)
              (make-instance 'documentation-element
                             :name (symbol-name class-symbol)
                             :description "Unique class symbol."))
            unique-classes)))

(defun document-unique-variables (package)
  "Document unique variable symbols in PACKAGE."
  (let ((unique-variables (unique-variable-symbols-in-package package)))
    (mapcar (lambda (var-symbol)
              (make-instance 'documentation-element
                             :name (symbol-name var-symbol)
                             :description "Unique variable symbol."))
            unique-variables)))

(defun document-unique-conditions (package)
  "Document unique condition symbols in PACKAGE."
  (let ((unique-conditions (unique-condition-symbols-in-package package)))
    (mapcar (lambda (condition-symbol)
              (make-instance 'documentation-element
                             :name (symbol-name condition-symbol)
                             :description "Unique condition symbol."))
            unique-conditions)))

(defun document-unique-generic-functions (package)
  "Document unique generic function symbols in PACKAGE."
  (let ((unique-generic-functions (unique-generic-function-symbols-in-package package)))
    (mapcar (lambda (generic-func-symbol)
              (make-instance 'documentation-element
                             :name (symbol-name generic-func-symbol)
                             :description "Unique generic function symbol."))
            unique-generic-functions)))

(defun ensure-package (package)
  "Return the package associated with PACKAGE, which can be either a package or a keyword.
If PACKAGE is a keyword, find the corresponding package. Raise an error if the package is not found."
  (cond
    ((packagep package) package) ; If package is already a package, return it.
    ((keywordp package) 
     (let ((pkg (find-package package))) ; Directly use find-package on the keyword
       (if pkg
           pkg
           (error "Package ~A not found." package))))
    (t (error "Input must be a package or a keyword."))))

(defun print-unique-symbols-documentation (package)
  "Print the documentation of unique symbols for the given PACKAGE or KEYWORD."
  (let ((pkg (ensure-package package))) ; Get the package using ensure-package
    (let* ((class-docs (document-unique-classes pkg))
           (var-docs (document-unique-variables pkg))
           (condition-docs (document-unique-conditions pkg))
           (generic-func-docs (document-unique-generic-functions pkg))
           (func-docs (document-unique-functions pkg)))
      (format t "Classes: ~a~%" class-docs)
      (format t "Variables: ~a~%" var-docs)
      (format t "Conditions: ~a~%" condition-docs)
      (format t "Generic Functions: ~a~%" generic-func-docs)
      (format t "Functions: ~a~%" func-docs))))

;; Example usage:
(print-unique-symbols-documentation :reference-generator) ; Using a keyword
(print-unique-symbols-documentation :reference-generator) ; Using a package directly


