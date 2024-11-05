(in-package :reference-generator)

;;; Symbols for a Package not in :cl

(defun functions-in-package (package)
  "Return a list of function symbols in PACKAGE excluding those in the :cl package."
  (let ((function-symbols ()))
    (do-symbols (sym package)
      (when (and (fboundp sym)  ; Check if symbol has a function binding
                 (not (find-symbol (symbol-name sym) :cl))) ; Check if symbol is not in :cl
        (push sym function-symbols)))
    function-symbols))

(defun class-symbols-in-package (package)
  (let ((class-symbols ()))
    (do-symbols (sym package)
      (when (and (find-class sym nil) ; Check if symbol names a class
                 (not (find-symbol (symbol-name sym) :cl))) ; Check if symbol is not in :cl
        (push sym class-symbols)))
    class-symbols))

(defun variable-symbols-in-package (package)
  (let ((variable-symbols ()))
    (do-symbols (sym package)
      (when (and (boundp sym)    ; Check if symbol has a value
                 (not (functionp sym)) ; Ensure it's not a function
                 (not (find-symbol (symbol-name sym) :cl))) ; Check if symbol is not in :cl
        (push sym variable-symbols)))
    variable-symbols))

(defun condition-symbols-in-package (package)
  (let ((condition-symbols ()))
    (do-symbols (sym package)
      (let ((class (find-class sym nil)))
        (when (and class
                   (subtypep class 'condition) ; Check if class is a subtype of `condition`
                   (not (find-symbol (symbol-name sym) :cl))) ; Check if symbol is not in :cl
          (push sym condition-symbols))))
    condition-symbols))

(defun generic-function-p (fn)
  "Check if FN is a generic function."
  (typep fn 'standard-generic-function))

(defun generic-function-symbols-in-package (package)
  (let ((generic-function-symbols ()))
    (do-symbols (sym package)
      (when (and (fboundp sym)   ; Check if symbol has a function binding
                 (generic-function-p (symbol-function sym)) ; Check if it's a generic function
                 (not (find-symbol (symbol-name sym) :cl))) ; Check if symbol is not in :cl
        (push sym generic-function-symbols)))
    generic-function-symbols))


;;; Symbols Unique to a Package - not imported or used from another package

(defun unique-symbol-p (symbol package)
  "Check if SYMBOL is unique to PACKAGE, i.e., defined in PACKAGE and not imported from other packages."
  (eq (symbol-package symbol) package)
  ;; (and (eq (symbol-package symbol) package) ; Check if the symbol is defined in the package
  ;;      (not (find-symbol (symbol-name symbol) (package-use-list package))))
  ) ; Ensure it is not imported

(defun unique-functions-in-package (package)
  "Return a list of unique function symbols in PACKAGE, excluding symbols imported from other packages."
  (let ((unique-function-symbols ()))
    (do-symbols (sym package)
      (when (and (fboundp sym)  ; Check if symbol has a function binding
                 (unique-symbol-p sym package)) ; Use the helper function
        (push sym unique-function-symbols)))
    unique-function-symbols))

(defun unique-class-symbols-in-package (package)
  "Return a list of unique class symbols in PACKAGE, excluding symbols imported from other packages."
  (let ((unique-class-symbols ()))
    (do-symbols (sym package)
      (when (and (find-class sym nil) ; Check if symbol names a class
                 (unique-symbol-p sym package)) ; Use the helper function
        (push sym unique-class-symbols)))
    unique-class-symbols))

(defun unique-variable-symbols-in-package (package)
  "Return a list of unique variable symbols in PACKAGE, excluding symbols imported from other packages."
  (let ((unique-variable-symbols ()))
    (do-symbols (sym package)
      (when (and (boundp sym)    ; Check if symbol has a value
                 (not (functionp sym)) ; Ensure it's not a function
                 (unique-symbol-p sym package)) ; Use the helper function
        (push sym unique-variable-symbols)))
    unique-variable-symbols))

(defun unique-condition-symbols-in-package (package)
  "Return a list of unique condition symbols in PACKAGE, excluding symbols imported from other packages."
  (let ((unique-condition-symbols ()))
    (do-symbols (sym package)
      (let ((class (find-class sym nil)))
        (when (and class
                   (subtypep class 'condition) ; Check if class is a subtype of `condition`
                   (unique-symbol-p sym package)) ; Use the helper function
          (push sym unique-condition-symbols))))
    unique-condition-symbols))

(defun unique-generic-function-symbols-in-package (package)
  "Return a list of unique generic function symbols in PACKAGE, excluding symbols imported from other packages."
  (let ((unique-generic-function-symbols ()))
    (do-symbols (sym package)
      (when (and (fboundp sym)   ; Check if symbol has a function binding
                 (generic-function-p (symbol-function sym)) ; Check if it's a generic function
                 (unique-symbol-p sym package)) ; Use the helper function
        (push sym unique-generic-function-symbols)))
    unique-generic-function-symbols))

