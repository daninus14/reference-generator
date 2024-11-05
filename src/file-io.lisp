(in-package :reference-generator)

(defun write-docs-index-html (content &optional &key path (if-exists :error))
  "Write CONTENT to index.html in the specified PATH.
If no PATH is provided, it defaults to (uiop:getcwd)/docs.
If index.html already exists, control its behavior with IF-EXISTS:
- :error (default) signals an error.
- :supersede overwrites the existing file.
- :ignore returns nil."
  (let* ((docs-dir (merge-pathnames "docs/" (uiop:getcwd)))
         (default-file-path (merge-pathnames "index.html" docs-dir))
         (file-path (if path path default-file-path)))
    
    ;; Check if index.html exists
    (cond
      ((and (uiop:file-exists-p file-path)
            (eq if-exists :error))
       (error "File index.html already exists at ~A." file-path))
      ((and (uiop:file-exists-p file-path)
            (eq if-exists :supersede))
       ;; Overwrite the existing file
       (str:to-file file-path content)
       ;; (with-open-file (file file-path :direction :output
       ;;                                 :if-does-not-exist :create
       ;;                                 :if-exists :supersede)
       ;;   (write-sequence content file))
       )
      ((and (uiop:file-exists-p file-path)
            (eq if-exists :ignore))
       nil) ;; Do nothing, return nil
      (t
       ;; Write to index.html since it doesn't exist
       (uiop:ensure-pathname file-path :ensure-directories-exist t)
       (str:to-file file-path content)
       ;; (with-open-file (file file-path :direction :output
       ;;                                 :if-exists :supersede
       ;;                                 :if-does-not-exist :create)
       ;;   (write-sequence content file))
       ))))
