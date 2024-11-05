(in-package :reference-generator)

(defun write-index-html (content &optional &key path (if-exists :error))
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
       (with-open-file (file file-path :direction :output :element-type 'character
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
         (write-sequence content file)))
      ((and (uiop:file-exists-p file-path)
            (eq if-exists :ignore))
       nil) ;; Do nothing, return nil
      (t
       ;; Write to index.html since it doesn't exist
       (with-open-file (file file-path :direction :output :element-type 'character
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
         (write-sequence content file))))))
