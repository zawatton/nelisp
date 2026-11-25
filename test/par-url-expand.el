;;; -*- lexical-binding: t; -*-
(require 'url-expand)

(defmacro par-url-expand-row (label form)
  `(princ (format "%s %S\n" ,label
                  (condition-case err ,form (error (car err))))))

(par-url-expand-row "Whole syntax-table range"
  (let* ((table (make-syntax-table))
         (entry (aref (standard-syntax-table) ?a))
         (result (set-char-table-range table t entry)))
    (list (equal result entry)
          (equal (aref table ?/) entry)
          (equal (aref table #x10ffff) entry))))
(par-url-expand-row "HTTPS relative path"
  (url-expand-file-name "b" "https://example.com/a/c"))
(par-url-expand-row "HTTPS parent path, query and fragment"
  (url-expand-file-name "../b?q=2#part" "https://example.com:8443/a/c"))
(par-url-expand-row "HTTP root-relative path"
  (url-expand-file-name "/b" "http://example.com/a/c"))
