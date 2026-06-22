;;; nemacs-library-package-catalog.el --- consumer package API catalog -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a consumer-facing package catalog by joining the package guide
;; rows with the package-scoped API inventory.  The catalog is intentionally
;; still advisory: it makes package API review easier without broadening the
;; narrow facade contract.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nemacs-library-package-api)
(require 'nemacs-library-package-guide)

(defvar nemacs-library-package-catalog-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-catalog-output
  (expand-file-name "build/nemacs-library-package-catalog.tsv"
                    nemacs-library-package-catalog-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-catalog-summary-output
  (expand-file-name "build/nemacs-library-package-catalog.org"
                    nemacs-library-package-catalog-repo-root)
  "Org catalog output path.")

(defun nemacs-library-package-catalog--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-catalog--sort-strings (values)
  "Return sorted unique string VALUES."
  (sort (delete-dups
         (mapcar #'nemacs-library-package-catalog--symbol-name
                 (copy-sequence values)))
        #'string<))

(defun nemacs-library-package-catalog--join (values)
  "Return VALUES as a comma-separated stable string."
  (mapconcat #'identity
             (nemacs-library-package-catalog--sort-strings values)
             ","))

(defun nemacs-library-package-catalog--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-catalog--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-catalog--tsv-cell cells "\t"))

(defun nemacs-library-package-catalog--api-by-package (api-rows)
  "Return API-ROWS grouped by package id and class."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row api-rows)
      (let* ((package-id (nth 0 row))
             (class (nth 7 row))
             (symbol (nth 11 row))
             (classes (or (gethash package-id table)
                          (let ((new (make-hash-table :test 'equal)))
                            (puthash package-id new table)
                            new))))
        (puthash class (cons symbol (gethash class classes)) classes)))
    table))

(defun nemacs-library-package-catalog--api-symbols (api-table package-id class)
  "Return symbols for PACKAGE-ID and CLASS from API-TABLE."
  (let ((classes (gethash package-id api-table)))
    (if classes
        (nemacs-library-package-catalog--sort-strings
         (gethash class classes))
      nil)))

(defun nemacs-library-package-catalog--catalog-row (guide-row api-table)
  "Return catalog row for GUIDE-ROW using API-TABLE."
  (let* ((package-id (nth 0 guide-row))
         (public (nemacs-library-package-catalog--api-symbols
                  api-table package-id "public-prefixed"))
         (compat (nemacs-library-package-catalog--api-symbols
                  api-table package-id "compat-global"))
         (private (nemacs-library-package-catalog--api-symbols
                   api-table package-id "private-helper")))
    (list package-id
          (nth 1 guide-row)
          (nth 2 guide-row)
          (nth 3 guide-row)
          (nth 4 guide-row)
          (nth 5 guide-row)
          (nth 6 guide-row)
          (nth 7 guide-row)
          (nth 8 guide-row)
          (length public)
          (length compat)
          (length private)
          public
          compat)))

(defun nemacs-library-package-catalog--rows ()
  "Return consumer package catalog rows."
  (let ((api-table
         (nemacs-library-package-catalog--api-by-package
          (nemacs-library-package-api--rows))))
    (mapcar (lambda (guide-row)
              (nemacs-library-package-catalog--catalog-row
               guide-row api-table))
            (nemacs-library-package-guide--rows))))

(defun nemacs-library-package-catalog--write-tsv (rows output)
  "Write catalog ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-catalog--row
      "package_id" "facade_name" "owner" "loader_feature"
      "package_requires" "member_features" "lazy_features"
      "host_features" "vendor_package_features" "public_prefixed_count"
      "compat_global_count" "private_helper_count"
      "public_prefixed_symbols" "compat_global_symbols")
     "\n")
    (dolist (row rows)
      (insert
       (nemacs-library-package-catalog--row
        (nth 0 row)
        (nth 1 row)
        (nth 2 row)
        (nth 3 row)
        (nemacs-library-package-catalog--join (nth 4 row))
        (nemacs-library-package-catalog--join (nth 5 row))
        (nemacs-library-package-catalog--join (nth 6 row))
        (nemacs-library-package-catalog--join (nth 7 row))
        (nemacs-library-package-catalog--join (nth 8 row))
        (nth 9 row)
        (nth 10 row)
        (nth 11 row)
        (nemacs-library-package-catalog--join (nth 12 row))
        (nemacs-library-package-catalog--join (nth 13 row)))
       "\n"))))

(defun nemacs-library-package-catalog--insert-symbol-list (title symbols)
  "Insert TITLE and SYMBOLS as an Org source block."
  (insert (format "- %s: %d\n" title (length symbols)))
  (when symbols
    (insert "\n#+begin_src text\n")
    (dolist (symbol symbols)
      (insert symbol "\n"))
    (insert "#+end_src\n")))

(defun nemacs-library-package-catalog--write-summary (rows output)
  "Write catalog ROWS to Org OUTPUT."
  (let ((public-total 0)
        (compat-total 0)
        (private-total 0))
    (dolist (row rows)
      (setq public-total (+ public-total (nth 9 row)))
      (setq compat-total (+ compat-total (nth 10 row)))
      (setq private-total (+ private-total (nth 11 row))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library package consumer API catalog\n\n")
      (insert (format "* Summary\n\n- packages: %d\n- public-prefixed symbols: %d\n- compat-global symbols: %d\n- private-helper symbols: %d\n\n"
                      (length rows)
                      public-total
                      compat-total
                      private-total))
      (insert "* Entry point\n\n")
      (insert "External consumers should prefer the top-level facade unless they are deliberately testing a package loader:\n\n")
      (insert "#+begin_src emacs-lisp\n")
      (insert "(add-to-list 'load-path \"/path/to/nelisp-emacs/src\")\n")
      (insert "(require 'nelisp-emacs)\n")
      (insert "#+end_src\n\n")
      (insert "* Packages\n\n")
      (dolist (row rows)
        (insert (format "** %s\n\n" (nth 0 row)))
        (insert (format "- facade-name: =%s=\n" (nth 1 row)))
        (insert (format "- owner: =%s=\n" (nth 2 row)))
        (insert "- package loader:\n\n")
        (insert "#+begin_src emacs-lisp\n")
        (insert (format "(require '%s)\n" (nth 3 row)))
        (insert "#+end_src\n\n")
        (insert (format "- package-requires: =%s=\n"
                        (nemacs-library-package-catalog--join (nth 4 row))))
        (insert (format "- member-features: =%s=\n"
                        (nemacs-library-package-catalog--join (nth 5 row))))
        (insert (format "- lazy-features: =%s=\n"
                        (nemacs-library-package-catalog--join (nth 6 row))))
        (insert (format "- host-features: =%s=\n"
                        (nemacs-library-package-catalog--join (nth 7 row))))
        (insert (format "- vendor-package-features: =%s=\n\n"
                        (nemacs-library-package-catalog--join (nth 8 row))))
        (nemacs-library-package-catalog--insert-symbol-list
         "public-prefixed API candidates" (nth 12 row))
        (insert "\n")
        (nemacs-library-package-catalog--insert-symbol-list
         "compat-global shim names" (nth 13 row))
        (insert (format "\n- private-helper count: %d\n\n"
                        (nth 11 row))))
      (insert "* Notes\n\n")
      (insert "- This catalog is generated from the package guide and package-scoped API inventory.\n")
      (insert "- =public-prefixed= symbols are preferred reusable API candidates, not a stability promise by themselves.\n")
      (insert "- =compat-global= symbols intentionally match Emacs names for compatibility.\n")
      (insert "- Private helpers are counted for extraction review; consumers must not depend on them.\n"))))

;;;###autoload
(defun nemacs-library-package-catalog-batch ()
  "Write consumer package API catalog artifacts."
  (let ((rows (nemacs-library-package-catalog--rows)))
    (unless rows
      (error "empty nelisp-emacs library package catalog"))
    (nemacs-library-package-catalog--write-tsv
     rows nemacs-library-package-catalog-output)
    (nemacs-library-package-catalog--write-summary
     rows nemacs-library-package-catalog-summary-output)
    (princ
     (format
      "nemacs-library-package-catalog: packages=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-catalog-output
      nemacs-library-package-catalog-summary-output))))

(provide 'nemacs-library-package-catalog)

;;; nemacs-library-package-catalog.el ends here
