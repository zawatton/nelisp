;;; nemacs-library-package-api.el --- package-scoped API inventory -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate package-scoped public API artifacts by joining the draft package
;; descriptors with the generated public API inventory.  This gives external
;; consumers a package-oriented view without making the broader advisory
;; inventory part of the stable facade contract.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nemacs-library-package-descriptors)
(require 'nemacs-public-api-inventory)

(defvar nemacs-library-package-api-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-api-output
  (expand-file-name "build/nemacs-library-package-api.tsv"
                    nemacs-library-package-api-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-api-summary-output
  (expand-file-name "build/nemacs-library-package-api.org"
                    nemacs-library-package-api-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-api--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-api--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-api--tsv-cell cells "\t"))

(defun nemacs-library-package-api--descriptor-file-map (descriptors)
  "Return hash table mapping descriptor source files to package metadata."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (descriptor descriptors)
      (let ((package-id (nth 1 descriptor))
            (facade-name (nth 0 descriptor))
            (owner (nth 2 descriptor))
            (loader (nth 3 descriptor)))
        (dolist (file (nth 5 descriptor))
          (puthash file
                   (list package-id facade-name owner loader "eager")
                   table))
        (dolist (file (nth 8 descriptor))
          (puthash file
                   (list package-id facade-name owner loader "lazy")
                   table))))
    table))

(defun nemacs-library-package-api--rows ()
  "Return package-scoped API inventory rows."
  (let* ((descriptors (nemacs-library-package-descriptors--descriptor-rows))
         (file-map
          (nemacs-library-package-api--descriptor-file-map descriptors))
         rows)
    (dolist (api-row (nemacs-public-api-inventory--rows))
      (let* ((file (nth 4 api-row))
             (package (gethash file file-map)))
        (when package
          (push (append package
                        (list (nth 0 api-row)
                              (nth 1 api-row)
                              (nth 2 api-row)
                              (nth 3 api-row)
                              file
                              (nth 5 api-row)
                              (nth 6 api-row)))
                rows))))
    (sort (nreverse rows)
          (lambda (a b)
            (string<
             (mapconcat (lambda (cell) (format "%s" cell)) a "\t")
             (mapconcat (lambda (cell) (format "%s" cell)) b "\t"))))))

(defun nemacs-library-package-api--inc (table key)
  "Increment TABLE count for KEY."
  (puthash key (1+ (or (gethash key table) 0)) table))

(defun nemacs-library-package-api--sorted-counts (table)
  "Return TABLE counts sorted by key."
  (let (items)
    (maphash (lambda (key value) (push (cons key value) items)) table)
    (sort items (lambda (a b) (string< (car a) (car b))))))

(defun nemacs-library-package-api--write-tsv (rows output)
  "Write package API ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-api--row
      "package_id" "facade_name" "owner" "loader_feature" "role"
      "group" "surface" "class" "kind" "file" "line" "symbol")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-api--row row) "\n"))))

(defun nemacs-library-package-api--write-summary (rows output)
  "Write package API ROWS summary to Org OUTPUT."
  (let ((by-package-class (make-hash-table :test 'equal))
        (by-package-surface (make-hash-table :test 'equal))
        (public 0)
        (compat 0)
        (private 0))
    (dolist (row rows)
      (let ((package (nth 0 row))
            (surface (nth 6 row))
            (class (nth 7 row)))
        (nemacs-library-package-api--inc
         by-package-class (format "%s/%s" package class))
        (nemacs-library-package-api--inc
         by-package-surface (format "%s/%s" package surface))
        (cond
         ((equal class "public-prefixed") (setq public (1+ public)))
         ((equal class "compat-global") (setq compat (1+ compat)))
         ((equal class "private-helper") (setq private (1+ private))))))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library package API inventory\n\n")
      (insert (format "* Summary\n\n- rows: %d\n- public-prefixed: %d\n- compat-global: %d\n- private-helper: %d\n\n"
                      (length rows) public compat private))
      (insert "* Counts by package/class\n\n")
      (insert "| Package/Class | Count |\n|---------------+-------|\n")
      (dolist (item (nemacs-library-package-api--sorted-counts
                     by-package-class))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Counts by package/surface\n\n")
      (insert "| Package/Surface | Count |\n|-----------------+-------|\n")
      (dolist (item (nemacs-library-package-api--sorted-counts
                     by-package-surface))
        (insert (format "| %s | %d |\n" (car item) (cdr item))))
      (insert "\n* Notes\n\n")
      (insert "- This is package-scoped inventory, not a stability promise.\n")
      (insert "- `public-prefixed' rows are preferred candidates for reusable package API.\n")
      (insert "- `compat-global' rows are Emacs-compatible shim surface.\n")
      (insert "- `private-helper' rows are included so extraction reviewers can see private surface size; consumers must not depend on them.\n")
      (insert "- `role' is `eager' for package loader members and `lazy' for package-owned lazy companions.\n"))))

;;;###autoload
(defun nemacs-library-package-api-batch ()
  "Write package-scoped API inventory artifacts."
  (let ((rows (nemacs-library-package-api--rows)))
    (unless rows
      (error "empty nelisp-emacs package API inventory"))
    (nemacs-library-package-api--write-tsv
     rows nemacs-library-package-api-output)
    (nemacs-library-package-api--write-summary
     rows nemacs-library-package-api-summary-output)
    (princ
     (format
      "nemacs-library-package-api: rows=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-api-output
      nemacs-library-package-api-summary-output))))

(provide 'nemacs-library-package-api)

;;; nemacs-library-package-api.el ends here
