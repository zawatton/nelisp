;;; nemacs-library-package-manifest.el --- export facade package manifest -*- lexical-binding: t; -*-

;;; Commentary:

;; Writes machine-readable and review-friendly artifacts from the public
;; `nelisp-emacs' package manifest API.  The output is generated from the
;; same facade external consumers use, so it stays aligned with the library
;; contract instead of duplicating package membership by hand.

;;; Code:

(require 'nelisp-emacs)

(defvar nemacs-library-package-manifest-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-manifest-output
  (expand-file-name "build/nemacs-library-package-manifest.tsv"
                    nemacs-library-package-manifest-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-manifest-summary-output
  (expand-file-name "build/nemacs-library-package-manifest-summary.org"
                    nemacs-library-package-manifest-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-manifest--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-manifest--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-manifest--tsv-cell cells "\t"))

(defun nemacs-library-package-manifest--feature-list (features)
  "Return FEATURES formatted as a comma-separated string."
  (mapconcat #'symbol-name features ","))

(defun nemacs-library-package-manifest--rows ()
  "Return facade package manifest rows."
  (mapcar
   (lambda (entry)
     (let ((name (car entry))
           (plist (cdr entry)))
       (list name
             (plist-get plist :owner)
             (plist-get plist :feature)
             (plist-get plist :features)
             (plist-get plist :lazy-features))))
   (nelisp-emacs-library-package-manifest)))

(defun nemacs-library-package-manifest--write-tsv (rows output)
  "Write ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-manifest--row
      "name" "owner" "feature" "member_features" "lazy_features")
     "\n")
    (dolist (row rows)
      (insert
       (nemacs-library-package-manifest--row
        (nth 0 row)
        (nth 1 row)
        (nth 2 row)
        (nemacs-library-package-manifest--feature-list (nth 3 row))
        (nemacs-library-package-manifest--feature-list (nth 4 row)))
       "\n"))))

(defun nemacs-library-package-manifest--write-summary (rows output)
  "Write ROWS summary to OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert "#+TITLE: nemacs library package manifest summary\n\n")
    (insert (format "* Contract\n\n- version: %d\n- packages: %d\n\n"
                    nelisp-emacs-library-contract-version
                    (length rows)))
    (insert "* Package groups\n\n")
    (insert "| Name | Owner | Feature | Members | Lazy |\n")
    (insert "|------+-------+---------+---------+------|\n")
    (dolist (row rows)
      (insert
       (format "| %s | %s | %s | %s | %s |\n"
               (nth 0 row)
               (nth 1 row)
               (nth 2 row)
               (nemacs-library-package-manifest--feature-list
                (nth 3 row))
               (nemacs-library-package-manifest--feature-list
                (nth 4 row)))))
    (insert "\n* Notes\n\n")
    (insert "- Generated from `nelisp-emacs-library-package-manifest'.\n")
    (insert "- Consumers should prefer facade query APIs over this artifact at runtime.\n")
    (insert "- `lazy_features' are package-owned companions, not facade eager loads.\n")
    (insert "- This artifact is for review, packaging, and extraction planning.\n")))

;;;###autoload
(defun nemacs-library-package-manifest-batch ()
  "Write library package manifest artifacts."
  (let ((rows (nemacs-library-package-manifest--rows)))
    (unless rows
      (error "empty nelisp-emacs library package manifest"))
    (nemacs-library-package-manifest--write-tsv
     rows nemacs-library-package-manifest-output)
    (nemacs-library-package-manifest--write-summary
     rows nemacs-library-package-manifest-summary-output)
    (princ
     (format
      "nemacs-library-package-manifest: packages=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-manifest-output
      nemacs-library-package-manifest-summary-output))))

(provide 'nemacs-library-package-manifest)

;;; nemacs-library-package-manifest.el ends here
