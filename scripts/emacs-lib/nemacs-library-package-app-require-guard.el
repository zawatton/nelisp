;;; nemacs-library-package-app-require-guard.el --- guard package/app requires -*- lexical-binding: t; -*-

;;; Commentary:

;; Verify that reusable package scaffold files do not `require' features
;; provided by the app/frontend scaffold.  This complements the descriptor
;; dependency table by checking the generated physical scaffold itself.

;;; Code:

(require 'cl-lib)

(defvar nemacs-library-package-app-require-guard-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-app-require-guard-package-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-app-require-guard-repo-root)
  "Reusable package scaffold index.")

(defvar nemacs-library-package-app-require-guard-app-scaffold
  (expand-file-name "build/nemacs-library-app-scaffold.tsv"
                    nemacs-library-package-app-require-guard-repo-root)
  "App/frontend scaffold index.")

(defvar nemacs-library-package-app-require-guard-output
  (expand-file-name "build/nemacs-library-package-app-require-guard.tsv"
                    nemacs-library-package-app-require-guard-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-app-require-guard-summary-output
  (expand-file-name "build/nemacs-library-package-app-require-guard.org"
                    nemacs-library-package-app-require-guard-repo-root)
  "Org summary output path.")

(defun nemacs-library-package-app-require-guard--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-app-require-guard--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-app-require-guard--tsv-cell cells "\t"))

(defun nemacs-library-package-app-require-guard--scaffold-rows (index)
  "Return file rows from scaffold INDEX."
  (let (rows)
    (unless (file-readable-p index)
      (error "missing scaffold index: %s" index))
    (with-temp-buffer
      (insert-file-contents index)
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (let ((fields (split-string
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
                       "\t")))
          (when (equal (nth 0 fields) "file")
            (push fields rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun nemacs-library-package-app-require-guard--absolute (relative)
  "Return repository absolute path for RELATIVE."
  (expand-file-name relative
                    nemacs-library-package-app-require-guard-repo-root))

(defun nemacs-library-package-app-require-guard--read-forms (file)
  "Return top-level forms read from FILE."
  (let (forms)
    (unless (file-readable-p file)
      (error "missing readable Elisp file: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (nreverse forms)))

(defun nemacs-library-package-app-require-guard--quoted-symbol (form)
  "Return quoted symbol represented by FORM, or nil."
  (and (consp form)
       (eq (car form) 'quote)
       (symbolp (cadr form))
       (cadr form)))

(defun nemacs-library-package-app-require-guard--form-provide (form)
  "Return provided feature in FORM, or nil."
  (and (consp form)
       (eq (car form) 'provide)
       (nemacs-library-package-app-require-guard--quoted-symbol
        (cadr form))))

(defun nemacs-library-package-app-require-guard--collect-requires
    (form scope)
  "Return quoted require entries in FORM with SCOPE."
  (let (entries)
    (cond
     ((and (consp form) (eq (car form) 'quote))
      nil)
     ((and (consp form) (eq (car form) 'require))
      (let ((feature
             (nemacs-library-package-app-require-guard--quoted-symbol
              (cadr form))))
        (when feature
          (push (cons feature scope) entries))))
     ((consp form)
      (while (consp form)
        (setq entries
              (append
               entries
               (nemacs-library-package-app-require-guard--collect-requires
                (car form) scope)))
        (setq form (cdr form)))
      (when form
        (setq entries
              (append
               entries
               (nemacs-library-package-app-require-guard--collect-requires
                form scope))))))
    entries))

(defun nemacs-library-package-app-require-guard--requires-in-file (file)
  "Return required feature entries found in FILE."
  (let (entries)
    (dolist (form (nemacs-library-package-app-require-guard--read-forms file))
      (setq entries
            (append
             entries
             (if (and (consp form) (eq (car form) 'require))
                 (nemacs-library-package-app-require-guard--collect-requires
                  form "top-level")
               (nemacs-library-package-app-require-guard--collect-requires
                form "lazy")))))
    (sort (delete-dups entries)
          (lambda (a b)
            (string< (format "%s\t%s" (car a) (cdr a))
                     (format "%s\t%s" (car b) (cdr b)))))))

(defun nemacs-library-package-app-require-guard--app-provide-map
    (app-rows)
  "Return feature -> app scaffold row hash table from APP-ROWS."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (row app-rows)
      (let ((target (nemacs-library-package-app-require-guard--absolute
                     (nth 4 row))))
        (dolist (form
                 (nemacs-library-package-app-require-guard--read-forms
                  target))
          (let ((feature
                 (nemacs-library-package-app-require-guard--form-provide
                  form)))
            (when (and feature (not (gethash feature table)))
              (puthash feature row table))))))
    table))

(defun nemacs-library-package-app-require-guard--violation-rows ()
  "Return reusable package -> app/frontend require violation rows."
  (let* ((package-rows
          (nemacs-library-package-app-require-guard--scaffold-rows
           nemacs-library-package-app-require-guard-package-scaffold))
         (app-rows
          (nemacs-library-package-app-require-guard--scaffold-rows
           nemacs-library-package-app-require-guard-app-scaffold))
         (app-providers
          (nemacs-library-package-app-require-guard--app-provide-map
           app-rows))
         violations)
    (dolist (package-row package-rows)
      (let ((package-target
             (nemacs-library-package-app-require-guard--absolute
              (nth 4 package-row))))
        (dolist (required
                 (nemacs-library-package-app-require-guard--requires-in-file
                  package-target))
          (let ((app-row (gethash (car required) app-providers)))
            (when app-row
              (push
               (list (nth 1 package-row)
                     (nth 2 package-row)
                     (nth 3 package-row)
                     (nth 4 package-row)
                     (car required)
                     (cdr required)
                     (nth 2 app-row)
                     (nth 3 app-row)
                     (nth 4 app-row))
               violations))))))
    (sort (nreverse violations)
          (lambda (a b)
            (string< (mapconcat (lambda (cell) (format "%s" cell)) a "\t")
                     (mapconcat (lambda (cell) (format "%s" cell)) b "\t"))))))

(defun nemacs-library-package-app-require-guard--scan-counts ()
  "Return plist of scan counts for package/app scaffold."
  (let* ((package-rows
          (nemacs-library-package-app-require-guard--scaffold-rows
           nemacs-library-package-app-require-guard-package-scaffold))
         (app-rows
          (nemacs-library-package-app-require-guard--scaffold-rows
           nemacs-library-package-app-require-guard-app-scaffold))
         (app-providers
          (nemacs-library-package-app-require-guard--app-provide-map
           app-rows))
         (requires 0)
         (features 0))
    (maphash (lambda (_feature _row) (setq features (1+ features)))
             app-providers)
    (dolist (package-row package-rows)
      (setq requires
            (+ requires
               (length
                (nemacs-library-package-app-require-guard--requires-in-file
                 (nemacs-library-package-app-require-guard--absolute
                  (nth 4 package-row)))))))
    (list :package-files (length package-rows)
          :app-files (length app-rows)
          :package-requires requires
          :app-features features)))

(defun nemacs-library-package-app-require-guard--write-tsv (rows)
  "Write violation ROWS to `nemacs-library-package-app-require-guard-output'."
  (make-directory
   (file-name-directory nemacs-library-package-app-require-guard-output) t)
  (with-temp-file nemacs-library-package-app-require-guard-output
    (insert
     (nemacs-library-package-app-require-guard--row
      "package_id" "package_role" "package_source" "package_target"
      "required_feature" "require_scope" "app_role" "app_source"
      "app_target")
     "\n")
    (dolist (row rows)
      (insert
       (apply #'nemacs-library-package-app-require-guard--row row)
       "\n"))))

(defun nemacs-library-package-app-require-guard--write-summary
    (rows counts)
  "Write violation ROWS and scan COUNTS to summary output."
  (make-directory
   (file-name-directory
    nemacs-library-package-app-require-guard-summary-output)
   t)
  (with-temp-file nemacs-library-package-app-require-guard-summary-output
    (insert "#+TITLE: nemacs library package app require guard\n\n")
    (insert "* Summary\n\n")
    (insert (format "- package files checked: %d\n"
                    (plist-get counts :package-files)))
    (insert (format "- app files indexed: %d\n"
                    (plist-get counts :app-files)))
    (insert (format "- package require edges scanned: %d\n"
                    (plist-get counts :package-requires)))
    (insert (format "- app features indexed: %d\n"
                    (plist-get counts :app-features)))
    (insert (format "- violations: %d\n\n" (length rows)))
    (insert "* Violations\n\n")
    (insert "| Package | Source | Required feature | Scope | App source |\n")
    (insert "|---------+--------+------------------+-------+------------|\n")
    (if rows
        (dolist (row rows)
          (insert
           (format "| =%s= | =%s= | =%s= | =%s= | =%s= |\n"
                   (nth 0 row)
                   (nth 2 row)
                   (nth 4 row)
                   (nth 5 row)
                   (nth 7 row))))
      (insert "| none | none | none | none | none |\n"))
    (insert "\n* Notes\n\n")
    (insert "- Reusable package scaffold files must not require APP/bootstrap/frontend features.\n")
    (insert "- Tests that need APP/frontend glue should opt into the app scaffold load path explicitly.\n")
    (insert "- This guard scans the generated package/app scaffold files, not only source ownership metadata.\n")))

;;;###autoload
(defun nemacs-library-package-app-require-guard-batch ()
  "Verify package scaffold files do not require app/frontend features."
  (let* ((rows
          (nemacs-library-package-app-require-guard--violation-rows))
         (counts
          (nemacs-library-package-app-require-guard--scan-counts)))
    (nemacs-library-package-app-require-guard--write-tsv rows)
    (nemacs-library-package-app-require-guard--write-summary rows counts)
    (princ
     (format
      "nemacs-library-package-app-require-guard: package-files=%d app-features=%d violations=%d output=%s summary=%s\n"
      (plist-get counts :package-files)
      (plist-get counts :app-features)
      (length rows)
      nemacs-library-package-app-require-guard-output
      nemacs-library-package-app-require-guard-summary-output))
    (when rows
      (kill-emacs 1))))

(provide 'nemacs-library-package-app-require-guard)

;;; nemacs-library-package-app-require-guard.el ends here
