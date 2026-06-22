;;; nemacs-library-package-scaffold.el --- generate experimental packages scaffold -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate an experimental packages/ scaffold from the package layout plan.
;; This copies the current src/ files into packages/PACKAGE/lisp or
;; packages/PACKAGE/lazy and writes per-package README files.  It does not
;; delete or move src/; src/ remains the authoritative development load path.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nemacs-library-package-guide)
(require 'nemacs-library-package-layout)

(defvar nemacs-library-package-scaffold-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-package-scaffold-output
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    nemacs-library-package-scaffold-repo-root)
  "TSV output path.")

(defvar nemacs-library-package-scaffold-summary-output
  (expand-file-name "build/nemacs-library-package-scaffold.org"
                    nemacs-library-package-scaffold-repo-root)
  "Org summary output path.")

(defconst nemacs-library-package-scaffold--facade-source
  "src/nelisp-emacs.el"
  "Source file for the generated umbrella facade scaffold.")

(defconst nemacs-library-package-scaffold--facade-target
  "packages/nelisp-emacs-facade/lisp/nelisp-emacs.el"
  "Target file for the generated umbrella facade scaffold.")

(defconst nemacs-library-package-scaffold--facade-readme
  "packages/nelisp-emacs-facade/README.org"
  "README target for the generated umbrella facade scaffold.")

(defun nemacs-library-package-scaffold--symbol-name (value)
  "Return VALUE as a stable string."
  (if (symbolp value) (symbol-name value) (format "%s" value)))

(defun nemacs-library-package-scaffold--join (values)
  "Return VALUES as a comma-separated stable string."
  (mapconcat #'identity
             (sort (delete-dups
                    (mapcar #'nemacs-library-package-scaffold--symbol-name
                            (copy-sequence values)))
                   #'string<)
             ","))

(defun nemacs-library-package-scaffold--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-package-scaffold--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-package-scaffold--tsv-cell cells "\t"))

(defun nemacs-library-package-scaffold--target-path (relative)
  "Return absolute target path for RELATIVE."
  (expand-file-name relative nemacs-library-package-scaffold-repo-root))

(defun nemacs-library-package-scaffold--copy-layout-row (row)
  "Copy one layout ROW and return a scaffold report row."
  (let* ((package-id (nth 0 row))
         (role (nth 4 row))
         (source-relative (nth 5 row))
         (target-relative (nth 6 row))
         (source (expand-file-name source-relative
                                   nemacs-library-package-scaffold-repo-root))
         (target (nemacs-library-package-scaffold--target-path
                  target-relative)))
    (unless (file-exists-p source)
      (error "missing scaffold source: %s" source-relative))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (list "file" package-id role source-relative target-relative)))

(defun nemacs-library-package-scaffold--readme-path (package-id)
  "Return README path for PACKAGE-ID."
  (format "packages/%s/README.org" package-id))

(defun nemacs-library-package-scaffold--write-readme (guide-row)
  "Write package README for GUIDE-ROW and return a report row."
  (let* ((package-id (nth 0 guide-row))
         (facade-name (nth 1 guide-row))
         (owner (nth 2 guide-row))
         (loader (nth 3 guide-row))
         (requires (nth 4 guide-row))
         (members (nth 5 guide-row))
         (lazy (nth 6 guide-row))
         (host (nth 7 guide-row))
         (vendor (nth 8 guide-row))
         (eager-count (nth 9 guide-row))
         (lazy-count (nth 10 guide-row))
         (readme-relative
          (nemacs-library-package-scaffold--readme-path package-id))
         (readme (nemacs-library-package-scaffold--target-path
                  readme-relative)))
    (make-directory (file-name-directory readme) t)
    (with-temp-file readme
      (insert (format "#+TITLE: %s package scaffold\n\n" package-id))
      (insert "* Status\n\n")
      (insert "Generated experimental scaffold.  Do not edit this copy as the source of truth; edit =src/= and regenerate.\n\n")
      (insert "* Load\n\n")
      (insert "#+begin_src emacs-lisp\n")
      (insert (format "(require '%s)\n" loader))
      (insert "#+end_src\n\n")
      (insert "* Metadata\n\n")
      (insert (format "- facade-name: =%s=\n" facade-name))
      (insert (format "- owner: =%s=\n" owner))
      (insert (format "- package-requires: =%s=\n"
                      (nemacs-library-package-scaffold--join requires)))
      (insert (format "- member-features: =%s=\n"
                      (nemacs-library-package-scaffold--join members)))
      (insert (format "- lazy-features: =%s=\n"
                      (nemacs-library-package-scaffold--join lazy)))
      (insert (format "- host-features: =%s=\n"
                      (nemacs-library-package-scaffold--join host)))
      (insert (format "- vendor-package-features: =%s=\n"
                      (nemacs-library-package-scaffold--join vendor)))
      (insert (format "- source-counts: eager=%d lazy=%d\n"
                      eager-count lazy-count))
      (insert "\n* Smoke\n\n")
      (insert "Run the package-path smoke target from the repository root:\n\n")
      (insert "#+begin_src sh\n")
      (insert "make nemacs-library-package-path-smoke\n")
      (insert "#+end_src\n\n")
      (insert "* API Policy\n\n")
      (insert "Prefer stable prefixed APIs from the facade contract.  Compatibility globals and builtins helpers remain governed by Doc 26.\n"))
    (list "readme" package-id "metadata" "" readme-relative)))

(defun nemacs-library-package-scaffold--copy-facade ()
  "Copy the umbrella facade into the scaffold and return a report row."
  (let ((source (expand-file-name nemacs-library-package-scaffold--facade-source
                                  nemacs-library-package-scaffold-repo-root))
        (target (nemacs-library-package-scaffold--target-path
                 nemacs-library-package-scaffold--facade-target)))
    (unless (file-exists-p source)
      (error "missing scaffold facade source: %s"
             nemacs-library-package-scaffold--facade-source))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (list "facade" "nelisp-emacs-facade" "umbrella"
          nemacs-library-package-scaffold--facade-source
          nemacs-library-package-scaffold--facade-target)))

(defun nemacs-library-package-scaffold--write-facade-readme ()
  "Write README for the umbrella facade scaffold and return a report row."
  (let ((readme (nemacs-library-package-scaffold--target-path
                 nemacs-library-package-scaffold--facade-readme)))
    (make-directory (file-name-directory readme) t)
    (with-temp-file readme
      (insert "#+TITLE: nelisp-emacs facade package scaffold\n\n")
      (insert "* Status\n\n")
      (insert "Generated experimental umbrella facade.  Do not edit this copy as the source of truth; edit =src/nelisp-emacs.el= and regenerate.\n\n")
      (insert "* Load\n\n")
      (insert "#+begin_src emacs-lisp\n")
      (insert "(require 'nelisp-emacs)\n")
      (insert "#+end_src\n\n")
      (insert "* Smoke\n\n")
      (insert "Run the package-path consumer smoke target from the repository root:\n\n")
      (insert "#+begin_src sh\n")
      (insert "make nemacs-library-package-consumer-smoke\n")
      (insert "#+end_src\n\n")
      (insert "* Scope\n\n")
      (insert "This facade depends on the eight generated reusable package groups and remains outside the reusable package count in Doc 27.\n"))
    (list "readme" "nelisp-emacs-facade" "metadata" ""
          nemacs-library-package-scaffold--facade-readme)))

(defun nemacs-library-package-scaffold--write-tsv (rows output)
  "Write scaffold ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-package-scaffold--row
      "kind" "package_id" "role" "source_file" "target_file")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-package-scaffold--row row) "\n"))))

(defun nemacs-library-package-scaffold--write-summary (rows output)
  "Write scaffold ROWS summary to Org OUTPUT."
  (let ((file-count 0)
        (facade-count 0)
        (readme-count 0)
        (by-package (make-hash-table :test 'equal)))
    (dolist (row rows)
      (pcase (car row)
        ("file" (setq file-count (1+ file-count)))
        ("facade" (setq facade-count (1+ facade-count)))
        ("readme" (setq readme-count (1+ readme-count))))
      (puthash (nth 1 row) (1+ (or (gethash (nth 1 row) by-package) 0))
               by-package))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library package scaffold\n\n")
      (insert "* Summary\n\n")
      (insert (format "- copied files: %d\n" file-count))
      (insert (format "- facade files: %d\n" facade-count))
      (insert (format "- package readmes: %d\n" readme-count))
      (insert "- source of truth: =src/=\n")
      (insert "- scaffold root: =packages/=\n\n")
      (insert "* Counts By Package\n\n")
      (insert "| Package | Entries |\n|---------+---------|\n")
      (let (items)
        (maphash (lambda (key value) (push (cons key value) items))
                 by-package)
        (dolist (item (sort items (lambda (a b) (string< (car a) (car b)))))
          (insert (format "| %s | %d |\n" (car item) (cdr item)))))
      (insert "\n* Notes\n\n")
      (insert "- This scaffold is generated from =build/nemacs-library-package-layout.tsv= semantics.\n")
      (insert "- Regenerate after changing package membership or source files.\n")
      (insert "- Keep =src/= available until package-path smoke gates cover the full facade path.\n"))))

;;;###autoload
(defun nemacs-library-package-scaffold-batch ()
  "Generate the experimental library package scaffold."
  (let* ((layout-rows (nemacs-library-package-layout--rows))
         (guide-rows (nemacs-library-package-guide--rows))
         (rows nil))
    (unless layout-rows
      (error "empty nelisp-emacs library package layout"))
    (dolist (row layout-rows)
      (push (nemacs-library-package-scaffold--copy-layout-row row) rows))
    (push (nemacs-library-package-scaffold--copy-facade) rows)
    (dolist (row guide-rows)
      (push (nemacs-library-package-scaffold--write-readme row) rows))
    (push (nemacs-library-package-scaffold--write-facade-readme) rows)
    (setq rows (nreverse rows))
    (nemacs-library-package-scaffold--write-tsv
     rows nemacs-library-package-scaffold-output)
    (nemacs-library-package-scaffold--write-summary
     rows nemacs-library-package-scaffold-summary-output)
    (princ
     (format
      "nemacs-library-package-scaffold: entries=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-package-scaffold-output
      nemacs-library-package-scaffold-summary-output))))

(provide 'nemacs-library-package-scaffold)

;;; nemacs-library-package-scaffold.el ends here
