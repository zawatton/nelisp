;;; nemacs-library-app-scaffold.el --- generate experimental app scaffold -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate an experimental app/frontend scaffold for GUI bridge runtime glue
;; and app-local bootstrap utilities.
;; This is intentionally separate from the reusable library package scaffold:
;; these files are not counted as reusable packages yet.  The source of truth
;; remains src/; regenerate this scaffold instead of editing copied files.

;;; Code:

(require 'subr-x)

(defvar nemacs-library-app-scaffold-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar nemacs-library-app-scaffold-output
  (expand-file-name "build/nemacs-library-app-scaffold.tsv"
                    nemacs-library-app-scaffold-repo-root)
  "TSV output path.")

(defvar nemacs-library-app-scaffold-summary-output
  (expand-file-name "build/nemacs-library-app-scaffold.org"
                    nemacs-library-app-scaffold-repo-root)
  "Org summary output path.")

(defconst nemacs-library-app-scaffold--app-id "nelisp-emacs-app-gui"
  "Generated app/frontend scaffold identifier.")

(defconst nemacs-library-app-scaffold--entries
  '(("app-bootstrap" "src/emacs-init.el"
     "packages/nelisp-emacs-app-gui/lisp/emacs-init.el")
    ("app-bootstrap" "src/nemacs-loadup.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-loadup.el")
    ("app-utility" "src/nemacs-init-transport.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-init-transport.el")
    ("app-utility" "src/image-baker.el"
     "packages/nelisp-emacs-app-gui/lisp/image-baker.el")
    ("app-entry" "src/nemacs-main.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-main.el")
    ("bridge-family" "src/emacs-fileio-gui.el"
     "packages/nelisp-emacs-app-gui/lisp/emacs-fileio-gui.el")
    ("bridge-family" "src/emacs-dired-min-gui.el"
     "packages/nelisp-emacs-app-gui/lisp/emacs-dired-min-gui.el")
    ("bridge-family" "src/emacs-help-gui.el"
     "packages/nelisp-emacs-app-gui/lisp/emacs-help-gui.el")
    ("bridge-runtime" "src/nemacs-gui-file-bridge-runtime.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-gui-file-bridge-runtime.el")
    ("frontend" "src/nemacs-gtk-view-menu.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-gtk-view-menu.el")
    ("frontend" "src/nemacs-gtk-frontend.el"
     "packages/nelisp-emacs-app-gui/lisp/nemacs-gtk-frontend.el"))
  "App/frontend source files staged for package-backed smoke tests.")

(defconst nemacs-library-app-scaffold--readme
  "packages/nelisp-emacs-app-gui/README.org"
  "README target for the generated app/frontend scaffold.")

(defconst nemacs-library-app-scaffold--obsolete-targets
  '("packages/nelisp-emacs-app-gui/lisp/emacs-dump.el"
    "packages/nelisp-emacs-app-gui/lisp/image-loader.el")
  "Generated app scaffold targets that moved to reusable packages.")

(defun nemacs-library-app-scaffold--tsv-cell (value)
  "Return VALUE formatted as one TSV cell."
  (replace-regexp-in-string "[\t\n\r]+" " " (format "%s" (or value ""))))

(defun nemacs-library-app-scaffold--row (&rest cells)
  "Return CELLS as a TSV line."
  (mapconcat #'nemacs-library-app-scaffold--tsv-cell cells "\t"))

(defun nemacs-library-app-scaffold--target-path (relative)
  "Return absolute target path for RELATIVE."
  (expand-file-name relative nemacs-library-app-scaffold-repo-root))

(defun nemacs-library-app-scaffold--copy-entry (entry)
  "Copy one app scaffold ENTRY and return a report row."
  (let* ((role (nth 0 entry))
         (source-relative (nth 1 entry))
         (target-relative (nth 2 entry))
         (source (expand-file-name source-relative
                                   nemacs-library-app-scaffold-repo-root))
         (target (nemacs-library-app-scaffold--target-path target-relative)))
    (unless (file-exists-p source)
      (error "missing app scaffold source: %s" source-relative))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (list "file" nemacs-library-app-scaffold--app-id role
          source-relative target-relative)))

(defun nemacs-library-app-scaffold--delete-obsolete-targets ()
  "Delete generated app scaffold files that no longer belong to APP."
  (dolist (relative nemacs-library-app-scaffold--obsolete-targets)
    (let ((target (nemacs-library-app-scaffold--target-path relative)))
      (when (file-exists-p target)
        (delete-file target)))))

(defun nemacs-library-app-scaffold--write-readme ()
  "Write README for the generated app/frontend scaffold and return a row."
  (let ((readme (nemacs-library-app-scaffold--target-path
                 nemacs-library-app-scaffold--readme)))
    (make-directory (file-name-directory readme) t)
    (with-temp-file readme
      (insert "#+TITLE: nelisp-emacs app/gui scaffold\n\n")
      (insert "* Status\n\n")
      (insert "Generated experimental app/frontend scaffold.  Do not edit this copy as the source of truth; edit =src/= and regenerate.\n\n")
      (insert "* Scope\n\n")
      (insert "This scaffold stages bootstrap policy, app-local utilities, GUI bridge glue, and GTK frontend glue that are not part of the reusable library package count.  It exists so package-path frontend, bridge image, and app utility tests can avoid implicit =src/= fallbacks for known app/frontend files.\n\n")
      (insert "* Regenerate\n\n")
      (insert "#+begin_src sh\n")
      (insert "make nemacs-library-app-scaffold\n")
      (insert "#+end_src\n\n")
      (insert "* Smoke\n\n")
      (insert "#+begin_src sh\n")
      (insert "make nemacs-library-package-gui-bridge-smoke\n")
      (insert "make nemacs-library-package-gui-bridge-standalone-smoke\n")
      (insert "#+end_src\n"))
    (list "readme" nemacs-library-app-scaffold--app-id "metadata" ""
          nemacs-library-app-scaffold--readme)))

(defun nemacs-library-app-scaffold--write-tsv (rows output)
  "Write app scaffold ROWS to TSV OUTPUT."
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (insert
     (nemacs-library-app-scaffold--row
      "kind" "app_id" "role" "source_file" "target_file")
     "\n")
    (dolist (row rows)
      (insert (apply #'nemacs-library-app-scaffold--row row) "\n"))))

(defun nemacs-library-app-scaffold--write-summary (rows output)
  "Write app scaffold ROWS summary to Org OUTPUT."
  (let ((file-count 0)
        (readme-count 0)
        (by-role (make-hash-table :test 'equal)))
    (dolist (row rows)
      (pcase (car row)
        ("file" (setq file-count (1+ file-count)))
        ("readme" (setq readme-count (1+ readme-count))))
      (puthash (nth 2 row) (1+ (or (gethash (nth 2 row) by-role) 0))
               by-role))
    (make-directory (file-name-directory output) t)
    (with-temp-file output
      (insert "#+TITLE: nemacs library app scaffold\n\n")
      (insert "* Summary\n\n")
      (insert (format "- copied files: %d\n" file-count))
      (insert (format "- app readmes: %d\n" readme-count))
      (insert "- source of truth: =src/=\n")
      (insert "- scaffold root: =packages/nelisp-emacs-app-gui/=\n")
      (insert "- package-count impact: none; app scaffold is outside reusable package descriptors\n\n")
      (insert "* Counts By Role\n\n")
      (insert "| Role | Entries |\n|------+---------|\n")
      (let (items)
        (maphash (lambda (key value) (push (cons key value) items))
                 by-role)
        (dolist (item (sort items (lambda (a b) (string< (car a) (car b)))))
          (insert (format "| %s | %d |\n" (car item) (cdr item)))))
      (insert "\n* Notes\n\n")
      (insert "- This scaffold is a staging artifact for app/bootstrap/frontend glue, not a reusable library package.\n")
      (insert "- Regenerate after changing staged GUI bridge or GTK frontend app sources.\n")
      (insert "- Keep shrinking fallback-to-=src/= image inputs as ownership becomes explicit.\n"))))

;;;###autoload
(defun nemacs-library-app-scaffold-batch ()
  "Generate the experimental app/frontend scaffold."
  (let (rows)
    (nemacs-library-app-scaffold--delete-obsolete-targets)
    (dolist (entry nemacs-library-app-scaffold--entries)
      (push (nemacs-library-app-scaffold--copy-entry entry) rows))
    (push (nemacs-library-app-scaffold--write-readme) rows)
    (setq rows (nreverse rows))
    (nemacs-library-app-scaffold--write-tsv
     rows nemacs-library-app-scaffold-output)
    (nemacs-library-app-scaffold--write-summary
     rows nemacs-library-app-scaffold-summary-output)
    (princ
     (format
      "nemacs-library-app-scaffold: entries=%d output=%s summary=%s\n"
      (length rows)
      nemacs-library-app-scaffold-output
      nemacs-library-app-scaffold-summary-output))))

(provide 'nemacs-library-app-scaffold)

;;; nemacs-library-app-scaffold.el ends here
