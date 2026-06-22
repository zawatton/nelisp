;;; verify-production-runtime-path.el --- gate production runtime adapters -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast static gate for the production-vs-test image contract.
;; It intentionally does not load `build/nemacs-bootstrap.el': the bundle is
;; standalone-oriented and may run top-level bootstrap forms under host Emacs.
;; Instead, verify that `nemacs-main' requires the runtime modules, the
;; generated bootstrap bundle contains their daily-driver symbols, and the
;; same sources are mapped by the library package/app scaffolds.

;;; Code:

(require 'cl-lib)

(defvar verify-production-runtime-path-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar verify-production-runtime-path-bootstrap
  (expand-file-name "build/nemacs-bootstrap.el"
                    verify-production-runtime-path-repo-root)
  "Generated bootstrap bundle to inspect.")

(defvar verify-production-runtime-path-main
  (expand-file-name "src/nemacs-main.el"
                    verify-production-runtime-path-repo-root)
  "Production entry point to inspect.")

(defvar verify-production-runtime-path-package-scaffold
  (expand-file-name "build/nemacs-library-package-scaffold.tsv"
                    verify-production-runtime-path-repo-root)
  "Generated reusable package scaffold index to inspect.")

(defvar verify-production-runtime-path-app-scaffold
  (expand-file-name "build/nemacs-library-app-scaffold.tsv"
                    verify-production-runtime-path-repo-root)
  "Generated app/frontend scaffold index to inspect.")

(defvar verify-production-runtime-path-summary-output
  (expand-file-name "build/nemacs-production-runtime-path.org"
                    verify-production-runtime-path-repo-root)
  "Org summary output for production runtime path verification.")

(defconst verify-production-runtime-path--required-modules
  '((emacs-fileio-gui
     :source "src/emacs-fileio-gui.el"
     :scaffold app
     :symbol "(defun emacs-fileio-gui-find-file-core")
    (emacs-dired-min-gui
     :source "src/emacs-dired-min-gui.el"
     :scaffold app
     :symbol "(defun emacs-dired-min-gui-dired-command")
    (emacs-help-gui
     :source "src/emacs-help-gui.el"
     :scaffold app
     :symbol "(defun emacs-help-gui-describe-function-core")
    (emacs-info
     :source "src/emacs-info.el"
     :scaffold package
     :symbol "(defun emacs-info-gui-info-core")
    (emacs-replace
     :source "src/emacs-replace.el"
     :scaffold package
     :symbol "(defun emacs-query-replace-region")
    (emacs-shell-command
     :source "src/emacs-shell-command.el"
     :scaffold package
     :symbol "(defun emacs-shell-command-run-to-string")
    (emacs-command-loop
     :source "src/emacs-command-loop.el"
     :scaffold package
     :symbol "(defun emacs-command-loop-gui-register-backend")
    (emacs-fileio-builtins
     :source "src/emacs-fileio-builtins.el"
     :scaffold package
     :symbol "(defun emacs-fileio-locate-library")
    (emacs-keymap
     :source "src/emacs-keymap.el"
     :scaffold package
     :symbol "(defun emacs-keymap-make-keymap"))
  "Production entry modules that must stay mapped for reusable extraction.")

(defun verify-production-runtime-path--module-feature (module)
  "Return MODULE feature symbol."
  (car module))

(defun verify-production-runtime-path--module-get (module key)
  "Return MODULE property KEY."
  (plist-get (cdr module) key))

(defun verify-production-runtime-path--module-require-form (module)
  "Return the required top-level require form string for MODULE."
  (format "(require '%s)"
          (verify-production-runtime-path--module-feature module)))

(defun verify-production-runtime-path--module-source-header (module)
  "Return generated bootstrap source header required for MODULE."
  (format ";;; >>> %s"
          (verify-production-runtime-path--module-get module :source)))

(defun verify-production-runtime-path--required-main-forms ()
  "Return forms that connect production entry to runtime modules."
  (mapcar #'verify-production-runtime-path--module-require-form
          verify-production-runtime-path--required-modules))

(defun verify-production-runtime-path--required-bootstrap-strings ()
  "Return bootstrap strings required to keep runtime modules present."
  (let (strings)
    (dolist (module verify-production-runtime-path--required-modules
                    (nreverse strings))
      (push (verify-production-runtime-path--module-source-header module)
            strings)
      (push (verify-production-runtime-path--module-get module :symbol)
            strings))))

(defun verify-production-runtime-path--slurp (file)
  "Return FILE contents, signalling when FILE is missing."
  (unless (file-readable-p file)
    (user-error "missing readable file: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun verify-production-runtime-path--missing (needles haystack)
  "Return NEEDLES not present in HAYSTACK."
  (cl-remove-if (lambda (needle)
                  (string-match-p (regexp-quote needle) haystack))
                needles))

(defun verify-production-runtime-path--tsv-fields (line)
  "Return tab-separated fields parsed from LINE."
  (split-string line "\t"))

(defun verify-production-runtime-path--scaffold-row (source index)
  "Return scaffold row plist for SOURCE in INDEX, or nil."
  (when (file-readable-p index)
    (with-temp-buffer
      (insert-file-contents index)
      (let (row)
        (goto-char (point-min))
        (forward-line 1)
        (while (and (not row) (not (eobp)))
          (let ((fields (verify-production-runtime-path--tsv-fields
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))))
            (when (and (equal (nth 0 fields) "file")
                       (equal (nth 3 fields) source))
              (setq row (list :id (nth 1 fields)
                              :role (nth 2 fields)
                              :source (nth 3 fields)
                              :target (nth 4 fields)))))
          (forward-line 1))
        row))))

(defun verify-production-runtime-path--module-scaffold-row (module)
  "Return the scaffold row expected for MODULE."
  (let ((source (verify-production-runtime-path--module-get module :source))
        (scaffold (verify-production-runtime-path--module-get module :scaffold)))
    (pcase scaffold
      ('package
       (verify-production-runtime-path--scaffold-row
        source verify-production-runtime-path-package-scaffold))
      ('app
       (verify-production-runtime-path--scaffold-row
        source verify-production-runtime-path-app-scaffold))
      (_ nil))))

(defun verify-production-runtime-path--missing-scaffold-modules ()
  "Return production modules missing from their expected scaffold."
  (cl-remove-if #'verify-production-runtime-path--module-scaffold-row
                verify-production-runtime-path--required-modules))

(defun verify-production-runtime-path--status (module bootstrap)
  "Return summary status plist for MODULE against BOOTSTRAP."
  (let* ((feature (verify-production-runtime-path--module-feature module))
         (source (verify-production-runtime-path--module-get module :source))
         (scaffold (verify-production-runtime-path--module-get module :scaffold))
         (header (verify-production-runtime-path--module-source-header module))
         (symbol (verify-production-runtime-path--module-get module :symbol))
         (row (verify-production-runtime-path--module-scaffold-row module)))
    (list :feature feature
          :source source
          :scaffold scaffold
          :bootstrap (and (string-match-p (regexp-quote header) bootstrap)
                          (string-match-p (regexp-quote symbol) bootstrap))
          :target (plist-get row :target)
          :role (plist-get row :role))))

(defun verify-production-runtime-path--write-summary (statuses)
  "Write Org summary for STATUSES."
  (make-directory (file-name-directory verify-production-runtime-path-summary-output)
                  t)
  (with-temp-buffer
    (insert "#+TITLE: nemacs production runtime path verification\n\n")
    (insert "* Summary\n\n")
    (insert (format "- modules: %d\n" (length statuses)))
    (insert "- bootstrap symbols: ok\n")
    (insert "- scaffold mappings: ok\n\n")
    (insert "* Runtime Modules\n\n")
    (insert "| Feature | Source | Scaffold | Role | Target | Bootstrap |\n")
    (insert "|---------+--------+----------+------+--------+-----------|\n")
    (dolist (status statuses)
      (insert
       (format "| =%s= | =%s= | =%s= | =%s= | =%s= | %s |\n"
               (plist-get status :feature)
               (plist-get status :source)
               (plist-get status :scaffold)
               (or (plist-get status :role) "")
               (or (plist-get status :target) "")
               (if (plist-get status :bootstrap) "ok" "missing"))))
    (insert "\n* Notes\n\n")
    (insert "- App/frontend glue must be mapped by =nemacs-library-app-scaffold=.\n")
    (insert "- Reusable runtime modules must be mapped by =nemacs-library-package-scaffold=.\n")
    (insert "- This gate keeps production runtime inputs visible during the src-to-package extraction.\n")
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (point-min) (point-max)
                    verify-production-runtime-path-summary-output
                    nil 'silent))))

;;;###autoload
(defun verify-production-runtime-path-batch ()
  "Verify production entry, bootstrap bundle, and scaffold runtime mapping."
  (let* ((main (verify-production-runtime-path--slurp
                verify-production-runtime-path-main))
         (bootstrap (verify-production-runtime-path--slurp
                     verify-production-runtime-path-bootstrap))
         (missing-main
          (verify-production-runtime-path--missing
           (verify-production-runtime-path--required-main-forms) main))
         (missing-bootstrap
          (verify-production-runtime-path--missing
           (verify-production-runtime-path--required-bootstrap-strings)
           bootstrap))
         (missing-scaffold
          (verify-production-runtime-path--missing-scaffold-modules))
         (statuses
          (mapcar (lambda (module)
                    (verify-production-runtime-path--status module bootstrap))
                  verify-production-runtime-path--required-modules)))
    (verify-production-runtime-path--write-summary statuses)
    (when (or missing-main missing-bootstrap missing-scaffold)
      (princ "verify-production-runtime-path: FAIL\n")
      (dolist (needle missing-main)
        (princ (format "missing nemacs-main form: %s\n" needle)))
      (dolist (needle missing-bootstrap)
        (princ (format "missing bootstrap runtime symbol: %s\n" needle)))
      (dolist (module missing-scaffold)
        (princ
         (format "missing %s scaffold mapping: %s\n"
                 (verify-production-runtime-path--module-get module :scaffold)
                 (verify-production-runtime-path--module-get module :source))))
      (kill-emacs 1))
    (princ
     (format "verify-production-runtime-path: ok main=%s bootstrap=%s modules=%d summary=%s\n"
             verify-production-runtime-path-main
             verify-production-runtime-path-bootstrap
             (length verify-production-runtime-path--required-modules)
             verify-production-runtime-path-summary-output))))

(provide 'verify-production-runtime-path)

;;; verify-production-runtime-path.el ends here
