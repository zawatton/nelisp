;;; verify-production-runtime-path.el --- gate production runtime adapters -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast static gate for the production-vs-test image contract.
;; It intentionally does not load `build/nemacs-bootstrap.el': the bundle is
;; standalone-oriented and may run top-level bootstrap forms under host Emacs.
;; Instead, verify that `nemacs-main' requires the runtime adapters and the
;; generated bootstrap bundle contains their daily-driver symbols.

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

(defconst verify-production-runtime-path--required-main-forms
  '("(require 'emacs-fileio-gui)"
    "(require 'emacs-dired-min-gui)")
  "Forms that connect production entry to GUI runtime adapters.")

(defconst verify-production-runtime-path--required-bootstrap-strings
  '(";;; >>> src/emacs-fileio-gui.el"
    ";;; >>> src/emacs-dired-min-gui.el"
    "(defun emacs-fileio-gui-find-file-core"
    "(defun emacs-fileio-gui-save-buffer-core"
    "(defun emacs-fileio-gui-current-context-command"
    "(defun emacs-fileio-gui-switch-to-buffer-command"
    "(defun emacs-fileio-gui-list-buffers-command"
    "(defun emacs-dired-min-gui-dired-command"
    "(defun emacs-dired-min-gui-current-context-command"
    "(defun emacs-dired-min-gui-apply-mark-core")
  "Bootstrap strings required to keep daily-driver commands off fallback paths.")

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

;;;###autoload
(defun verify-production-runtime-path-batch ()
  "Verify production entry and bootstrap bundle carry runtime adapters."
  (let* ((main (verify-production-runtime-path--slurp
                verify-production-runtime-path-main))
         (bootstrap (verify-production-runtime-path--slurp
                     verify-production-runtime-path-bootstrap))
         (missing-main
          (verify-production-runtime-path--missing
           verify-production-runtime-path--required-main-forms main))
         (missing-bootstrap
          (verify-production-runtime-path--missing
           verify-production-runtime-path--required-bootstrap-strings
           bootstrap)))
    (when (or missing-main missing-bootstrap)
      (princ "verify-production-runtime-path: FAIL\n")
      (dolist (needle missing-main)
        (princ (format "missing nemacs-main form: %s\n" needle)))
      (dolist (needle missing-bootstrap)
        (princ (format "missing bootstrap runtime symbol: %s\n" needle)))
      (kill-emacs 1))
    (princ
     (format "verify-production-runtime-path: ok main=%s bootstrap=%s symbols=%d\n"
             verify-production-runtime-path-main
             verify-production-runtime-path-bootstrap
             (length verify-production-runtime-path--required-bootstrap-strings)))))

(provide 'verify-production-runtime-path)

;;; verify-production-runtime-path.el ends here
