;;; nl-parens-cmd.el --- Interactive commands for nl-parens -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'nl-parens)
(require 'compile)

(defun nl-parens-report-buffer ()
  "Show current-buffer findings in a compilation-jumpable report buffer."
  (interactive)
  (let ((report (nl-parens-report (nl-parens-check-buffer) 'rich)))
    (with-current-buffer (get-buffer-create "*nl-parens*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert report)
      (compilation-mode)
      (goto-char (point-min)))
    (display-buffer "*nl-parens*")))

(defun nl-parens-fix-buffer ()
  "Insert missing closers in this buffer, leaving it modified and unsaved."
  (interactive)
  (let ((count (nl-parens--fix-current-buffer)))
    (message "nl-parens: inserted %d closing %s" count
             (nl-parens--plural count))))

;; Flymake is a host-only convenience.  Keep this file out of the standalone
;; load path so the analysis package retains its dependency-free contract.
;;
;; The two functions below are defined inside the `when', so the byte
;; compiler does not register them the way it registers a top-level
;; `defun'; declare the one referenced by the other or `make compile'
;; fails under `byte-compile-error-on-warn'.
(declare-function nl-parens-flymake-backend "nl-parens-cmd" (report-fn &rest _args))

(when (require 'flymake nil t)
  (defun nl-parens-flymake-backend (report-fn &rest _args)
    "Report current-buffer nl-parens findings to Flymake through REPORT-FN."
    (let ((diagnostics nil) (source (current-buffer)))
      (dolist (finding (nl-parens-check-buffer))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (plist-get finding :line)))
          (let ((begin (point)) (end (line-end-position)))
            (setq diagnostics
                  (cons (flymake-make-diagnostic
                         source begin end :error
                         (nl-parens--message finding))
                        diagnostics)))))
      (funcall report-fn diagnostics)))

  (defun nl-parens-flymake-setup ()
    "Enable buffer-local nl-parens Flymake diagnostics in an Elisp buffer.
Add this function to `emacs-lisp-mode-hook' to receive live feedback."
    (add-hook 'flymake-diagnostic-functions #'nl-parens-flymake-backend nil t)
    (when (fboundp 'flymake-mode) (flymake-mode 1))))

(provide 'nl-parens-cmd)

;;; nl-parens-cmd.el ends here
