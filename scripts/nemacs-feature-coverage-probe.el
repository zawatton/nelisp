;;; nemacs-feature-coverage-probe.el --- is each reference name bound here? -*- lexical-binding: t; -*-

;;; Commentary:

;; The other half of the coverage measurement (see
;; scripts/nemacs-feature-coverage.sh).  This half runs INSIDE the standalone
;; with the substrate bundle loaded, reads the reference rows host Emacs
;; produced, and answers one question per row: is that name bound here?
;;
;; What the answer means, exactly: `fboundp' / `boundp', nothing more.  A
;; present name may still behave differently, signal where Emacs returns, or
;; be a stub that returns nil.  So the coverage number this produces is an
;; UPPER BOUND on completeness -- the missing column is solid, the present
;; column is "not missing".  It is worth having anyway, because the failure
;; it measures is the one that actually bites: `(require 'url)' answers t,
;; `featurep' answers t, and the call dies on a name the port never defined.

;;; Code:

(defun nemacs-feature-coverage-probe--rows (path)
  "Read feature/kind/name rows from PATH."
  (let ((rows nil))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (parts (split-string line "\t")))
          (when (= (length parts) 3)
            (setq rows (cons parts rows))))
        (forward-line 1)))
    (nreverse rows)))

(defun nemacs-feature-coverage-probe-batch ()
  "Write feature/kind/name/present rows for the reference input."
  (let ((input (getenv "NEMACS_FEATURE_COVERAGE_REFERENCE"))
        (out (getenv "NEMACS_FEATURE_COVERAGE_PRESENT")))
    (unless (and input out)
      (error "nemacs-feature-coverage-probe: paths are unset"))
    (let ((rows (nemacs-feature-coverage-probe--rows input))
          (coding-system-for-write 'utf-8-unix))
      (with-temp-buffer
        (dolist (row rows)
          (let* ((feature (nth 0 row))
                 (kind (nth 1 row))
                 (name (intern (nth 2 row)))
                 (present (if (equal kind "var")
                              (if (boundp name) 1 0)
                            (if (fboundp name) 1 0))))
            (insert (format "%s\t%s\t%s\t%d\n" feature kind name present))))
        (write-region (point-min) (point-max) out nil 'silent)))))

(provide 'nemacs-feature-coverage-probe)

;;; nemacs-feature-coverage-probe.el ends here
