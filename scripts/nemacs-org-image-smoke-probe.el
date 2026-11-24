;;; nemacs-org-image-smoke-probe.el --- Org image smoke, NeLisp and GNU -*- lexical-binding: t; -*-

;; Runs identically on a baked Org runtime image and on GNU Emacs (control).
;; Set `nemacs-org-smoke-fixture' before loading.  Prints ORG-LOAD / ORG-OUTLINE
;; / ORG-EXPORT / ORG-AGENDA PASS|FAIL lines and per-step timing.

(defvar nemacs-org-smoke-fixture nil "Path of the fixture .org file.")

(defun nemacs-org-smoke--out (fmt &rest args)
  (let ((s (concat (apply #'format fmt args) "\n")))
    (if (fboundp 'nelisp--write-stdout-bytes)
        (nelisp--write-stdout-bytes s)
      (princ s))))

(defmacro nemacs-org-smoke--check (name &rest body)
  "Run BODY, print NAME PASS when it returns non-nil, FAIL with the error otherwise."
  `(let ((t0 (float-time)))
     (nemacs-org-smoke--out
      "%s %s (%.2fs)" ,name
      (condition-case e (if (progn ,@body) "PASS" "FAIL")
        (error (format "FAIL %S" e)))
      (- (float-time) t0))))

(nemacs-org-smoke--check "ORG-LOAD"
  (progn (require 'org) (featurep 'org)))

(nemacs-org-smoke--check "ORG-OUTLINE"
  (progn
    (find-file nemacs-org-smoke-fixture)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^\\* TODO")
    (beginning-of-line)
    (org-cycle) (org-cycle) (org-cycle)
    (and (eq major-mode 'org-mode)
         (equal (org-map-entries (lambda () (nth 4 (org-heading-components))))
                '("First task" "Sub heading" "Second" "Third")))))

(nemacs-org-smoke--check "ORG-EXPORT"
  (progn
    (require 'ox-ascii)
    (let ((s (org-export-as 'ascii)))
      (and (string-match-p "SMOKE" s) (string-match-p "First task" s)))))

(nemacs-org-smoke--check "ORG-AGENDA"
  (progn
    (require 'org-agenda)
    (let ((org-agenda-files (list nemacs-org-smoke-fixture)))
      (org-agenda-list nil "2026-11-23" 3)
      (string-match-p "First task"
                      (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'nemacs-org-image-smoke-probe)
;;; nemacs-org-image-smoke-probe.el ends here
