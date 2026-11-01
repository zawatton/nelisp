;;; nl-typecheck-gate.el --- Doc 236 build gate for the type checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run from the repository root with the nl-typecheck load path.
;; Environment:
;;   TYPECHECK_MODE            gate (default), vendor or baseline
;;   TYPECHECK_FILES           space-separated files overriding the default set
;;   NELISP_TYPECHECK_STRICT   1 makes notes fail typed modules
;;   GNU_LISP_DIR              GNU lisp/ tree for vendor mode
;; Scope per file: typed (a -*- nelisp-typed: t -*- cookie on line 1),
;; vendor (vendor mode only), own (everything else).  Rules:
;;   typed  error or warning fails; note fails only in strict mode
;;   own    any error fails; warnings may not exceed the baseline count
;;   vendor reported only; vendor mode fails on any error
;; The baseline tools/typecheck-baseline.eld holds per-file warning counts
;; and is written only by TYPECHECK_MODE=baseline.

;;; Code:

(require 'nl-typecheck)

(defconst nl-typecheck-gate--baseline-file "tools/typecheck-baseline.eld")

(defconst nl-typecheck-gate--vendor-names
  '("subr-x" "seq" "ring" "pcase" "map" "cl-seq" "cl-extra" "gv" "nadvice"
    "thunk"))

(defun nl-typecheck-gate--typed-p (file)
  "Non-nil when FILE's first line carries the nelisp-typed cookie."
  (with-temp-buffer
    (insert-file-contents file nil 0 400)
    (goto-char (point-min))
    (let ((line (buffer-substring (point) (line-end-position))))
      (and (string-match "-\\*-.*nelisp-typed:[ \t]*t[ \t;].*-\\*-" line) t))))

(defun nl-typecheck-gate--default-files ()
  "Return lisp/*.el plus typed packages/*/src/*.el files."
  (let ((files (directory-files "lisp" nil "\\.el\\'")) (out nil))
    (dolist (f files) (setq out (cons (concat "lisp/" f) out)))
    (dolist (f (file-expand-wildcards "packages/*/src/*.el"))
      (when (nl-typecheck-gate--typed-p f) (setq out (cons f out))))
    (sort out #'string<)))

(defun nl-typecheck-gate--read-baseline ()
  "Return the baseline alist (FILE . WARNINGS)."
  (when (file-exists-p nl-typecheck-gate--baseline-file)
    (with-temp-buffer
      (insert-file-contents nl-typecheck-gate--baseline-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun nl-typecheck-gate--count (diags level)
  "Count DIAGS of LEVEL."
  (let ((n 0))
    (dolist (d diags) (when (eq (plist-get d :level) level) (setq n (1+ n))))
    n))

(defun nl-typecheck-gate--print (diags levels)
  "Print DIAGS whose level is in LEVELS."
  (dolist (d diags)
    (when (memq (plist-get d :level) levels)
      (princ (concat (nl-typecheck-format-diagnostic d) "\n")))))

(defun nl-typecheck-gate--run-gate (files)
  "Check FILES and return the number of failing files."
  (let ((baseline (nl-typecheck-gate--read-baseline))
        (strict (equal (getenv "NELISP_TYPECHECK_STRICT") "1"))
        (fail 0) (errors 0) (warnings 0) (skipped 0) (forms 0))
    (dolist (f files)
      (let* ((typed (nl-typecheck-gate--typed-p f))
             (nelisp-typecheck-strict (and strict typed))
             (diags (nl-typecheck-file f (if typed 'typed 'own)))
             (e (nl-typecheck-gate--count diags 'error))
             (w (nl-typecheck-gate--count diags 'warning))
             (n (nl-typecheck-gate--count diags 'note))
             (base (or (cdr (assoc f baseline)) 0)))
        (setq errors (+ errors e) warnings (+ warnings w)
              skipped (+ skipped (plist-get nl-typecheck-last-stats :skipped))
              forms (+ forms (plist-get nl-typecheck-last-stats :forms)))
        (cond
         (typed
          (when (or (> e 0) (> w 0) (and strict (> n 0)))
            (setq fail (1+ fail))
            (nl-typecheck-gate--print diags '(error warning note))))
         (t
          (cond ((> e 0)
                 (setq fail (1+ fail))
                 (nl-typecheck-gate--print diags '(error warning)))
                ((> w base)
                 (setq fail (1+ fail))
                 (princ (format "%s: ratchet: %d warnings exceed baseline %d\n"
                                f w base))
                 (nl-typecheck-gate--print diags '(warning))))))))
    (princ (format "typecheck-gate: files=%d forms=%d skipped=%d errors=%d warnings=%d failing=%d\n"
                   (length files) forms skipped errors warnings fail))
    fail))

(defun nl-typecheck-gate--run-baseline (files)
  "Rewrite the baseline from FILES; return the error count."
  (let ((entries nil) (errors 0) (total 0) (skipped 0))
    (dolist (f files)
      (let* ((diags (nl-typecheck-file f 'own))
             (w (nl-typecheck-gate--count diags 'warning)))
        (setq errors (+ errors (nl-typecheck-gate--count diags 'error))
              skipped (+ skipped (plist-get nl-typecheck-last-stats :skipped)))
        (when (> w 0)
          (setq total (+ total w))
          (setq entries (cons (cons f w) entries)))))
    (setq entries (sort entries (lambda (a b) (string< (car a) (car b)))))
    (with-temp-file nl-typecheck-gate--baseline-file
      (insert ";; Per-file warning counts for the Doc 236 type-check ratchet.\n"
              ";; Written only by `make typecheck-baseline-update'.\n(")
      (let ((first t))
        (dolist (e entries)
          (insert (if first "" "\n ") (prin1-to-string e))
          (setq first nil)))
      (insert ")\n"))
    (princ (format "typecheck-baseline: files-with-warnings=%d warnings=%d errors=%d skipped=%d\n"
                   (length entries) total errors skipped))
    errors))

(defun nl-typecheck-gate--run-vendor ()
  "Check the vendored GNU subset and return the error count."
  (let ((dir (getenv "GNU_LISP_DIR")) (errors 0) (warnings 0) (n 0) (skipped 0))
    (unless (and dir (file-directory-p dir))
      (princ "typecheck-vendor: GNU_LISP_DIR is not set to a directory\n")
      (kill-emacs 2))
    (dolist (name nl-typecheck-gate--vendor-names)
      (let* ((base (expand-file-name (concat "emacs-lisp/" name) dir))
             (file (cond ((file-exists-p (concat base ".el")) (concat base ".el"))
                         ((file-exists-p (concat base ".el.gz")) (concat base ".el.gz")))))
        (if (null file)
            (princ (format "typecheck-vendor: %s not found\n" name))
          (let ((diags (nl-typecheck-file file 'vendor)))
            (setq n (1+ n)
                  errors (+ errors (nl-typecheck-gate--count diags 'error))
                  warnings (+ warnings (nl-typecheck-gate--count diags 'warning))
                  skipped (+ skipped (plist-get nl-typecheck-last-stats :skipped)))
            (nl-typecheck-gate--print diags '(error))))))
    (princ (format "typecheck-vendor: files=%d skipped=%d errors=%d warnings=%d\n"
                   n skipped errors warnings))
    (if (= n 0) 1 errors)))

(let* ((mode (or (getenv "TYPECHECK_MODE") "gate"))
       (env-files (getenv "TYPECHECK_FILES"))
       (files (if (and env-files (not (string= env-files "")))
                  (split-string env-files " " t)
                (nl-typecheck-gate--default-files)))
       (nl-typecheck-summary-dir (expand-file-name "target/typecheck/summary"))
       (rc (condition-case err
               (cond ((string= mode "vendor") (nl-typecheck-gate--run-vendor))
                     ((string= mode "baseline") (nl-typecheck-gate--run-baseline files))
                     (t (nl-typecheck-gate--run-gate files)))
             (error (princ (format "typecheck: internal error: %S\n" err)) 2))))
  (kill-emacs (if (and (integerp rc) (> rc 0)) 1 0)))
;;; nl-typecheck-gate.el ends here
