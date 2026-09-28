;;; nemacs-s2-batch4-vendor-test.el --- ERT for the S2 coverage batch 4 vendor add  -*- lexical-binding: t; -*-

;;; Commentary:

;; S2 coverage batch 4 (2026-09-28) adds one GNU Emacs 31.1 vendor file,
;; verbatim, to `nelisp-bootstrap-vendor-tail-extra-files' in
;; `scripts/build-nelisp-bootstrap.el':
;;
;;   - `vendor/emacs-lisp/custom.el' -- the real `defcustom'/theme engine.
;;     Its one top-level `(require 'widget)' resolves, at standalone replay
;;     time, to the small two-function `widget.el' facade (NOT the large
;;     `wid-edit.el' UI, which is autoload-only and never loads here); see
;;     the comment above this entry in `build-nelisp-bootstrap.el' for the
;;     full trail, including why it has to live in the *tail* list rather
;;     than alongside `ring.el'/`format-spec.el' (load-path ordering).
;;
;; `vendor/emacs-lisp/url/url-vars.el' was tried in the same batch and is
;; NOT part of this addition: `standalone-source-normalize-dropped-source-
;; files' already denylists it by basename for the REPL bootstrap path (the
;; same trap batch 3 hit with `thingatpt.el'), so it was dropped before
;; writing any test for it.
;;
;; This file loads the vendored copy directly by path (matching the
;; existing `cl-lib-s2-batch3-test.el' pattern) so the assertions below
;; exercise exactly what ships in the bootstrap bundle, not merely whatever
;; host Emacs happens to preload.  On host Emacs `custom' is already
;; preloaded, so the `featurep' guard skips straight past this file's own
;; top-level forms -- this doubles as the real-Emacs oracle for the
;; assertions below, the same role batch 3's host fallback served for
;; `cl-lib.el'.
;;
;; A manual standalone spot check against `build/nemacs-bootstrap.repl'
;; (custom-set-default, custom-reevaluate-setting, enable-theme,
;; disable-theme, and `(featurep 'widget)') is recorded in the batch 4
;; worklog; ERT itself only runs on host Emacs (`make test-fast'), per the
;; repo's own S3.1-style suites.

;;; Code:

(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (unless (featurep 'custom)
    (load (expand-file-name "../vendor/emacs-lisp/custom.el" dir) nil t)))

(ert-deftest nemacs-s2-batch4-vendor-test/custom-fboundp ()
  (dolist (sym '(custom-set-default custom-initialize-default
                 custom-initialize-reset custom-reevaluate-setting
                 enable-theme disable-theme custom-push-theme
                 custom-declare-theme custom-load-symbol))
    (should (fboundp sym)))
  ;; `widget.el's small facade, pulled in transitively by `custom.el's
  ;; top-level `(require 'widget)'.
  (should (fboundp 'define-widget))
  (should (featurep 'widget)))

(ert-deftest nemacs-s2-batch4-vendor-test/custom-set-default-round-trip ()
  (defvar nemacs-s2-batch4-vendor-test--var 5)
  (custom-declare-variable 'nemacs-s2-batch4-vendor-test--var 5 "doc"
                            :type 'integer :group 'nemacs-s2-batch4-vendor-test)
  (should (= nemacs-s2-batch4-vendor-test--var 5))
  (custom-set-default 'nemacs-s2-batch4-vendor-test--var 42)
  (should (= nemacs-s2-batch4-vendor-test--var 42))
  ;; error case: enabling an undefined theme signals a plain `error'.
  (should (eq (car (should-error
                     (enable-theme 'nemacs-s2-batch4-vendor-test--no-such-theme)
                     :type 'error))
              'error)))

(provide 'nemacs-s2-batch4-vendor-test)

;;; nemacs-s2-batch4-vendor-test.el ends here
