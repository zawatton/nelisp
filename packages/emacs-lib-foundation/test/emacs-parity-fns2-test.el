;;; emacs-parity-fns2-test.el --- ERT tests for emacs-parity-fns2  -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for `coding-system-get' (T52): the function was void
;; anywhere in src/ except a magit-bridge-only shim not on the default boot
;; path (the standalone package load matrix hit `(void-function
;; coding-system-get)' for `message', `webkit', `webkit-ace',
;; `webkit-dark' -- none of which are magit).  The definition here is gated
;; `unless (fboundp ...)', so under host Emacs it is a no-op and these tests
;; exercise the host's own real `mule.el' algorithm; under standalone
;; NeLisp (no coding-system attribute table) they exercise the honest-nil
;; substrate answer documented at the definition site in
;; `emacs-parity-fns2.el'.

;;; Code:

(require 'ert)
(require 'emacs-parity-fns2)

(ert-deftest emacs-parity-fns2-test/coding-system-get-callable ()
  (should (fboundp 'coding-system-get))
  (should (equal '(2 . 2) (func-arity (symbol-function 'coding-system-get))))
  (should (not (eq 'ERR
                    (condition-case nil
                        (coding-system-get 'utf-8 :mime-charset)
                      (error 'ERR))))))

(ert-deftest emacs-parity-fns2-test/coding-system-get-vendor-caller-shape ()
  "Exercise the exact property-name shape real vendor callers use.
`set-file-name-coding-system' (emacs-parity-fns2.el, same file) and
mule.el's own callers only ever read `:ascii-compatible-p' /
`:suitable-for-file-name' / `:mime-charset' as an optional decoration,
taking their fallback branch on nil -- these must never signal for a
coding system that genuinely exists (host's real answer) nor for the
substrate's honest-nil standalone answer."
  (dolist (prop '(:ascii-compatible-p :suitable-for-file-name :mime-charset))
    (should (not (eq 'ERR (condition-case nil
                               (coding-system-get 'utf-8 prop)
                             (error 'ERR)))))))

;; T62: `define-translation-table' had a real implementation in
;; `emacs-translation-table.el' (its own behavior is covered by
;; `emacs-translation-table-test.el') that nothing on the default boot
;; path required, so it was void for `cp5022x'.  This test covers the
;; *wiring*: requiring `emacs-parity-fns2' (already on the default boot
;; path, per the file commentary) must transitively provide it.
(ert-deftest emacs-parity-fns2-test/define-translation-table-wired-transitively ()
  (should (featurep 'emacs-translation-table))
  (should (fboundp 'define-translation-table))
  (should (equal '(1 . many) (func-arity (symbol-function 'define-translation-table)))))

;; T62: `vc-directory-exclusion-list' is a real defcustom in the vendored
;; `vc-hooks.el' that nothing on the default boot path required, so it
;; was void for `projectile', `magit-todos', `consult-projectile'.  This
;; test covers the wiring: requiring `emacs-parity-fns2' must
;; transitively provide `vc-hooks' and bind the real value.  Verified
;; against host Emacs 31.1: `vc-directory-exclusion-list' is a non-empty
;; list of strings that includes at least the classic VCS directory
;; names common to every Emacs version this vendored `vc-hooks.el' could
;; plausibly be (this vendored copy predates `.repo'/`.jj' support, so
;; the list is a real, if older, GNU value rather than a fabricated one).
(ert-deftest emacs-parity-fns2-test/vc-directory-exclusion-list-wired-transitively ()
  (should (featurep 'vc-hooks))
  (should (boundp 'vc-directory-exclusion-list))
  (should (listp vc-directory-exclusion-list))
  (dolist (entry vc-directory-exclusion-list)
    (should (stringp entry)))
  (dolist (classic '("CVS" "RCS" ".git" ".svn" ".hg" ".bzr"))
    (should (member classic vc-directory-exclusion-list))))

(provide 'emacs-parity-fns2-test)

;;; emacs-parity-fns2-test.el ends here
