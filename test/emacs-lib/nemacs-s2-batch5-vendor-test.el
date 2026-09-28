;;; nemacs-s2-batch5-vendor-test.el --- ERT for the S2 coverage batch 5 add  -*- lexical-binding: t; -*-

;;; Commentary:

;; S2 coverage batch 5 (2026-09-28) makes two kinds of change:
;;
;; 1. A library fix in `emacs-buffer-builtins.el': `get-buffer' is always
;;    installed on the standalone now (previously it deferred entirely to
;;    the native primitive whenever one was already `fboundp'), and falls
;;    back to whatever `get-buffer' existed before it for anything it does
;;    not itself recognize.  This fixes a real crash:
;;    `(with-current-buffer (car (buffer-list)) ...)' on this standalone
;;    signalled `(wrong-type-argument stringp BUF)' because the native
;;    `get-buffer' it dispatches through has no notion of this bridge's own
;;    `nelisp-ec-buffer' struct.  On host Emacs this is inert:
;;    `emacs-buffer-builtins--standalone-p' is nil there, so the original
;;    `emacs-buffer-builtins--install-function-p' gate alone still governs
;;    and the genuine C `get-buffer' is untouched during ordinary use; the
;;    tests below force the standalone branch to exercise the new code path
;;    directly, then restore the original function.
;;
;; 2. Seven more GNU Emacs 31.1 vendor files added to
;;    `nelisp-bootstrap-vendor-tail-extra-files' in
;;    `scripts/build-nelisp-bootstrap.el': `vc-hooks.el', `vc.el', `man.el',
;;    `xref.el', `replace.el', `comint.el', `simple.el' (`cl-macs.el' too,
;;    ordered last -- see the long comment above its list entry for why).
;;    Each loads with zero errors on the standalone (verified by probe,
;;    recorded in the batch 5 worklog); `simple.el' needed fix (1) above to
;;    reach that state.  As with batch 3/4, ERT itself only runs on host
;;    Emacs (`make test-fast'); these files are already real Emacs
;;    built-ins there, so `require'/`featurep' short-circuits past this
;;    file's own re-load attempts and host Emacs serves as the oracle for
;;    the fboundp assertions below.  The standalone-specific claims (the
;;    crash fix, and each vendor file loading clean on
;;    `build/nemacs-bootstrap.repl') are manual probe results recorded in
;;    the batch 5 worklog, not something this ERT file can exercise.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-buffer-builtins)
(require 'nelisp-emacs-compat)

;;;; 1. get-buffer fix ------------------------------------------------------

(defun nemacs-s2-batch5-vendor-test--force-standalone-get-buffer ()
  "Reinstall `get-buffer' as if `emacs-buffer-builtins--standalone-p' were t.
Returns the function to restore the original definition.

Uses `advice-add' rather than `cl-letf' to mock
`emacs-buffer-builtins--standalone-p': the reload below hits this same
file's own unconditional `(defun emacs-buffer-builtins--standalone-p ()
...)' top-level form, which plainly overwrites a `cl-letf' binding (the
mock would already be gone by the time the `get-buffer' install check
below it runs).  An `:override' advice survives that redefinition --
`defalias' (what `defun' expands to) explicitly re-applies any existing
advice to a redefined function -- so the mock stays in effect for the
whole reload."
  (let ((original (symbol-function 'get-buffer))
        (mock (lambda () t)))
    (advice-add 'emacs-buffer-builtins--standalone-p :override mock)
    (unwind-protect
        (load (locate-library "emacs-buffer-builtins") nil t)
      (advice-remove 'emacs-buffer-builtins--standalone-p mock))
    (lambda () (fset 'get-buffer original))))

(ert-deftest nemacs-s2-batch5-vendor-test/get-buffer-recognizes-nelisp-ec-buffer ()
  "Normal case: `get-buffer' on a live `nelisp-ec-buffer' returns it."
  (let ((restore (nemacs-s2-batch5-vendor-test--force-standalone-get-buffer)))
    (unwind-protect
        (let ((nelisp-ec--buffers nil)
              (nelisp-ec--current-buffer nil))
          (let ((buf (nelisp-ec-generate-new-buffer
                      " *nemacs-s2-batch5-vendor-test*")))
            (should (eq (get-buffer buf) buf))
            ;; A killed buffer is recognized (`nelisp-ec-buffer-p' still
            ;; matches) but is no longer live, matching real Emacs's
            ;; documented "return it if live else nil".
            (nelisp-ec-kill-buffer buf)
            (should (null (get-buffer buf)))))
      (funcall restore))))

(ert-deftest nemacs-s2-batch5-vendor-test/get-buffer-falls-back-for-non-nelisp-ec-values ()
  "Error/edge case: unrecognized values still resolve exactly as they did
before this fix -- via the captured pre-existing `get-buffer', not via a
new code path that could silently swallow a real error."
  (let ((restore (nemacs-s2-batch5-vendor-test--force-standalone-get-buffer)))
    (unwind-protect
        (progn
          ;; `nil' keeps its own documented meaning (checked before the
          ;; fallback is even consulted).
          (should (null (get-buffer nil)))
          ;; A value neither `nelisp-ec-buffer-p' nor a known registry
          ;; string must reach the fallback captured before this fix
          ;; installed; on host Emacs that fallback is the real C
          ;; `get-buffer', which signals on a bad type exactly as it always
          ;; has -- this fix must not change or hide that.
          (should-error (get-buffer 42) :type 'wrong-type-argument))
      (funcall restore))))

;;;; 2. vendor file additions -----------------------------------------------

(defmacro nemacs-s2-batch5-vendor-test--maybe-load (feature relative-path)
  "Load RELATIVE-PATH (from this file's directory) unless FEATURE is loaded.
Mirrors the `cl-lib-s2-batch3-test.el' / `nemacs-s2-batch4-vendor-test.el'
pattern: on host Emacs FEATURE is already preloaded, so this guard skips
straight past the vendored copy and host Emacs serves as the oracle."
  `(unless (featurep ,feature)
     (load (expand-file-name ,relative-path
                              (file-name-directory
                               (or load-file-name buffer-file-name)))
           nil t)))

(nemacs-s2-batch5-vendor-test--maybe-load 'vc-hooks "../vendor/emacs-lisp/vc/vc-hooks.el")
(nemacs-s2-batch5-vendor-test--maybe-load 'vc "../vendor/emacs-lisp/vc/vc.el")
(nemacs-s2-batch5-vendor-test--maybe-load 'man "../vendor/emacs-lisp/man.el")
(nemacs-s2-batch5-vendor-test--maybe-load 'xref "../vendor/emacs-lisp/progmodes/xref.el")
(nemacs-s2-batch5-vendor-test--maybe-load 'replace "../vendor/emacs-lisp/replace.el")
(nemacs-s2-batch5-vendor-test--maybe-load 'comint "../vendor/emacs-lisp/comint.el")
(nemacs-s2-batch5-vendor-test--maybe-load 'cl-macs "../vendor/emacs-lisp/emacs-lisp/cl-macs.el")
;; `simple' is always already loaded (it is part of host Emacs's own
;; preloaded core), so this guard always skips; listed for symmetry with
;; the fboundp assertions below.
(nemacs-s2-batch5-vendor-test--maybe-load 'simple "../vendor/emacs-lisp/simple.el")

(ert-deftest nemacs-s2-batch5-vendor-test/vendor-fboundp ()
  (dolist (sym '(vc-registered vc-backend vc-up-to-date-p vc-mode-line
                 vc-checkout vc-checkin vc-diff vc-print-log
                 manual-entry Man-getpage-in-background Man-mode
                 xref-make-file-location xref-location-marker
                 xref-backend-references xref--collect-matches
                 perform-replace query-replace replace-regexp occur
                 comint-send-input comint-output-filter comint-send-string
                 make-comint comint-mode
                 cl-typep cl-defmethod cl-defgeneric cl-defstruct
                 kill-region yank open-line back-to-indentation
                 kill-line read-only-mode visual-line-mode))
    (should (fboundp sym)))
  (dolist (sym '(vc-handled-backends kill-ring comint-prompt-regexp
                 xref-marker-ring-length visual-line-fringe-indicators))
    (should (boundp sym))))

(ert-deftest nemacs-s2-batch5-vendor-test/vendor-behavior-round-trip ()
  "Normal case: a couple of the newly-bound functions actually work."
  (should (equal (xref-make-file-location "/tmp/x.el" 3 0)
                  (xref-make-file-location "/tmp/x.el" 3 0)))
  (with-temp-buffer
    (insert "line one\nline two\nline three\n")
    (goto-char (point-min))
    (should (= (how-many "line") 3)))
  ;; Error case: `manual-entry'/`Man-getpage-in-background' need an actual
  ;; man(1) page lookup; assert the documented contract instead --
  ;; `vc-backend' on a file that is not under any VCS returns nil rather
  ;; than signaling, so a non-existent path is the well-defined negative
  ;; case for this group.
  (should (null (vc-backend "/nonexistent/nemacs-s2-batch5-vendor-test-path.el"))))

(provide 'nemacs-s2-batch5-vendor-test)

;;; nemacs-s2-batch5-vendor-test.el ends here
