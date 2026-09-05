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

(provide 'emacs-parity-fns2-test)

;;; emacs-parity-fns2-test.el ends here
