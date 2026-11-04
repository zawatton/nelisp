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
(require 'emacs-parity-shims)
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

;; T88: the real init form `(set-file-name-coding-system 'utf-8-unix)'
;; (inside `(when *is-unix* ...)') signaled "utf-8-unix is not suitable
;; for file names" under the standalone substrate even though stock Emacs
;; 30.1/31.1 accepts it -- `coding-system-get' used to answer nil
;; unconditionally for `:ascii-compatible-p' (T52), which made every
;; non-nil coding system look ASCII-incompatible AND not-suitable, so
;; `set-file-name-coding-system''s verbatim-ported validity check errored
;; for every non-nil coding system.  The following tests pin the exact
;; truth table verified against stock Emacs `emacs -Q --batch' (30.1 and
;; 31.1); every `should' below is a real host answer, not an assumption.

(ert-deftest emacs-parity-fns2-test/coding-system-get-ascii-compatible-p-table ()
  "Pin `:ascii-compatible-p' against the stock Emacs 30.1/31.1 truth table."
  (dolist (cs '(utf-8 utf-8-unix utf-8-dos utf-8-mac utf-8-emacs
                undecided raw-text no-conversion ascii us-ascii
                binary emacs-mule latin-1 iso-8859-1 iso-latin-1
                euc-jp shift_jis japanese-shift-jis cp932 sjis))
    (should (eq t (coding-system-get cs :ascii-compatible-p))))
  (dolist (cs '(utf-16 utf-16le utf-16be utf-7 iso-2022-jp prefer-utf-8))
    (should (eq nil (coding-system-get cs :ascii-compatible-p))))
  ;; nil (no explicit coding system) is ASCII-compatible on stock Emacs.
  (should (eq t (coding-system-get nil :ascii-compatible-p))))

(ert-deftest emacs-parity-fns2-test/check-coding-system-callable ()
  (should (fboundp 'check-coding-system))
  (should (equal '(1 . 1) (func-arity (symbol-function 'check-coding-system)))))

(ert-deftest emacs-parity-fns2-test/check-coding-system-accepts-known ()
  (should (eq nil (check-coding-system nil)))
  (dolist (cs '(utf-8 utf-8-unix utf-8-dos utf-8-mac undecided raw-text
                no-conversion binary emacs-mule))
    (should (eq cs (check-coding-system cs)))))

(ert-deftest emacs-parity-fns2-test/check-coding-system-rejects-unknown ()
  "Stock Emacs: `(check-coding-system 'bogus)' signals `coding-system-error'
with data `(bogus)' and message \"Invalid coding system: bogus\"."
  (let ((err (should-error (check-coding-system 'nemacs-t88-bogus-coding-system)
                            :type 'coding-system-error)))
    (should (equal (cdr err) '(nemacs-t88-bogus-coding-system)))
    (should (equal (error-message-string err)
                    "Invalid coding system: nemacs-t88-bogus-coding-system"))))

(ert-deftest emacs-parity-fns2-test/set-file-name-coding-system-t88-gap-form ()
  "The exact form from the user's real init.el that triggered the T88 gap."
  (let (file-name-coding-system)
    (should (eq 'utf-8-unix (set-file-name-coding-system 'utf-8-unix)))
    (should (eq 'utf-8-unix file-name-coding-system))))

(ert-deftest emacs-parity-fns2-test/set-file-name-coding-system-accepts-table ()
  (let (file-name-coding-system)
    (dolist (cs '(utf-8 utf-8-unix utf-8-dos utf-8-mac undecided raw-text
                  no-conversion ascii binary emacs-mule euc-jp shift_jis
                  cp932))
      (should (eq cs (set-file-name-coding-system cs)))
      (should (eq cs file-name-coding-system)))
    (should (eq nil (set-file-name-coding-system nil)))
    (should (eq nil file-name-coding-system))))

(ert-deftest emacs-parity-fns2-test/set-file-name-coding-system-rejects-non-ascii ()
  "Stock Emacs really does reject these -- both `:ascii-compatible-p' and
`:suitable-for-file-name' are nil for the whole UTF-16/UTF-7/ISO-2022-JP
family, verified against Emacs 30.1/31.1.  The T88 fix must not turn
`set-file-name-coding-system' into an unconditional accept."
  (dolist (cs '(utf-16 utf-16le utf-16be utf-7 iso-2022-jp))
    (should-error (set-file-name-coding-system cs) :type 'error)))

(ert-deftest emacs-parity-fns2-test/set-file-name-coding-system-rejects-unknown ()
  (should-error (set-file-name-coding-system 'nemacs-t88-bogus-coding-system)
                :type 'coding-system-error))

(provide 'emacs-parity-fns2-test)

;;; emacs-parity-fns2-test.el ends here
