;;; rx-test.el --- ERT for the rx facade's char-pattern/or-optimization helpers  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/rx.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; S2 coverage batch (2026-09-28): `rx--normalize-char-pattern' and
;; `rx--optimize-or-args' are the American-spelling names GNU Emacs 31.1
;; uses; this facade had ported an older GNU Emacs release that spelled
;; them `rx--normalise-char-pattern'/`rx--optimise-or-args' (also present,
;; unchanged, in vendor/emacs-lisp's own older reference copy).  Renamed
;; here (and in the one standalone-only caller, `emacs-parity-rx.el') to
;; match current GNU Emacs; the function bodies were already
;; byte-for-byte identical to upstream (only the British/American
;; spelling in a couple of docstrings/comments differed).  Values pinned
;; against real GNU Emacs 31.1.

(ert-deftest rx-test/s2-batch-fboundp ()
  (dolist (sym '(rx--normalize-char-pattern rx--optimize-or-args))
    (should (fboundp sym))))

(ert-deftest rx-test/normalize-char-pattern-values ()
  (should (equal (rx--normalize-char-pattern ?a) "a"))
  (should (equal (rx--normalize-char-pattern '(or "a" ?b)) '(or "a" "b")))
  (should (equal (rx--normalize-char-pattern "z") "z")))

(ert-deftest rx-test/optimize-or-args-values ()
  (should (equal (rx--optimize-or-args nil) '(rx--char-alt nil)))
  ;; All-string branches collapse to one `(seq (regexp ...))' rather
  ;; than staying as separate `or' alternatives.
  (should (equal (rx-to-string '(or "cat" "car" "dog") t)
                 "\\(?:ca[rt]\\|dog\\)")))

(ert-deftest rx-test/or-and-not-end-to-end ()
  "End-to-end `rx-to-string' cases that exercise both renamed helpers
through the normal `or'/`not'/`any' translation path."
  (should (equal (rx-to-string '(any "a-c" digit) t) "[a-c[:digit:]]"))
  (should (equal (rx-to-string '(not (syntax whitespace)) t) "\\S-"))
  (should (equal (rx-to-string '(or (any "a-c") digit) t) "[a-c[:digit:]]")))

(ert-deftest rx-test/not-form-arity-error ()
  "`(not ...)' on something that cannot reduce to a char-alt signals,
matching real GNU Emacs 31.1."
  (should (eq (car (should-error
                     (rx-to-string '(not (repeat 1 2 "a")) t)))
              'error)))

(provide 'rx-test)

;;; rx-test.el ends here
