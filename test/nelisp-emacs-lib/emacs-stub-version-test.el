;;; emacs-stub-version-test.el --- ERT for subr.el version helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; T71 (2026-09-05) -- eat.el's `compat-require' macro evaluates
;; `(version< emacs-version VERSION)' at load time
;; (`nelisp-emacs-magit-bridge.el' Commentary), which made
;; `void-function version<' a standalone load-order gap.  By the time
;; this file was written `emacs-stub.el' already carried
;; `version-to-list', `version-list-<', `version-list-=',
;; `version-list-<=', `version-list-not-zero', `version<' and
;; `version<=' (added in 941c65e3, replacing an earlier hand-rolled
;; dotted-numeric parser); `version=' was the one GNU `subr.el' entry
;; still missing (confirmed void-function on the baked standalone
;; runtime image before this file's `emacs-stub.el' change).
;;
;; These tests pin GNU `subr.el' version-comparison semantics --
;; `version-to-list' parsing (including the pre/beta/alpha/rc/cvs/git
;; `version-regexp-alist' fragments and the letter-suffix special
;; case), the list comparators' "trailing zeros are insignificant"
;; zero-padding rule ("1.0" = "1", "1.0.1" > "1"), and `version<' /
;; `version<=' / `version=' string-level wrappers -- against a table
;; of strings whose expected results were captured from host GNU
;; Emacs 31.1 (`emacs -Q --batch').
;;
;; Under host Emacs these run against the host's own `subr.el'
;; definitions (matching the `unless (fboundp ...)' convention used
;; throughout `emacs-stub.el' and asserted on directly elsewhere in
;; this suite, e.g. `emacs-subr-extras-test.el'); under standalone
;; NeLisp the same assertions exercise the `emacs-stub.el' fallbacks.
;; Error-message assertions use `string-match-p' against the
;; substance of the message (offending substring / "must start with a
;; number") rather than exact-string comparison, since surrounding
;; quote characters are subject to `text-quoting-style' curving that
;; can differ between host Emacs and the standalone message formatter.

;;; Code:

(require 'ert)
(require 'emacs-stub)

;;;; version-to-list: valid syntax

(defconst emacs-stub-version-test--valid-cases
  '((".5" . (0 5))
    ("0.9 alpha" . (0 9 -3))
    ("0.9AlphA1" . (0 9 -3 1))
    ("0.9alpha1" . (0 9 -3 1))
    ("0.9snapshot" . (0 9 -4))
    ("1.0-git" . (1 0 -4))
    ("1.0.7.5" . (1 0 7 5))
    ("1.0.cvs" . (1 0 -4))
    ("1.0PRE2" . (1 0 -1 2))
    ("1.0pre2" . (1 0 -1 2))
    ("22.8 Beta3" . (22 8 -2 3))
    ("22.8beta3" . (22 8 -2 3))
    ("6.9.30Beta" . (6 9 30 -2))
    ("2.4.snapshot" . (2 4 -4))
    ("1" . (1))
    ("1.0" . (1 0))
    ("1.0.0" . (1 0 0))
    ("1.2.3" . (1 2 3))
    ("1.2-3" . (1 2 -4 3))
    ("1.0a" . (1 0 1))
    ("1.0z" . (1 0 26))
    ("1.0cvs" . (1 0 -4))
    ("1.0unknown" . (1 0 -4))
    ("1.0bzr" . (1 0 -4))
    ("1.0hg" . (1 0 -4))
    ("1.0darcs" . (1 0 -4))
    ("1.0 rc1" . (1 0 -1 1))
    ("22.8beta" . (22 8 -2))
    ("1.0-" . (1 0 -4)))
  "(STRING . EXPECTED-LIST) pairs captured from host GNU Emacs 31.1
`version-to-list' (`emacs -Q --batch').")

(ert-deftest emacs-stub-version-test/version-to-list-valid ()
  (dolist (case emacs-stub-version-test--valid-cases)
    (should (equal (version-to-list (car case)) (cdr case)))))

;;;; version-to-list: invalid syntax

(ert-deftest emacs-stub-version-test/version-to-list-invalid-syntax ()
  (dolist (bad '("1.0prepre2" "1.0..7.5" "22.8X3" "1..0"))
    (let ((err (should-error (version-to-list bad) :type 'error)))
      (should (string-match-p (regexp-quote bad) (error-message-string err))))))

(ert-deftest emacs-stub-version-test/version-to-list-invalid-not-numeric-start ()
  (dolist (bad '("alpha3.2" ""))
    (let ((err (should-error (version-to-list bad) :type 'error)))
      (should (string-match-p "must start with a number" (error-message-string err))))))

(ert-deftest emacs-stub-version-test/version-to-list-non-string ()
  (should-error (version-to-list 42) :type 'error))

;;;; version-list-not-zero

(ert-deftest emacs-stub-version-test/version-list-not-zero ()
  (should (= 0 (version-list-not-zero nil)))
  (should (= 0 (version-list-not-zero '(0 0 0))))
  (should (= 3 (version-list-not-zero '(0 0 3 0))))
  (should (= -2 (version-list-not-zero '(-2 5)))))

;;;; version-list-< / version-list-= / version-list-<=: zero-padding

(ert-deftest emacs-stub-version-test/version-list-comparators-trailing-zero ()
  ;; "1" == "1.0" == "1.0.0" -- trailing zeros are insignificant.
  (should (version-list-= '(1) '(1 0)))
  (should (version-list-= '(1) '(1 0 0)))
  (should (version-list-<= '(1) '(1 0)))
  (should (version-list-<= '(1 0) '(1)))
  (should-not (version-list-< '(1) '(1 0)))
  (should-not (version-list-< '(1 0) '(1))))

(ert-deftest emacs-stub-version-test/version-list-comparators-real-difference ()
  (should (version-list-< '(1) '(1 0 1)))
  (should-not (version-list-< '(1 0 1) '(1)))
  (should-not (version-list-= '(1 0 1) '(1)))
  (should (version-list-< '(1 -1) '(1)))    ; "1pre" < "1"
  (should (version-list-< '(1 -3) '(1 -2))) ; "1alpha" < "1beta"
  (should (version-list-<= '(1) '(1 0 1)))
  (should-not (version-list-<= '(1 0 1) '(1))))

;;;; version< / version<= / version=

(defconst emacs-stub-version-test--pair-cases
  '(("1.0" "1" eq) ("1.0.1" "1" gt) ("1" "1.0.1" lt)
    ("1.0" "1.0.0" eq) ("1.0" "1.1" lt) ("2.0" "1.9" gt)
    ("1.0pre1" "1.0" lt) ("1.0" "1.0pre1" gt)
    ("1.0alpha" "1.0beta" lt) ("1.0beta" "1.0alpha" gt)
    ("1.0" "1.0" eq) ("1.0.0.0" "1" eq) ("1.0.0.1" "1" gt))
  "(A B RELATION) triples; RELATION is A's ordering against B.")

(ert-deftest emacs-stub-version-test/version-pairs ()
  (dolist (case emacs-stub-version-test--pair-cases)
    (let ((a (nth 0 case)) (b (nth 1 case)) (rel (nth 2 case)))
      (pcase rel
        ('eq (should (version= a b))
             (should (version= b a))
             (should-not (version< a b))
             (should-not (version< b a))
             (should (version<= a b))
             (should (version<= b a)))
        ('lt (should (version< a b))
             (should (version<= a b))
             (should-not (version< b a))
             (should-not (version<= b a))
             (should-not (version= a b)))
        ('gt (should (version< b a))
             (should (version<= b a))
             (should-not (version< a b))
             (should-not (version<= a b))
             (should-not (version= a b)))))))

;;; emacs-stub-version-test.el ends here
