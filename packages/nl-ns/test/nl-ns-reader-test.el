;;; nl-ns-reader-test.el --- ERT tests for nl-ns-reader -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 189 Phase 0.  Two directions matter equally here: an annotated
;; read resolves a short member name to its namespace-qualified symbol
;; (the feature), and every unannotated read -- before, after, and
;; interleaved with an annotated one -- is byte-for-byte what it was
;; before this file existed (the Phase 0 contract).

;;; Code:

(require 'ert)
(require 'nelisp-read)
(require 'nl-ns-in)
(require 'nl-ns-reader)

(defun nl-ns-reader-test--reset ()
  "Start from a clean registry with namespace `tns' declared."
  (nl-ns-clear-namespaces)
  (eval '(nl-ns-define tns :members (limit wrap chunk)) t))

;;;; The opt-in direction: an annotated read resolves ------------------

(ert-deftest nl-ns-reader-read-in-resolves-a-declared-member ()
  (nl-ns-reader-test--reset)
  (should (eq (nl-ns-read-in 'tns "wrap") 'tns-wrap)))

(ert-deftest nl-ns-reader-read-in-does-not-intern-the-short-name ()
  ;; The whole point of resolving before `intern': a HIT must never
  ;; become a table entry as a side effect of the reference read that
  ;; resolved it.  Declaring a namespace member necessarily interns its
  ;; short name too (the declaration's own `:members' list is ordinary
  ;; source data), so that unavoidable, expected intern is undone with
  ;; `unintern' first -- isolating what this test actually checks: does
  ;; reading a REFERENCE occurrence re-intern the short name, the way
  ;; an ordinary (non-resolving) read of the same text always does.
  ;;
  ;; Uses a namespace/member name found nowhere else in this tree
  ;; (grepped before landing this test), never `wrap'/`tns': `test' runs
  ;; every `*-test.el' file in one shared Emacs process (Makefile
  ;; `TEST_LOADS'), and `nl-ns-in-test.el' quotes `wrap' at ITS OWN
  ;; file-load time.  `unintern'-ing a name any other already-loaded
  ;; file has quoted orphans that file's literal from whatever `intern'
  ;; mints next under the same print name -- the exact split-object
  ;; class of bug the reader's own nil/t canonicalization fix this
  ;; session exists to prevent, so this test does not reintroduce it
  ;; via a shared name.  Compares by `symbol-name'/`intern-soft', never
  ;; `eq' against a quoted literal, for the same reason.
  ;;
  ;; `unintern' is a host-Emacs primitive this repo's standalone stdlib
  ;; does not implement (grepped absent from
  ;; `scripts/nelisp-stdlib-prelude.el').  This test's own ERT body is
  ;; reused verbatim by `nl-ns-reader-standalone-smoke.el' (same pattern
  ;; as `nl-ns-in-standalone-smoke.el'), so it skips itself, loudly, on
  ;; that runtime instead of failing void-function on a primitive it
  ;; does not need for anything but this one test.  No `ert-skip': the
  ;; standalone smoke's minimal `ert-deftest' shim does not define it.
  (if (not (fboundp 'unintern))
      (princ "nl-ns-reader-read-in-does-not-intern-the-short-name: GATE-SKIP no `unintern' on this runtime\n")
    (nl-ns-clear-namespaces)
    (eval '(nl-ns-define nl-ns-reader-test-transient-ns
             :members (nl-ns-reader-test-transient-member))
          t)
    (unintern "nl-ns-reader-test-transient-member" obarray)
    (should (null (intern-soft "nl-ns-reader-test-transient-member")))
    (should (equal (symbol-name
                    (nl-ns-read-in 'nl-ns-reader-test-transient-ns
                                    "nl-ns-reader-test-transient-member"))
                   "nl-ns-reader-test-transient-ns-nl-ns-reader-test-transient-member"))
    ;; Resolving a hit must not intern the short name back:
    (should (null (intern-soft "nl-ns-reader-test-transient-member")))
    ;; Control: the same now-uninterned text, read the ordinary way
    ;; (resolver unbound), DOES intern -- confirming the prior assertion
    ;; exercised a real difference, not an artifact of `unintern' itself.
    (should (equal (symbol-name
                    (nelisp-read "nl-ns-reader-test-transient-member"))
                   "nl-ns-reader-test-transient-member"))
    (should (intern-soft "nl-ns-reader-test-transient-member"))))

(ert-deftest nl-ns-reader-read-from-string-in-resolves ()
  (nl-ns-reader-test--reset)
  (should (equal (nl-ns-read-from-string-in 'tns "wrap chunk")
                  '(tns-wrap . 4))))

(ert-deftest nl-ns-reader-read-all-in-resolves-every-form ()
  (nl-ns-reader-test--reset)
  (should (equal (nl-ns-read-all-in
                  'tns "(defun wrap (s) (chunk s limit))")
                 '((defun tns-wrap (s) (tns-chunk s tns-limit))))))

(ert-deftest nl-ns-reader-non-member-tokens-are-untouched ()
  (nl-ns-reader-test--reset)
  (should (eq (nl-ns-read-in 'tns "unrelated-name") 'unrelated-name))
  (should (equal (nl-ns-read-all-in 'tns "(let ((x 1)) (+ x limit))")
                 '((let ((x 1)) (+ x tns-limit))))))

(ert-deftest nl-ns-reader-with-resolution-macro-resolves ()
  (nl-ns-reader-test--reset)
  (should (eq (nl-ns-read-with-resolution tns (nelisp-read "wrap")) 'tns-wrap)))

(ert-deftest nl-ns-reader-read-eval-in-runs-thunk-under-resolution ()
  (nl-ns-reader-test--reset)
  (should (equal (nl-ns-read-eval-in
                  'tns (lambda () (list (nelisp-read "wrap")
                                         (nelisp-read "elsewhere"))))
                 '(tns-wrap elsewhere))))

;;;; The zero-effect direction: unannotated reads are untouched --------

(ert-deftest nl-ns-reader-plain-read-before-any-resolution-is-untouched ()
  (should (eq (nelisp-read "wrap") 'wrap)))

(ert-deftest nl-ns-reader-plain-read-after-resolution-is-restored ()
  (nl-ns-reader-test--reset)
  (nl-ns-read-in 'tns "wrap")
  (should (eq (nelisp-read "wrap") 'wrap))
  (should (null nelisp-read-namespace-resolve)))

(ert-deftest nl-ns-reader-plain-read-restored-even-on-error ()
  (nl-ns-reader-test--reset)
  (should-error
   (nl-ns-read-eval-in 'tns (lambda () (error "boom"))))
  (should (null nelisp-read-namespace-resolve))
  (should (eq (nelisp-read "wrap") 'wrap)))

(ert-deftest nl-ns-reader-hook-defaults-to-nil ()
  (should (null nelisp-read-namespace-resolve)))

(ert-deftest nl-ns-reader-nil-and-t-canonicalize-inside-resolution ()
  ;; The reader's nil/t special case (`nelisp-read.el', the fix this
  ;; session's intern-canonicalization work landed) runs BEFORE the
  ;; resolution hook is ever consulted; nil/t must stay themselves even
  ;; with an active resolution table, including one whose namespace
  ;; happens to declare members that are not nil/t.
  (nl-ns-reader-test--reset)
  (should (eq (nl-ns-read-in 'tns "nil") nil))
  (should (eq (nl-ns-read-in 'tns "t") t)))

(ert-deftest nl-ns-reader-two-namespaces-do-not-leak-into-each-other ()
  (nl-ns-clear-namespaces)
  (eval '(nl-ns-define ns-a :members (wrap)) t)
  (eval '(nl-ns-define ns-b :members (wrap)) t)
  (should (eq (nl-ns-read-in 'ns-a "wrap") 'ns-a-wrap))
  (should (eq (nl-ns-read-in 'ns-b "wrap") 'ns-b-wrap))
  (should (null nelisp-read-namespace-resolve)))

;;;; Errors --------------------------------------------------------------

(ert-deftest nl-ns-reader-undeclared-namespace-signals ()
  (nl-ns-clear-namespaces)
  (should-error (nl-ns-read-in 'nowhere "x")))

(provide 'nl-ns-reader-test)

;;; nl-ns-reader-test.el ends here
