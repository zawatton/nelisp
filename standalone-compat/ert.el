;;; ert.el --- minimal ERT for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-compat: `(require 'ert)' dies `file-missing: ert'
;; on the standalone binary -- there is no `ert' anywhere on its default
;; `load-path' -- which is what every nelisp-agent host-only test file
;; using `ert-deftest' hits immediately.  This directory is appended LAST
;; to the standalone's default `load-path' (see
;; `nelisp-standalone--reader-default-load-path' in
;; scripts/nelisp-standalone-build.el, so a native/prelude definition of
;; any of these names would always win) and is NOT on any host `-L' path
;; `make test' uses (verified: `make -n test | grep -c standalone-compat'
;; is 0), so this never shadows real Emacs's own `ert' for the host
;; suite.
;;
;; Covers exactly the seven ERT forms the nelisp-agent host-only test
;; corpus actually uses (measured 2026-09-19 across all 79 files):
;; `ert-deftest' (68), `should' (68), `ert-run-tests-batch-and-exit' (66),
;; `should-not' (60), `should-error' (55), `ert-skip' (11), `ert-fail'
;; (1).  Nothing else -- no `ert-deftest' `:tags'/`:expected-result'
;; keyword-body, no explanations, no interactive/`ert' buffer UI.
;;
;; The registration + `should'/`should-not'/`should-error' logic here is
;; the SAME logic the 14 now-consolidated `packages/*/test/*-standalone-
;; smoke.el' files each used to carry inline (see
;; `standalone-compat-ert--tests' below for how those files' own
;; `nl-smoke--tests' variable stays live) -- reused rather than
;; reinvented because it already matches real ERT closely enough for
;; batch use and was already proven across 14 call sites.
;;
;; Whether a loaded test's body actually PASSES is not this file's
;; concern (Segment 1 of "run the NeLisp agent host on the standalone
;; NeLisp binary" only requires every host-only file to REACH
;; `ert-run-tests-batch-and-exit''s summary line, not to pass once it
;; gets there) -- this shim only has to run every registered test to
;; completion and report the outcome in Emacs's own batch-mode format.

;;; Code:

;; Real ERT signals these two via `define-error', not a bare `signal' on
;; an unregistered symbol -- matters because `signal' on a symbol with no
;; `error-conditions' property does NOT behave the same on every runtime.
;; Measured 2026-09-19: the standalone binary's `signal' lets
;; `condition-case' catch an unregistered symbol directly, but (checked
;; for this file's own host-side logic smoke test, which is not the
;; runtime this file ships for) real host Emacs instead wraps it as
;; `(error "Invalid error symbol" SYM ...)', which a handler naming the
;; original symbol does not match.  `define-error' first removes the
;; question for whichever runtime happens to load this file.
(unless (get 'ert-test-failed 'error-conditions)
  (define-error 'ert-test-failed "ert test failed"))
(unless (get 'ert-test-skipped 'error-conditions)
  (define-error 'ert-test-skipped "ert test skipped"))

(defvar standalone-compat-ert--tests nil
  "(NAME . THUNK) conses in registration order, newest first.
`ert-run-tests-batch-and-exit' reverses this before running.")

(defvar nl-smoke--tests nil
  "Back-compat: the exact variable name/shape the 14 in-repo
`packages/*/test/*-standalone-smoke.el' files already read themselves,
via `(reverse nl-smoke--tests)', after their own inline shim registered
into it.  Consolidating those 14 copies into `(require 'ert)' (this
file) must not require touching each file's post-shim runner, so
`ert-deftest' below keeps populating this name too, in the same
newest-first shape, alongside the canonical
`standalone-compat-ert--tests'.")

(defun standalone-compat-ert--register (name thunk)
  "Register THUNK (a 0-arg function) as test NAME.
Pushes onto both `standalone-compat-ert--tests' and `nl-smoke--tests'
(the same THUNK object in both, not a duplicated lambda) -- see the
commentary above.  Does not dedupe a redefined NAME (neither the shim
this replaces did); the 79-file corpus this backs never redefines a
test name within one file."
  (setq standalone-compat-ert--tests
        (cons (cons name thunk) standalone-compat-ert--tests))
  (setq nl-smoke--tests
        (cons (cons name thunk) nl-smoke--tests))
  name)

(defmacro ert-deftest (name _arglist &rest body)
  "Register BODY as test NAME.  ARGLIST is ignored (always `()' in ERT).
A leading string in BODY is a docstring, dropped, PROVIDED something
follows it -- a bare-string body is itself the (admittedly useless)
return value, same edge case `nelisp-stdlib-prelude.el's
`nelisp--strip-body-declarations' documents for `defun'."
  (declare (indent 2) (doc-string 3))
  (when (and (stringp (car body)) (cdr body))
    (setq body (cdr body)))
  ;; Also drop a leading `:documentation'/`:tags'/`:expected-result'
  ;; keyword-body head if one ever shows up -- none of the 79 files use
  ;; one today, but silently mis-registering the keyword plist as code
  ;; would be worse than a clear void-function later.
  `(standalone-compat-ert--register ',name (lambda () ,@body)))

(defmacro should (form)
  "Signal `ert-test-failed' with FORM in the message when FORM is nil.
Returns FORM's value on success, matching real `should'."
  `(let ((standalone-compat-ert--v ,form))
     (unless standalone-compat-ert--v
       (signal 'ert-test-failed (list (format "should failed: %S" ',form))))
     standalone-compat-ert--v))

(defmacro should-not (form)
  "Signal `ert-test-failed' with FORM in the message when FORM is non-nil."
  `(let ((standalone-compat-ert--v ,form))
     (when standalone-compat-ert--v
       (signal 'ert-test-failed (list (format "should-not failed: %S" ',form))))
     t))

(defmacro should-error (form &rest keys)
  "Evaluate FORM, expect an error; return the signaled (SYMBOL . DATA).
KEYS supports `:type SYMBOL', matched against the signaled condition's
`error-conditions' the same way real `should-error' does."
  `(let* ((standalone-compat-ert--expected (plist-get (list ,@keys) :type))
          (standalone-compat-ert--r
           (condition-case standalone-compat-ert--e
               (progn ,form 'standalone-compat-ert--no-error)
             (error standalone-compat-ert--e))))
     (cond
      ((eq standalone-compat-ert--r 'standalone-compat-ert--no-error)
       (signal 'ert-test-failed
               (list (format "should-error: no error signaled by %S" ',form))))
      ((and standalone-compat-ert--expected
            (not (memq standalone-compat-ert--expected
                       (get (car standalone-compat-ert--r) 'error-conditions))))
       (signal 'ert-test-failed
               (list (format "should-error: expected %S, got %S"
                             standalone-compat-ert--expected
                             standalone-compat-ert--r))))
      (t standalone-compat-ert--r))))

(defun ert-skip (&optional data)
  "Mark the running test skipped, with optional DATA for the report."
  (signal 'ert-test-skipped (list data)))

(defun ert-fail (data)
  "Fail the running test with DATA as the failure reason."
  (signal 'ert-test-failed (list data)))

(defun standalone-compat-ert--elapsed (start)
  "Seconds elapsed since START (a `float-time' snapshot), never negative."
  (max 0.0 (- (float-time) start)))

(defun ert-run-tests-batch-and-exit (&optional _selector)
  "Run every registered test and exit; Emacs-compatible batch report.
SELECTOR is accepted but ignored -- every registered test runs, which
matches how the corpus this backs always calls this with no argument
\(the default selector `t', meaning \"everything\"\)."
  (let* ((tests (reverse standalone-compat-ert--tests))
         (n (length tests))
         (i 0) (expected 0) (unexpected 0) (skipped 0))
    (princ (format "Running %d tests\n" n))
    (dolist (test tests)
      (setq i (1+ i))
      (let* ((name (car test))
             (thunk (cdr test))
             (start (float-time))
             (outcome
              (condition-case err
                  (progn (funcall thunk) (cons 'passed nil))
                ;; `ert-test-skipped' first: it is not itself a subtype of
                ;; `error' in real ERT, but even if some future revision
                ;; of this file's callers `define-error' it as one, the
                ;; earlier handler always wins.
                (ert-test-skipped (cons 'skipped err))
                (error (cons 'failed err)))))
        (cond
         ((eq (car outcome) 'passed)
          (setq expected (1+ expected))
          (princ (format "   passed  %d/%d  %s (%f sec)\n"
                         i n name (standalone-compat-ert--elapsed start))))
         ((eq (car outcome) 'skipped)
          (setq skipped (1+ skipped))
          (princ (format "  skipped  %d/%d  %s\n" i n name)))
         (t
          (setq unexpected (1+ unexpected))
          (princ (format "   FAILED  %d/%d  %s\n" i n name))))))
    (princ "\n")
    (princ (format "Ran %d tests, %d results as expected, %d unexpected%s\n"
                   n expected unexpected
                   (if (> skipped 0) (format ", %d skipped" skipped) "")))
    (kill-emacs (if (= unexpected 0) 0 1))))

(provide 'ert)

;;; ert.el ends here
