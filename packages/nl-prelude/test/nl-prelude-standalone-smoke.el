;;; nl-prelude-standalone-smoke.el --- run nl-prelude tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 169 Phase 1: run the exact ERT
;; test bodies from `nl-prelude-test.el' on `target/nelisp', which has
;; no ert.  A minimal ert shim (`ert-deftest' / `should' / `should-not'
;; / `should-error') is installed first, then the real test file is
;; loaded.  Doc 198's two million-step positive tests are omitted here:
;; the standalone interpreter accumulates enough transient loop frames
;; that process teardown becomes impractically long.  The dedicated
;; `nl-hygiene-standalone-smoke.el' proves one million mutual bounces;
;; host ERT runs both exact million-step bodies.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-prelude/test/nl-prelude-standalone-smoke.el
;;
;; The final line is `nl-prelude-standalone-smoke: PASS (N tests)'; any
;; failure raises an error so the process exits non-zero.
;;
;; On host Emacs this file is inert for `make test' (its name does not
;; match the `nl-*-test.el' glob) and the shim only installs when ert
;; is absent.

;;; Code:

(defvar nl-smoke--tests nil
  "Alist of (NAME . BODY-FN) registered by the `ert-deftest' shim.")

(unless (featurep 'ert)
  (defmacro ert-deftest (name _args &rest body)
    "Register BODY as test NAME in `nl-smoke--tests'."
    ;; Drop a leading docstring so it is not the (useless) body value.
    (when (and (stringp (car body)) (cdr body))
      (setq body (cdr body)))
    `(setq nl-smoke--tests
           (cons (cons ',name (lambda () ,@body)) nl-smoke--tests)))
  (defmacro should (form)
    `(let ((nl-smoke--v ,form))
       (unless nl-smoke--v
         (error "should failed: %S" ',form))
       nl-smoke--v))
  (defmacro should-not (form)
    `(let ((nl-smoke--v ,form))
       (when nl-smoke--v
         (error "should-not failed: %S" ',form))
       t))
  (defmacro should-error (form &rest keys)
    "Evaluate FORM, expect an error; return (SYMBOL . DATA).
KEYS supports `:type SYMBOL' for condition matching like ert."
    `(let* ((nl-smoke--expected (plist-get (list ,@keys) :type))
            (nl-smoke--r
             (condition-case nl-smoke--e
                 (progn ,form 'nl-smoke--no-error)
               (error nl-smoke--e))))
       (cond
        ((eq nl-smoke--r 'nl-smoke--no-error)
         (error "should-error: no error signaled by %S" ',form))
        ((and nl-smoke--expected
              (not (memq nl-smoke--expected
                         (get (car nl-smoke--r) 'error-conditions))))
         (error "should-error: expected %S, got %S"
                nl-smoke--expected nl-smoke--r))
        (t nl-smoke--r))))
  (provide 'ert))

(load "packages/nl-prelude/src/nl-prelude-trampoline.el")
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-prelude/test/nl-prelude-test.el")

(let ((tests (reverse nl-smoke--tests))
      (ran 0)
      (omitted 0)
      (deep-tests '(nl-prelude-loop-1e6-does-not-overflow
                    nl-prelude-trampoline-mutual-recursion-1e6))
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (if (memq (car test) deep-tests)
          (setq omitted (1+ omitted))
        (condition-case err
            (progn
              (funcall (cdr test))
              (setq ran (1+ ran)))
          (error
           (setq failures
                 (cons (format "%s: %S" (car test) err) failures))))))
    (setq tests (cdr tests)))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-prelude-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 93)
    (error "nl-prelude-standalone-smoke: only %d tests ran (expected >= 93)"
           ran))
  (unless (= omitted 2)
    (error "nl-prelude-standalone-smoke: omitted %d deep tests (expected 2)"
           omitted))
  (princ (format
          "nl-prelude-standalone-smoke: PASS (%d tests, %d delegated deep tests)\n"
          ran omitted)))

;;; nl-prelude-standalone-smoke.el ends here
