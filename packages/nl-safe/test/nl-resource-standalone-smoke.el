;;; nl-resource-standalone-smoke.el --- run nl-resource tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 170 Stage 3: run the exact ERT
;; test bodies from `nl-resource-test.el' on `target/nelisp', which has
;; no ert.  A minimal ert shim (`ert-deftest' / `should' / `should-not'
;; / `should-error') is installed first, then the real test file is
;; loaded and every registered test body is executed.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-safe/test/nl-resource-standalone-smoke.el
;;
;; The final line is `nl-resource-standalone-smoke: PASS (N tests)'; any
;; failure raises an error so the process exits non-zero.
;;
;; Dependencies are loaded explicitly by path: on the standalone,
;; (require 'nl-safe) would silently "succeed" even with the file
;; absent, so `load' is the only trustworthy path (same pattern as the
;; nl-prelude and nl-safe smoke runners).
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

(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-safe/src/nl-safe.el")
(load "packages/nl-safe/src/nl-resource.el")
(load "packages/nl-safe/test/nl-resource-test.el")

(let ((tests (reverse nl-smoke--tests))
      (ran 0)
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (condition-case err
          (progn
            (funcall (cdr test))
            (setq ran (1+ ran)))
        (error
         (setq failures
               (cons (format "%s: %S" (car test) err) failures)))))
    (setq tests (cdr tests)))
  ;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
  ;; report what the gate checked; its absence is itself a hard failure
  ;; there (see tools/ai/nelisp-ai.sh's `cmd_gate').
  (princ (format "GATE-COUNT checked=%d findings=%d\n" ran (length failures)))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-resource-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 29)
    (error "nl-resource-standalone-smoke: only %d tests ran (expected >= 29)"
           ran))
  (princ (format "nl-resource-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-resource-standalone-smoke.el ends here
