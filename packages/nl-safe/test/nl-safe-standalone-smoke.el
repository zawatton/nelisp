;;; nl-safe-standalone-smoke.el --- run nl-safe tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 170 Stage 1: run the exact ERT
;; test bodies from `nl-safe-test.el' and `nl-safe-report-test.el' on
;; `target/nelisp', which has no ert.  A minimal ert shim
;; (`ert-deftest' / `should' / `should-not' / `should-error') is
;; installed first, then the real test files are loaded and every
;; registered test body is executed.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-safe/test/nl-safe-standalone-smoke.el
;;
;; The final line is `nl-safe-standalone-smoke: PASS (N tests)'; any
;; failure raises an error so the process exits non-zero.
;;
;; Dependencies are loaded explicitly by path: on the standalone,
;; (require 'nl-prelude) would silently "succeed" even with the file
;; absent, so `load' is the only trustworthy path (same pattern as
;; nl-prelude's smoke runner).
;;
;; On host Emacs this file is inert for `make test' (its name does not
;; match the `nl-*-test.el' glob) and the shim only installs when ert
;; is absent.

;;; Code:

(require 'ert)

(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-safe/src/nl-safe.el")
(load "packages/nl-safe/src/nl-safe-report.el")
(load "packages/nl-safe/test/nl-safe-test.el")
(load "packages/nl-safe/test/nl-safe-report-test.el")

;; Doc 170 borrow-violation example (examples/nl-safe/borrow-violation.el):
;; keeps the demo honest on the substrate its README claims to support.
(load "packages/nl-safe/test/nl-safe-example-test.el")

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
    (error "nl-safe-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 106)
    (error "nl-safe-standalone-smoke: only %d tests ran (expected >= 106)"
           ran))
  (princ (format "nl-safe-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-safe-standalone-smoke.el ends here
