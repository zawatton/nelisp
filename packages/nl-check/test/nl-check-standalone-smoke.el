;;; nl-check-standalone-smoke.el --- run nl-resource tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 170 sections 6.2/6.3/10: run the exact ERT
;; test bodies from `nl-check-test.el' on `target/nelisp', which has
;; no ert.  A minimal ert shim (`ert-deftest' / `should' / `should-not'
;; / `should-error') is installed first, then the real test file is
;; loaded and every registered test body is executed.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nl-check/test/nl-check-standalone-smoke.el
;;
;; The final line is `nl-check-standalone-smoke: PASS (N tests)'; any
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

(require 'ert)

(load "packages/nl-prelude/src/nl-prelude-trampoline.el") ; wave8: nl-prelude requires it
(load "packages/nl-prelude/src/nl-prelude.el")
(load "packages/nl-safe/src/nl-safe.el")
(load "packages/nl-safe/src/nl-resource.el")
(load "packages/nl-check/src/nl-check.el")
(load "packages/nl-check/test/nl-check-test.el")

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
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-check-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 31)
    (error "nl-check-standalone-smoke: only %d tests ran (expected >= 31)"
           ran))
  (princ (format "nl-check-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-check-standalone-smoke.el ends here
