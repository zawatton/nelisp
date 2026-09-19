;;; nelisp-toml-standalone-smoke.el --- run nelisp-toml tests on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance for `nelisp-toml'.  target/nelisp has no ERT, so
;; a small shim registers the exact bodies in nelisp-toml-test.el -- same
;; pattern as `nl-num-standalone-smoke.el' (packages/nl-num/test/), copied
;; rather than reinvented.
;;
;; This package gets a dedicated standalone gate (unlike nelisp-uuid and
;; nelisp-log, which do not) because string and integer *parsing* is
;; exactly where host Emacs and the standalone reader are known to
;; disagree in this tree; a TOML reader whose only proof of correctness
;; ran through the host Emacs reader path would not actually demonstrate
;; anything about the substrate it exists to run on.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load packages/nelisp-toml/test/nelisp-toml-standalone-smoke.el

;;; Code:

(require 'ert)

;; Explicit paths are intentional: standalone `require' cannot be trusted
;; to locate an absent library, while each source file's own require
;; still checks the already-provided dependency feature.
(load "packages/nelisp-toml/src/nelisp-toml.el")
(load "packages/nelisp-toml/test/nelisp-toml-test.el")

(let ((tests (reverse nl-smoke--tests))
      (ran 0)
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (condition-case error-data
          (progn
            (funcall (cdr test))
            (setq ran (+ ran 1)))
        (error
         (setq failures
               (cons (format "%s: %S" (car test) error-data) failures)))))
    (setq tests (cdr tests)))
  (princ (format "GATE-COUNT checked=%d findings=%d\n"
                 ran (length failures)))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nelisp-toml-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 40)
    (error "nelisp-toml-standalone-smoke: only %d tests ran (expected >= 40)"
           ran))
  (princ (format "nelisp-toml-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nelisp-toml-standalone-smoke.el ends here
