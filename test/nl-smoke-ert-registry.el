;;; nl-smoke-ert-registry.el --- nl-smoke--tests registry over GNU ert -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The standalone smoke runners (packages/*/test/*standalone-smoke.el)
;; read `nl-smoke--tests', a newest-first list of (NAME . THUNK).  The
;; old standalone-compat ert shim filled it; GNU ert (served from
;; vendor/) does not.  Loading this file requires `ert' and records every
;; test defined through `ert-deftest' (which ends in `ert-set-test') as
;; (NAME . BODY-THUNK) on `nl-smoke--tests'.  Loading it twice is safe:
;; the advice is named and re-adding it is idempotent.

;;; Code:

(require 'ert)

(defvar nl-smoke--tests nil
  "Newest-first list of (NAME . THUNK) for the standalone smoke runners.")

(defun nl-smoke-ert-registry--record (name definition)
  "Push (NAME . body thunk of DEFINITION) onto `nl-smoke--tests'."
  (when (and (symbolp name) (ert-test-p definition))
    (setq nl-smoke--tests
          (cons (cons name (ert-test-body definition)) nl-smoke--tests))))

(unless (advice-member-p 'nl-smoke-ert-registry--record 'ert-set-test)
  (advice-add 'ert-set-test :after #'nl-smoke-ert-registry--record
              '((name . nl-smoke-ert-registry--record))))

(provide 'nl-smoke-ert-registry)
;;; nl-smoke-ert-registry.el ends here
