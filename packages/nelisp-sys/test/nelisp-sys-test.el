;;; nelisp-sys-test.el --- ERT smoke tests for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Package-level smoke tests.  Per-stage behavior is tested in the
;; sibling nelisp-sys-<topic>-test.el files.  These tests must keep
;; passing after extraction (Doc 130 testing strategy).

;;; Code:

(require 'ert)
(require 'nelisp-sys)
(require 'nelisp-sys-adapter-nelisp)

(ert-deftest nelisp-sys-loads ()
  "The package aggregator loads and exposes a version string."
  (should (featurep 'nelisp-sys))
  (should (stringp (nelisp-sys-version)))
  (should (string-match-p "\\." (nelisp-sys-version))))

(ert-deftest nelisp-sys-error-is-defined ()
  "The package error symbol exists and is an `error' subtype."
  (should (get 'nelisp-sys-error 'error-conditions))
  (should (memq 'error (get 'nelisp-sys-error 'error-conditions))))

(ert-deftest nelisp-sys-adapter-availability-is-boolean ()
  "The adapter availability predicate returns a definite boolean."
  (let ((v (nelisp-sys-adapter-available-p)))
    (should (memq v '(nil t)))))

;;; nelisp-sys-test.el ends here
