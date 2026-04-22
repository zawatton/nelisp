;;; nelisp-test.el --- Smoke tests for NeLisp scaffold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 1 Week 1-2 smoke test.  Confirms the package loads and exposes
;; the expected version constant.  Real interpreter tests arrive with
;; the reader and evaluator in Week 3+.

;;; Code:

(require 'ert)
(require 'nelisp)

(ert-deftest nelisp-test-package-loads ()
  "Package loads without error and provides `nelisp'."
  (should (featurep 'nelisp)))

(ert-deftest nelisp-test-version-semver ()
  "`nelisp-version' is a semver-shaped string."
  (should (stringp nelisp-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" nelisp-version)))

(provide 'nelisp-test)

;;; nelisp-test.el ends here
