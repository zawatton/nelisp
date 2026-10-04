;;; gnu-31.1-rest.el --- GNU 31.1 materialized rest-argument fixtures -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(defun dialect-fixture-rest-only (&rest values)
  values)

(defun dialect-fixture-rest-required (required &rest values)
  values)

;;; gnu-31.1-rest.el ends here
