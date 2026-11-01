;;; run-tests.el --- ERT runner honoring TYPECHECK_TEST_SELECTOR -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (f (directory-files dir t (or (getenv "TYPECHECK_TEST_FILES") "-test\\.el\\'")))
    (load f nil t)))
(let* ((s (or (getenv "TYPECHECK_TEST_SELECTOR") "t"))
       (selector (cond ((member s '("" "t")) t)
                       ((ert-test-boundp (intern s)) (intern s))
                       (t s))))
  ;; A selector that matches no test is a failure, not a pass.
  (when (null (ert-select-tests selector t))
    (message "run-tests: selector %S matches no test" s)
    (kill-emacs 2))
  (ert-run-tests-batch-and-exit selector))
