;;; nelisp-windows-os-compat-test.el --- tests for Windows OS compat script  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for tools/windows-os-compat-test.ps1.  These tests do not
;; run PowerShell; they verify that the real-machine Windows OS compatibility
;; ERT script exposes the intended suites and invokes the OS test file.

;;; Code:

(require 'ert)

(defconst nelisp-windows-os-compat-test--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for Windows OS compatibility script tests.")

(defun nelisp-windows-os-compat-test--read-file-text (path)
  "Return PATH as plain text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-windows-os-compat-test--count-windows-tests (text)
  "Return the number of ERT tests in TEXT whose names contain windows."
  (let ((pos 0)
        (count 0))
    (while (string-match "(ert-deftest[ \t\n]+[^ \t\n)]*windows" text pos)
      (setq count (1+ count))
      (setq pos (match-end 0)))
    count))

(ert-deftest nelisp-windows-os-compat-script-exposes-suites ()
  "The Windows OS compatibility script exposes stable focused suites."
  (let* ((script-path
          (expand-file-name "tools/windows-os-compat-test.ps1"
                            nelisp-windows-os-compat-test--repo-root))
         (script (nelisp-windows-os-compat-test--read-file-text script-path)))
    (dolist (suite '("all" "timerfd" "peercred" "socketpair" "inotify"
                     "signals" "eventfd" "fds" "process" "sockets"))
      (should (string-match-p
               (regexp-quote (format "Name = \"%s\"" suite))
               script)))
    (should (string-match-p
             (regexp-quote "Selector = \"timerfd-windows\"")
             script))
    (should (string-match-p
             (regexp-quote "Selector = \"getsockopt-peercred-windows\"")
             script))
    (should (string-match-p
             (regexp-quote "Selector = \"sigprocmask-windows\\|signalfd-windows\"")
             script))
    (should (string-match-p
             (regexp-quote "shutdown-windows")
             script))
    (should (string-match-p
             (regexp-quote "sendto-inet.*windows\\|recvfrom-inet.*windows")
             script))))

(ert-deftest nelisp-windows-os-compat-script-runs-ert-file ()
  "The Windows OS compatibility script invokes the OS ERT file by selector."
  (let* ((script-path
          (expand-file-name "tools/windows-os-compat-test.ps1"
                            nelisp-windows-os-compat-test--repo-root))
         (script (nelisp-windows-os-compat-test--read-file-text script-path)))
    (should (string-match-p
             (regexp-quote "-l test/nelisp-stdlib-os-test.el")
             script))
    (should (string-match-p
             (regexp-quote "NELISP_WINDOWS_OS_SELECTOR")
             script))
    (should (string-match-p
             (regexp-quote "ert-run-tests-batch-and-exit")
             script))
    (should (string-match-p
             (regexp-quote "[windows-os] all PASS")
             script))))

(ert-deftest nelisp-windows-os-compat-script-checks-all-test-count ()
  "The all-suite runner fails when the windows selector silently skips tests."
  (let* ((script-path
          (expand-file-name "tools/windows-os-compat-test.ps1"
                            nelisp-windows-os-compat-test--repo-root))
         (test-path
          (expand-file-name "test/nelisp-stdlib-os-test.el"
                            nelisp-windows-os-compat-test--repo-root))
         (script (nelisp-windows-os-compat-test--read-file-text script-path))
         (test-text (nelisp-windows-os-compat-test--read-file-text test-path))
         (count (nelisp-windows-os-compat-test--count-windows-tests test-text)))
    (should (= count 274))
    (should (string-match-p
             (regexp-quote "ExpectedCount = 274")
             script))
    (should (string-match-p
             (regexp-quote "Ran ([0-9]+) tests")
             script))
    (should (string-match-p
             (regexp-quote "expected at least")
             script))
    (should (string-match-p
             (regexp-quote "$SuiteItem.ContainsKey(\"ExpectedCount\")")
             script))))

(provide 'nelisp-windows-os-compat-test)

;;; nelisp-windows-os-compat-test.el ends here
