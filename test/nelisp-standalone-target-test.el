;;; nelisp-standalone-target-test.el --- tests for standalone target selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-side checks for standalone target/ABI selection.  These guard the
;; Windows-native path against mixing Win64 object cache entries with the
;; existing Linux/SysV standalone cache.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (repo-root (and test-dir
                       (file-name-directory
                        (directory-file-name test-dir)))))
  (dolist (dir '("lisp" "src" "scripts"))
    (let ((path (and repo-root (expand-file-name dir repo-root))))
      (when (and path (file-directory-p path))
        (add-to-list 'load-path path)))))

(require 'nelisp-standalone-build)

(ert-deftest nelisp-standalone-target-defaults-to-linux-sysv ()
  "The default target remains Linux/SysV for compatibility on every host."
  (should (eq nelisp-standalone--target 'linux-x86_64))
  (should (eq (nelisp-standalone--target-abi 'linux-x86_64) 'sysv)))

(ert-deftest nelisp-standalone-target-windows-uses-win64 ()
  "The Windows-native target maps to the Microsoft x64 ABI."
  (should (eq (nelisp-standalone--target-abi 'windows-x86_64) 'win64)))

(ert-deftest nelisp-standalone-target-cache-is-target-qualified ()
  "Unit cache paths include the target name to avoid ABI mixing."
  (let ((base (file-name-as-directory nelisp-standalone--cache-dir)))
    (should (string-prefix-p
             base
             (nelisp-standalone--target-cache-dir 'linux-x86_64)))
    (should (string-suffix-p
             "linux-x86_64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'linux-x86_64))))
    (should (string-suffix-p
             "windows-x86_64"
             (directory-file-name
              (nelisp-standalone--target-cache-dir 'windows-x86_64))))))

(ert-deftest nelisp-standalone-target-rejects-unknown-target ()
  "Unsupported targets fail before producing a mixed-ABI object cache."
  (should-error (nelisp-standalone--target-abi 'plan9-x86_64)
                :type 'error))

(provide 'nelisp-standalone-target-test)

;;; nelisp-standalone-target-test.el ends here
