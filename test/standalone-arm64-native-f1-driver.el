;;; standalone-arm64-native-f1-driver.el --- Observed F1 acceptance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(unless (nelisp-native-load--arm64-p) (error "F1 requires the sealed Linux ARM64 target"))
(princ (format "ARM64-PAGE-BYTES=%d\n" (nelisp-native-linux-page-bytes)))
(load "test/support/native-entry-observer.el" nil t)
(load "test/standalone-bytecode-native-funcall-driver.el" nil t)
(let ((entries 0))
  (dolist (row (nelisp-test-native-entry-snapshot)) (setq entries (+ entries (cdr row))))
  (unless (> entries 0) (error "F1 made no native entry"))
  (princ (format "ARM64-NATIVE-ENTRIES=%d\n" entries)))
