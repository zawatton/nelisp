;;; nelisp-stdlib-time-test.el --- ERT for Doc 87 §86.1.f time wrappers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 87 §86.1.f — `nl-current-unix-time' / `nl-format-unix-time'
;; migrated from Rust `bi_*' helpers to elisp wrappers on top of new
;; `nl_jit_current_unix_time' / `nl_jit_format_unix_time' trampolines
;; in `build-tool/src/jit/time.rs'.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-time-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-stdlib-time-test--bin
  (and nelisp-stdlib-time-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-time-test--repo-root)))

(defun nelisp-stdlib-time-test--skip-unless-built ()
  (unless (and nelisp-stdlib-time-test--bin
               (file-executable-p nelisp-stdlib-time-test--bin))
    (ert-skip "nelisp binary missing")))

(defun nelisp-stdlib-time-test--eval (expr-string)
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-time-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest nelisp-stdlib-time/current-unix-time-positive ()
  (nelisp-stdlib-time-test--skip-unless-built)
  (let ((r (nelisp-stdlib-time-test--eval "(nl-current-unix-time)")))
    (should (= 0 (car r)))
    ;; >= 1700000000 (= 2023-11-14 onward).
    (should (string-match-p "[12][0-9]\\{9\\}" (cdr r)))))

(ert-deftest nelisp-stdlib-time/format-unix-time-iso ()
  (nelisp-stdlib-time-test--skip-unless-built)
  (let ((r (nelisp-stdlib-time-test--eval
            "(nl-format-unix-time \"%Y-%m-%d\" 0)")))
    (should (= 0 (car r)))
    ;; Epoch 0 = 1970-01-01.
    (should (string-match-p "1970-01-01" (cdr r)))))

(ert-deftest nelisp-stdlib-time/format-unix-time-with-time-component ()
  (nelisp-stdlib-time-test--skip-unless-built)
  (let ((r (nelisp-stdlib-time-test--eval
            "(nl-format-unix-time \"%H:%M:%S\" 0)")))
    (should (= 0 (car r)))
    ;; Epoch 0 = 00:00:00 UTC.
    (should (string-match-p "00:00:00" (cdr r)))))

;;; nelisp-stdlib-time-test.el ends here
