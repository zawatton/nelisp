;;; nelisp-stdlib-regex-test.el --- ERT for Doc 87 §86.1.f regex wrapper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 87 §86.1.f — `string-match-p' migrated from Rust `bi_*' helper
;; to elisp wrapper on top of `nl_jit_string_match_p' trampoline in
;; `build-tool/src/jit/regex.rs'.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-regex-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-stdlib-regex-test--bin
  (and nelisp-stdlib-regex-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-regex-test--repo-root)))

(defun nelisp-stdlib-regex-test--skip-unless-built ()
  (unless (and nelisp-stdlib-regex-test--bin
               (file-executable-p nelisp-stdlib-regex-test--bin))
    (ert-skip "nelisp binary missing")))

(defun nelisp-stdlib-regex-test--eval (expr-string)
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-regex-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest nelisp-stdlib-regex/decimal-positive-match ()
  (nelisp-stdlib-regex-test--skip-unless-built)
  (let ((r (nelisp-stdlib-regex-test--eval
            "(string-match-p \"\\\\`-?[0-9]+\\\\(\\\\.[0-9]+\\\\)?\\\\'\" \"42\")")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest nelisp-stdlib-regex/decimal-negative-match ()
  (nelisp-stdlib-regex-test--skip-unless-built)
  (let ((r (nelisp-stdlib-regex-test--eval
            "(string-match-p \"\\\\`-?[0-9]+\\\\(\\\\.[0-9]+\\\\)?\\\\'\" \"-3.14\")")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest nelisp-stdlib-regex/decimal-mismatch-on-letters ()
  (nelisp-stdlib-regex-test--skip-unless-built)
  (let ((r (nelisp-stdlib-regex-test--eval
            "(string-match-p \"\\\\`-?[0-9]+\\\\(\\\\.[0-9]+\\\\)?\\\\'\" \"abc\")")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bnil\\b" (cdr r)))))

(ert-deftest nelisp-stdlib-regex/ipv4-pattern ()
  (nelisp-stdlib-regex-test--skip-unless-built)
  (let ((r (nelisp-stdlib-regex-test--eval
            "(string-match-p \"\\\\`[0-9]+\\\\.[0-9]+\\\\.[0-9]+\\\\.[0-9]+\\\\'\" \"127.0.0.1\")")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest nelisp-stdlib-regex/whitespace-only-string ()
  (nelisp-stdlib-regex-test--skip-unless-built)
  (let ((r (nelisp-stdlib-regex-test--eval
            "(string-match-p \"^[[:space:]]*$\" \"   \")")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

;;; nelisp-stdlib-regex-test.el ends here
