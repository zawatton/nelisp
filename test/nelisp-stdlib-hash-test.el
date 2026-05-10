;;; nelisp-stdlib-hash-test.el --- ERT for Doc 87 §86.1.f secure-hash wrapper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 87 §86.1.f — `nl-secure-hash' migrated from Rust `bi_*' helper
;; to elisp wrapper on top of `nl_jit_secure_hash' trampoline in
;; `build-tool/src/jit/hash.rs'.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-hash-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-stdlib-hash-test--bin
  (and nelisp-stdlib-hash-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-hash-test--repo-root)))

(defun nelisp-stdlib-hash-test--skip-unless-built ()
  (unless (and nelisp-stdlib-hash-test--bin
               (file-executable-p nelisp-stdlib-hash-test--bin))
    (ert-skip "nelisp binary missing")))

(defun nelisp-stdlib-hash-test--eval (expr-string)
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-hash-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest nelisp-stdlib-hash/sha1-empty-string ()
  (nelisp-stdlib-hash-test--skip-unless-built)
  (let ((r (nelisp-stdlib-hash-test--eval "(nl-secure-hash 'sha1 \"\")")))
    (should (= 0 (car r)))
    ;; SHA-1 of empty string is a well-known constant.
    (should (string-match-p "da39a3ee5e6b4b0d3255bfef95601890afd80709" (cdr r)))))

(ert-deftest nelisp-stdlib-hash/sha256-empty-string ()
  (nelisp-stdlib-hash-test--skip-unless-built)
  (let ((r (nelisp-stdlib-hash-test--eval "(nl-secure-hash 'sha256 \"\")")))
    (should (= 0 (car r)))
    (should (string-match-p
             "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
             (cdr r)))))

(ert-deftest nelisp-stdlib-hash/md5-abc ()
  (nelisp-stdlib-hash-test--skip-unless-built)
  (let ((r (nelisp-stdlib-hash-test--eval "(nl-secure-hash 'md5 \"abc\")")))
    (should (= 0 (car r)))
    (should (string-match-p "900150983cd24fb0d6963f7d28e17f72" (cdr r)))))

(ert-deftest nelisp-stdlib-hash/sha512-abc ()
  (nelisp-stdlib-hash-test--skip-unless-built)
  (let ((r (nelisp-stdlib-hash-test--eval "(nl-secure-hash 'sha512 \"abc\")")))
    (should (= 0 (car r)))
    ;; First 16 chars suffice for fingerprint match.
    (should (string-match-p "ddaf35a193617aba" (cdr r)))))

;;; nelisp-stdlib-hash-test.el ends here
