;;; nelisp-stdlib-math-test.el --- ERT for Doc 87 §86.1.f math wrappers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 87 §86.1.f — `float' / `exp' / `log' migrated from Rust `bi_*'
;; helpers to elisp wrappers on top of the new
;; `:trampoline-unary-float' ABI mode.  These end-to-end ERTs drive
;; the wrappers through the real `bin/nelisp' binary (= same pattern
;; as `test/nelisp-stdlib-os-test.el').  Tests skip when the binary
;; is missing so a `make test' without prior `cargo build --release
;; --bin nelisp' degrades quietly.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-math-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir)))
  "Absolute path to the dev/nelisp worktree, or nil when undeterminable.")

(defconst nelisp-stdlib-math-test--bin
  (and nelisp-stdlib-math-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-math-test--repo-root)))

(defun nelisp-stdlib-math-test--skip-unless-built ()
  (unless (and nelisp-stdlib-math-test--bin
               (file-executable-p nelisp-stdlib-math-test--bin))
    (ert-skip
     (format "nelisp binary missing — run `cargo build --release' (looked at %s)"
             nelisp-stdlib-math-test--bin))))

(defun nelisp-stdlib-math-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-math-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest nelisp-stdlib-math/float-int-promotes-to-float ()
  (nelisp-stdlib-math-test--skip-unless-built)
  (let ((r (nelisp-stdlib-math-test--eval "(float 3)")))
    (should (= 0 (car r)))
    (should (string-match-p "3\\.0" (cdr r)))))

(ert-deftest nelisp-stdlib-math/float-float-identity ()
  (nelisp-stdlib-math-test--skip-unless-built)
  (let ((r (nelisp-stdlib-math-test--eval "(float 2.5)")))
    (should (= 0 (car r)))
    (should (string-match-p "2\\.5" (cdr r)))))

(ert-deftest nelisp-stdlib-math/exp-zero ()
  (nelisp-stdlib-math-test--skip-unless-built)
  (let ((r (nelisp-stdlib-math-test--eval "(exp 0)")))
    (should (= 0 (car r)))
    (should (string-match-p "1\\(\\.0*\\)?" (cdr r)))))

(ert-deftest nelisp-stdlib-math/log-one-is-zero ()
  (nelisp-stdlib-math-test--skip-unless-built)
  (let ((r (nelisp-stdlib-math-test--eval "(log 1)")))
    (should (= 0 (car r)))
    ;; `(log 1)' = 0.0 (via natural log).
    (should (string-match-p "\\b0\\(\\.0+\\)?\\b" (cdr r)))))

(ert-deftest nelisp-stdlib-math/log-with-base-decompose ()
  (nelisp-stdlib-math-test--skip-unless-built)
  (let ((r (nelisp-stdlib-math-test--eval "(log 8 2)")))
    (should (= 0 (car r)))
    ;; `(log 8 2)' = 3.0 within float epsilon.
    (should (string-match-p "3" (cdr r)))))

;;; nelisp-stdlib-math-test.el ends here
