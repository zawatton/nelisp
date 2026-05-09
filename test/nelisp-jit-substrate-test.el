;;; nelisp-jit-substrate-test.el --- ERT for Doc 80 substrate  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 80 Stage 80.2 (2026-05-09) — verify the pre-stdlib elisp
;; substrate (`lisp/nelisp-jit-substrate.el') correctly bootstraps
;; `cond' / `when' / `unless' / `null' / `not' / signal helpers.
;; Drives the runtime via `nelisp eval' subprocess (= same pattern as
;; `nelisp-stdlib-eval-special-test.el').

;;; Code:

(require 'ert)

(defconst nelisp-jit-substrate-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-jit-substrate-test--bin
  (and nelisp-jit-substrate-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-jit-substrate-test--repo-root)))

(defun nelisp-jit-substrate-test--skip-unless-built ()
  (unless (and nelisp-jit-substrate-test--bin
               (file-executable-p nelisp-jit-substrate-test--bin))
    (ert-skip
     (format "nelisp binary missing — run `cargo build --release' (looked at %s)"
             nelisp-jit-substrate-test--bin))))

(defun nelisp-jit-substrate-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-jit-substrate-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest substrate-cond-multi-arm-dispatch ()
  (nelisp-jit-substrate-test--skip-unless-built)
  ;; Doc 80 substrate `cond' picks the matching arm.
  (let ((r (nelisp-jit-substrate-test--eval
            "(cond ((eq 1 2) (quote a)) ((eq 1 1) (quote b)) (t (quote c)))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bb\\b" (cdr r)))))

(ert-deftest substrate-cond-fallthrough-to-t ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval
            "(cond ((eq 1 2) (quote no)) (t (quote yes)))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\byes\\b" (cdr r)))))

(ert-deftest substrate-cond-empty-clauses-returns-nil ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval "(cond)")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest substrate-when-true-evaluates-body ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval
            "(when (eq 1 1) (quote yes))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\byes\\b" (cdr r)))))

(ert-deftest substrate-when-false-returns-nil ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval
            "(when (eq 1 2) (quote yes))")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest substrate-unless-false-evaluates-body ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval
            "(unless (eq 1 2) (quote yes))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\byes\\b" (cdr r)))))

(ert-deftest substrate-null-on-nil-returns-t ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval "(null nil)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest substrate-null-on-symbol-returns-nil ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval "(null (quote a))")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest substrate-signal-wrong-type-catchable ()
  (nelisp-jit-substrate-test--skip-unless-built)
  ;; Doc 80 §2.3 — signal-helper preserves canonical sym/data shape;
  ;; condition-case with `wrong-type-argument' clause catches it.
  (let ((r (nelisp-jit-substrate-test--eval
            "(condition-case e \
              (nelisp--signal-wrong-type (quote sequencep) (quote bad)) \
              (wrong-type-argument (princ (car e)) (princ (quote OK))))")))
    (should (= 0 (car r)))
    (should (string-match-p "wrong-type-argumentOK" (cdr r)))))

(ert-deftest substrate-signal-arith-catchable ()
  (nelisp-jit-substrate-test--skip-unless-built)
  (let ((r (nelisp-jit-substrate-test--eval
            "(condition-case e \
              (nelisp--signal-arith (cons \"oor 5\" nil)) \
              (arith-error (princ (car e))))")))
    (should (= 0 (car r)))
    (should (string-match-p "arith-error" (cdr r)))))

(provide 'nelisp-jit-substrate-test)
;;; nelisp-jit-substrate-test.el ends here
