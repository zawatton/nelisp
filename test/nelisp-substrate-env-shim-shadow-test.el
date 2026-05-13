;;; nelisp-substrate-env-shim-shadow-test.el --- Doc 86 §86.3.b shadow ERT  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 86 §86.3.b / Doc 89 §4.2.2 — env shadow path verify ERT.
;;
;; Runs the nelisp binary (= released against the configured Cargo
;; feature set; default builds have the verify code compiled out, the
;; gated build emits the verify path) and exercises the 12 callsites
;; covered by §86.3.b: 9 Tier 3 dispatch arms (`symbol-value' / `set'
;; / `boundp' / `fboundp' / `symbol-function' / `fset' / `defalias' /
;; `makunbound' / `fmakunbound') plus 3 direct env callsites
;; (`set_value' from sf_setq + 2× `set_value' from
;; `eval_str_all_at_path' init).
;;
;; Goal of these tests under the shadow path: confirm that none of
;; the standard env operations triggers a divergence panic / log when
;; running the released binary.  Driven by the same `nelisp eval'
;; subprocess pattern as `nelisp-jit-substrate-test.el'.
;;
;; The cargo-side `eval::env::shadow_verify_tests' module covers the
;; synthetic-divergence panic path — that test exercises the verify
;; helper directly with bogus rust_result inputs.  The ERT below is
;; the smoke-side companion: every standard env op MUST stay clean.

;;; Code:

(require 'ert)

(defconst nelisp-substrate-env-shim-shadow-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-substrate-env-shim-shadow-test--bin
  (and nelisp-substrate-env-shim-shadow-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-substrate-env-shim-shadow-test--repo-root)))

(defun nelisp-substrate-env-shim-shadow-test--skip-unless-built ()
  "Skip the test when the released nelisp binary is missing."
  (unless (and nelisp-substrate-env-shim-shadow-test--bin
               (file-executable-p
                nelisp-substrate-env-shim-shadow-test--bin))
    (ert-skip
     (format "nelisp binary missing — run `cargo build --release' (looked at %s)"
             nelisp-substrate-env-shim-shadow-test--bin))))

(defun nelisp-substrate-env-shim-shadow-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process
                 nelisp-substrate-env-shim-shadow-test--bin
                 nil t nil "eval" expr-string)))
      (cons code (buffer-substring-no-properties
                  (point-min) (point-max))))))

(ert-deftest substrate-env-shim-shadow-set-then-symbol-value ()
  "Tier 3 `set' + `symbol-value' round-trip clean under shadow verify."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (set (quote doc-86-3b-x) 7) (symbol-value (quote doc-86-3b-x)))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b7\\b" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-boundp-after-set ()
  "Tier 3 `boundp' agrees with shim view of globals."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (set (quote doc-86-3b-y) 1) (boundp (quote doc-86-3b-y)))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-boundp-on-absent-is-nil ()
  "Tier 3 `boundp' returns nil for never-set symbol."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(boundp (quote doc-86-3b-never-set))")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-fboundp-after-fset ()
  "Tier 3 `fboundp' agrees with shim after `fset'."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (fset (quote doc-86-3b-fn) (lambda (x) x)) \
                    (fboundp (quote doc-86-3b-fn)))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-symbol-function-after-fset ()
  "Tier 3 `symbol-function' returns the installed lambda."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (fset (quote doc-86-3b-sf) (lambda (a) a)) \
                    (consp (symbol-function (quote doc-86-3b-sf))))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-makunbound-clears ()
  "Tier 3 `makunbound' clears the value cell + shim agrees."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (set (quote doc-86-3b-mb) 1) \
                    (makunbound (quote doc-86-3b-mb)) \
                    (boundp (quote doc-86-3b-mb)))")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-fmakunbound-clears ()
  "Tier 3 `fmakunbound' clears the function cell + shim agrees."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (fset (quote doc-86-3b-fmb) (lambda () 1)) \
                    (fmakunbound (quote doc-86-3b-fmb)) \
                    (fboundp (quote doc-86-3b-fmb)))")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-defalias-installs ()
  "Tier 3 `defalias' installs the function-cell value clean."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (defalias (quote doc-86-3b-da) (lambda (x) x)) \
                    (fboundp (quote doc-86-3b-da)))")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest substrate-env-shim-shadow-setq-via-mod-rs ()
  "Tier 3 `setq' (= sf_setq → env.set_value verify wrap) clean."
  (nelisp-substrate-env-shim-shadow-test--skip-unless-built)
  (let ((r (nelisp-substrate-env-shim-shadow-test--eval
            "(progn (setq doc-86-3b-sq 99) doc-86-3b-sq)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b99\\b" (cdr r)))))

(provide 'nelisp-substrate-env-shim-shadow-test)
;;; nelisp-substrate-env-shim-shadow-test.el ends here
