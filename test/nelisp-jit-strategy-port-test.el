;;; nelisp-jit-strategy-port-test.el --- ERT for Doc 28 §3.7.a.1  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1.7.a.1 (Doc 28 §3.7.a.1, 2026-05-10) — verify the 5 elisp
;; ports of the deleted Rust `bi_*' fns in `build-tool/src/jit/strategy.rs'
;; behave identically to the pre-port semantics.  Drives the runtime via
;; `nelisp eval' subprocess (= same pattern as `nelisp-jit-substrate-test.el').
;;
;; Coverage:
;;
;;   1. nelisp--int-eq-zero — strict-integer predicate; T for 0, Nil for
;;      non-zero Int, signals `wrong-type-argument' for non-Int.
;;
;;   2. nelisp--logior2 / -logand2 / -logxor2 — Int+Int → Int, Float
;;      coerces via the bridge `as_int', non-numeric raises WrongType.
;;
;;   3. ash — covers count ∈ [-62, +62] via the JIT trampoline + the
;;      explicit clamping path for count >= 63 / count <= -63 that
;;      mirrors the deleted `bi_ash_impl' bit-for-bit.

;;; Code:

(require 'ert)

(defconst nelisp-jit-strategy-port-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir))))

(defconst nelisp-jit-strategy-port-test--bin
  (and nelisp-jit-strategy-port-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-jit-strategy-port-test--repo-root)))

(defun nelisp-jit-strategy-port-test--skip-unless-built ()
  (unless (and nelisp-jit-strategy-port-test--bin
               (file-executable-p nelisp-jit-strategy-port-test--bin))
    (ert-skip
     (format "nelisp binary missing — run `cargo build --release' (looked at %s)"
             nelisp-jit-strategy-port-test--bin))))

(defun nelisp-jit-strategy-port-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return (EXIT-CODE . STDOUT)."
  (with-temp-buffer
    (let ((code (call-process nelisp-jit-strategy-port-test--bin nil t nil
                              "eval" expr-string)))
      (cons code (buffer-substring-no-properties (point-min) (point-max))))))

;; ---------- nelisp--int-eq-zero ----------------------------------

(ert-deftest jit-strategy-port-int-eq-zero-zero-returns-t ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--int-eq-zero 0)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\bt\\b" (cdr r)))))

(ert-deftest jit-strategy-port-int-eq-zero-nonzero-returns-nil ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--int-eq-zero 42)")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest jit-strategy-port-int-eq-zero-negative-returns-nil ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--int-eq-zero -7)")))
    (should (= 0 (car r)))
    (should (string-match-p "nil" (cdr r)))))

(ert-deftest jit-strategy-port-int-eq-zero-non-int-signals-wrong-type ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; Float is rejected — `bi_int_eq_zero' was strict-integer in Rust;
  ;; the elisp port preserves that contract via `(type-of x) = 'integer'.
  (let ((r (nelisp-jit-strategy-port-test--eval
            "(condition-case e (nelisp--int-eq-zero 1.5) (wrong-type-argument (princ (quote OK))))")))
    (should (= 0 (car r)))
    (should (string-match-p "OK" (cdr r)))))

(ert-deftest jit-strategy-port-int-eq-zero-symbol-signals-wrong-type ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval
            "(condition-case e (nelisp--int-eq-zero (quote foo)) (wrong-type-argument (princ (quote OK))))")))
    (should (= 0 (car r)))
    (should (string-match-p "OK" (cdr r)))))

;; ---------- bitwise: nelisp--logior2 / -logand2 / -logxor2 -------

(ert-deftest jit-strategy-port-logior2-int-int ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--logior2 12 3)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b15\\b" (cdr r)))))

(ert-deftest jit-strategy-port-logand2-int-int ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--logand2 14 7)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b6\\b" (cdr r)))))

(ert-deftest jit-strategy-port-logxor2-int-int ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--logxor2 12 10)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b6\\b" (cdr r)))))

(ert-deftest jit-strategy-port-logior2-with-float ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; Float coerces to i64 via the bridge `as_int' (= same semantics
  ;; as the deleted Rust `bi_logior2_impl' which called `as_int' on
  ;; both args).  1.7 → 1, 2.3 → 2, 1 | 2 = 3.
  (let ((r (nelisp-jit-strategy-port-test--eval "(nelisp--logior2 1.7 2.3)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b3\\b" (cdr r)))))

(ert-deftest jit-strategy-port-logand2-non-numeric-signals ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval
            "(condition-case e (nelisp--logand2 (quote a) 1) (wrong-type-argument (princ (quote OK))))")))
    (should (= 0 (car r)))
    (should (string-match-p "OK" (cdr r)))))

;; ---------- ash --------------------------------------------------

(ert-deftest jit-strategy-port-ash-positive-count-shifts-left ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash 1 3)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b8\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-negative-count-arithmetic-shift-right ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash 8 -3)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b1\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-negative-n-sign-extends ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; `-8 >> 3' = -1 (sign bits replicated), bit-for-bit match with
  ;; the deleted Rust `bi_ash_impl' / `nl_jit_arith_ash' contract.
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash -8 -3)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b-1\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-count-zero-identity ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash 42 0)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b42\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-count-out-of-range-positive ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; count >= 63 → result clamped to 0 (= deleted Rust
  ;; `bi_ash_impl''s `if count >= 63 { 0 }' branch).
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash 1 63)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b0\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-count-out-of-range-negative-positive-n ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; count <= -63 with n >= 0 → 0 (deleted Rust path:
  ;; `if abs >= 63 { if n < 0 { -1 } else { 0 } }').
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash 100 -63)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b0\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-count-out-of-range-negative-negative-n ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; count <= -63 with n < 0 → -1 (sign bits replicated infinitely).
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash -100 -63)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b-1\\b" (cdr r)))))

(ert-deftest jit-strategy-port-ash-boundary-count-62-jit-fast-path ()
  (nelisp-jit-strategy-port-test--skip-unless-built)
  ;; count = 62 stays in the JIT fast path (= [-62, +62] inclusive).
  ;; 1 << 62 = 4611686018427387904 (exactly 2^62).
  (let ((r (nelisp-jit-strategy-port-test--eval "(ash 1 62)")))
    (should (= 0 (car r)))
    (should (string-match-p "\\b4611686018427387904\\b" (cdr r)))))

;; (provide 'nelisp-jit-strategy-port-test) — omitted, not required for ERT batch.
;;; nelisp-jit-strategy-port-test.el ends here
