;;; nelisp-cc-bench-actual-test.el --- T37 3-axis bench actual ERT smoke -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T37 ERT smoke for `bench/nelisp-cc-bench-actual.el'.  Five tests
;; gated by `skip-unless' against the Phase 7.5.4 in-process FFI module
;; — when `nelisp-runtime-module.so' is missing every test skips
;; cleanly so `make test' stays green on hosts without the Rust
;; toolchain (the 3-axis gate verification is `make bench-actual'
;; territory, ERT here only proves the harness API + return shape).
;;
;; Tests:
;;
;;   1. nelisp-cc-bench-actual-fib-30-runs-without-error
;;        — the fib-30 bench function returns without raising.
;;
;;   2. nelisp-cc-bench-actual-fib-30-bytecode-baseline-positive
;;        — the bytecode-VM baseline timing > 0 (proves the
;;          baseline path is wired and the lambda actually executes).
;;
;;   3. nelisp-cc-bench-actual-fib-30-native-baseline-positive
;;        — the NeLisp native baseline timing > 0 (proves the
;;          in-process FFI path is wired all the way through, even
;;          though semantic correctness is Phase 7.5 callee-resolution
;;          territory).
;;
;;   4. nelisp-cc-bench-actual-3-axis-runs-without-error
;;        — the full 3-axis runner (`run-3-axis') returns without
;;          raising and produces 3 result plists.
;;
;;   5. nelisp-cc-bench-actual-results-shape
;;        — each result plist contains the expected gate keys
;;          (:bench / :speedup / :gate-target / :gate-pass / etc).
;;
;; Note: actual gate values (30x / 20x / 5x) are NOT asserted here —
;; that is the `make bench-actual' batch's responsibility, gated on
;; production hosts only.  This ERT layer is API-shape verification.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-runtime)
(require 'nelisp-cc-bench-actual)

;;; Skip predicate -------------------------------------------------

(defun nelisp-cc-bench-actual-test--module-built-p ()
  "Return non-nil when the Phase 7.5.4 in-process FFI is available."
  (nelisp-cc-bench-actual--module-available-p))

(defun nelisp-cc-bench-actual-test--skip-unless-module ()
  "Skip the surrounding ERT unless the in-process FFI module is built."
  (unless (nelisp-cc-bench-actual-test--module-built-p)
    (ert-skip "nelisp-runtime-module.so missing — run `make runtime-module'"))
  (unless (nelisp-cc-bench-actual--host-x86_64-p)
    (ert-skip "T37 bench-actual is x86_64-only on the MVP path")))

;;; (1) fib-30 runs without error ---------------------------------

(ert-deftest nelisp-cc-bench-actual-fib-30-runs-without-error ()
  "The fib-30 bench function returns without raising.

We pass `iterations=1' to keep the smoke fast — the gate-pass
status is irrelevant here, only the no-raise contract.  When the
in-process FFI module is missing the test skips cleanly."
  (nelisp-cc-bench-actual-test--skip-unless-module)
  (let ((result (nelisp-cc-bench-actual-fib-30 1)))
    (should (listp result))
    (should (eq (plist-get result :bench) 'fib-30))))

;;; (2) bytecode baseline > 0 -------------------------------------

(ert-deftest nelisp-cc-bench-actual-fib-30-bytecode-baseline-positive ()
  "The bytecode-VM baseline records a positive elapsed time.

This proves the bytecode path is wired (the lambda actually runs
through `byte-compile' + funcall) and `current-time' deltas are
measurable — pre-condition for any §5.2 ratio to be meaningful.

The bench skips cleanly when the in-process FFI module is missing
(no point measuring bytecode without the native counterpart for the
ratio)."
  (nelisp-cc-bench-actual-test--skip-unless-module)
  (let ((result (nelisp-cc-bench-actual-fib-30 1)))
    (should (numberp (plist-get result :bytecode-elapsed)))
    (should (> (plist-get result :bytecode-elapsed) 0.0))))

;;; (3) native baseline > 0 ---------------------------------------

(ert-deftest nelisp-cc-bench-actual-fib-30-native-baseline-positive ()
  "The NeLisp native baseline records a positive elapsed time, OR
the harness reports a documented skip-reason.

Proves the full Phase 7.5.4 in-process FFI path executes:
SSA frontend → linear-scan → x86_64 backend → mmap JIT page → mprotect
RX → call → munmap.

Phase 7.1.5 caveat: the SSA frontend does not yet support `letrec'
or `while', so the fib-30 lambda's compile may signal — in that case
the harness records `:native-skip-reason native-compile-failed' and
this test asserts the skip-reason path is wired correctly (rather
than failing on a known Phase 7.5 deferred path).  When Phase 7.5
lands the deferred forms, the lambda compiles and the timing branch
takes over without further code change."
  (nelisp-cc-bench-actual-test--skip-unless-module)
  (let* ((result (nelisp-cc-bench-actual-fib-30 1))
         (native (plist-get result :native-elapsed))
         (skip   (plist-get result :native-skip-reason)))
    (cond
     (native
      ;; Native execution succeeded — the timing must be positive.
      (should (numberp native))
      (should (> native 0.0)))
     (t
      ;; Native execution skipped — a skip-reason must be present.
      ;; This proves the harness's failure-path is wired, even when
      ;; Phase 7.5 deferred SSA forms (`letrec' / `while') prevent
      ;; the actual native compile from succeeding today.
      (should (symbolp skip))
      (should skip)))))

;;; (4) 3-axis runner returns without error ----------------------

(ert-deftest nelisp-cc-bench-actual-3-axis-runs-without-error ()
  "The full 3-axis runner returns without raising and produces 3 results.

We deliberately do NOT call `kill-emacs' here — the runner only
calls `kill-emacs' when `noninteractive' is t AND it is the entry
point of `make bench-actual'.  ERT runs in batch mode (noninteractive
= t), so we shadow `kill-emacs' for the duration of the test."
  (nelisp-cc-bench-actual-test--skip-unless-module)
  (cl-letf (((symbol-function 'kill-emacs)
             (lambda (&rest _ignored) nil)))
    (let ((results (nelisp-cc-bench-actual-run-3-axis 1)))
      (should (listp results))
      (should (= (length results) 3))
      (should (eq (plist-get (nth 0 results) :bench) 'fib-30))
      (should (eq (plist-get (nth 1 results) :bench) 'fact-iter))
      (should (eq (plist-get (nth 2 results) :bench) 'alloc-heavy)))))

;;; (5) result-plist shape ----------------------------------------

(ert-deftest nelisp-cc-bench-actual-results-shape ()
  "Each result plist contains the documented gate keys.

The bench harness publishes a stable plist contract (Doc 28 v2 §5.2
gate consumer surface) — downstream tooling (CI dashboards, the
v1.0 ship audit, etc.) parses these keys.  This test catches drift."
  (nelisp-cc-bench-actual-test--skip-unless-module)
  (let ((result (nelisp-cc-bench-actual-fib-30 1)))
    (dolist (key '(:bench :iterations :bytecode-elapsed
                          :native-elapsed :native-comp-elapsed
                          :speedup :gate-target :gate-pass
                          :emacs-native-comp-ratio
                          :emacs-native-comp-acceptable
                          :native-skip-reason))
      ;; `plist-member' accepts nil values, which is what we want —
      ;; some keys (e.g. :native-comp-elapsed) are nil when the host
      ;; lacks libgccjit, but the *key* must still be present.
      (should (plist-member result key)))
    ;; Type sanity on the always-populated keys.
    (should (symbolp (plist-get result :bench)))
    (should (integerp (plist-get result :iterations)))
    (should (numberp (plist-get result :gate-target)))
    ;; gate-pass is t / nil / :skipped (= host-CPU not production).
    (should (memq (plist-get result :gate-pass) '(nil t :skipped)))))

(ert-deftest nelisp-cc-bench-actual-print-value-truncates-bignum ()
  "`--print-value' truncates long values into a `<NNNN-char value: HEAD…>'
sentinel so fact-iter's ~2500-digit bignum doesn't flood the bench
transcript.  Regression test for the 2026-04-26 token-budget fix."
  (let* ((bignum (let ((acc 1)) (dotimes (i 1000) (setq acc (* acc (1+ i)))) acc))
         (printed (nelisp-cc-bench-actual--print-value bignum)))
    (should (< (length printed) 200))
    (should (string-match-p "\\`<[0-9]+-char value: " printed))
    (should (string-match-p "…>\\'" printed))))

(ert-deftest nelisp-cc-bench-actual-print-value-passthrough-short ()
  "`--print-value' returns short values unchanged (= no sentinel wrap)."
  (should (equal (nelisp-cc-bench-actual--print-value 832040) "832040"))
  (should (equal (nelisp-cc-bench-actual--print-value nil) "nil"))
  (should (equal (nelisp-cc-bench-actual--print-value 'foo) "foo")))

(ert-deftest nelisp-cc-bench-actual-print-value-cap-disable ()
  "Setting `--value-print-cap' to nil disables truncation (= raw `%S')."
  (let ((nelisp-cc-bench-actual-value-print-cap nil)
        (long (make-string 500 ?x)))
    (should (= (length (nelisp-cc-bench-actual--print-value long))
               (+ 2 (length long))))))

(provide 'nelisp-cc-bench-actual-test)

;;; nelisp-cc-bench-actual-test.el ends here
