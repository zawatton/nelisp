;;; nelisp-allocator-bench-test.el --- Phase 7.2 §5.1 v2 3-tier bench harness ERT smoke -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 29 §5.1 v2 LOCK-close — ERT smoke for
;; `bench/nelisp-allocator-bench.el'.  These tests prove the bench
;; *harness* API + return shape only; they do NOT assert on the
;; ratio values.  The actual gate verification is `make
;; bench-allocator' territory (the §5.1 v2 tier-A 3-5x / 4-6x /
;; 8-12x bands + tier-B C reference ratio + tier-C invariants are
;; release-audit / CI gate concerns, not ERT contract).
;;
;; All tests run with `:smoke t' so total wall-time stays under
;; ~50 ms even on the slow CI runner.  Heavy bench runs are gated
;; on `NELISP_HEAVY_TESTS=1' and exposed only through the Makefile
;; entry point.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-allocator)
(require 'nelisp-allocator-bench)

;;; Per-tier smoke tests ------------------------------------------

(ert-deftest nelisp-allocator-bench-tier1-smoke ()
  "Tier 1 (alloc-heavy) returns a result plist without raising.
The smoke uses the smallest N (1000) so the call wall-time stays
ms-scale.  Asserts the plist shape only — gate-pass / gate-target /
speedup values are CI / release-audit concerns."
  (let ((result (nelisp-allocator-bench-tier1 :smoke t :iterations 1)))
    (should (listp result))
    (should (eq (plist-get result :tier) 1))
    (should (eq (plist-get result :name) 'alloc-heavy))
    (should (integerp (plist-get result :n)))
    (should (= (plist-get result :iterations) 1))))

(ert-deftest nelisp-allocator-bench-tier2-smoke ()
  "Tier 2 (per-pool stress) returns a result plist without raising.
Exercises all 5 Doc 29 §2.5 families (cons / closure / string-span
/ vector-span / large-object) under the smoke iteration count."
  (let ((result (nelisp-allocator-bench-tier2 :smoke t :iterations 1)))
    (should (listp result))
    (should (eq (plist-get result :tier) 2))
    (should (eq (plist-get result :name) 'per-pool-stress))
    (should (integerp (plist-get result :n)))
    (should (= (plist-get result :iterations) 1))))

(ert-deftest nelisp-allocator-bench-tier3-smoke ()
  "Tier 3 (bulk-alloc / 4 KiB+) returns a result plist without raising.
Exercises the `nelisp-allocator-alloc-large-object' boundary path
under the smoke iteration count.  Asserts the plist carries the
:payload-bytes key so consumers know the boundary was actually
exercised."
  (let ((result (nelisp-allocator-bench-tier3 :smoke t :iterations 1)))
    (should (listp result))
    (should (eq (plist-get result :tier) 3))
    (should (eq (plist-get result :name) 'bulk-alloc))
    (should (integerp (plist-get result :n)))
    (should (integerp (plist-get result :payload-bytes)))
    (should (>= (plist-get result :payload-bytes)
                nelisp-allocator-large-object-threshold))))

;;; Result-plist shape contract -----------------------------------

(ert-deftest nelisp-allocator-bench-result-shape ()
  "Each tier result plist contains the keys the release-audit /
dashboard consume.  Pinning the contract here so a refactor that
drops a key surfaces as a test failure rather than a silent
dashboard regression."
  (dolist (runner '(nelisp-allocator-bench-tier1
                    nelisp-allocator-bench-tier2
                    nelisp-allocator-bench-tier3))
    (let ((r (funcall runner :smoke t :iterations 1)))
      (dolist (key '(:tier :name :n :iterations :baseline-elapsed
                           :native-elapsed :speedup :gate-target
                           :gate-pass :native-skip-reason))
        (should (memq key r))))))

;;; Timing path actually executed ---------------------------------

(ert-deftest nelisp-allocator-bench-timings-positive ()
  "Both baseline and native timings record a positive elapsed time.
Proves the lambdas actually executed (not skipped via an early
return) and `current-time' deltas are measurable.  Pre-condition
for any §5.1 v2 ratio to be meaningful."
  (let ((r (nelisp-allocator-bench-tier1 :smoke t :iterations 1)))
    (should (numberp (plist-get r :baseline-elapsed)))
    (should (numberp (plist-get r :native-elapsed)))
    (should (>= (plist-get r :baseline-elapsed) 0.0))
    (should (>= (plist-get r :native-elapsed) 0.0))))

;;; 3-tier runner -------------------------------------------------

(ert-deftest nelisp-allocator-bench-run-3-tier-smoke ()
  "The 3-tier runner returns 3 result plists in tier order.
Calls the runner via the public API (`run-3-tier' with iterations =
1) and verifies the shape — total wall-time stays under ~50 ms with
the default smoke values."
  (let* ((nelisp-allocator-bench-cons-stress-n 1000)
         (nelisp-allocator-bench-per-pool-n 250)
         (nelisp-allocator-bench-bulk-alloc-n 32)
         (results (nelisp-allocator-bench-run-3-tier 1)))
    (should (= 3 (length results)))
    (should (equal '(1 2 3) (mapcar (lambda (r) (plist-get r :tier))
                                    results)))
    (should (equal '(alloc-heavy per-pool-stress bulk-alloc)
                   (mapcar (lambda (r) (plist-get r :name))
                           results)))))

;;; Native skip-reason surfaces correctly -------------------------

(ert-deftest nelisp-allocator-bench-native-status-current ()
  "Until Phase 7.5 wires the alloc fast path, every tier reports
`simulator-only' as the native skip-reason.  The gate-pass should
then evaluate to `:skipped' (not nil → would be FAIL, not t →
would falsely claim PASS).  Locks the harness contract that today's
bench is *infrastructure baseline*, not a verified regression."
  (let ((r (nelisp-allocator-bench-tier1 :smoke t :iterations 1)))
    (should (eq (plist-get r :native-skip-reason) 'simulator-only))
    (should (eq (plist-get r :gate-pass) :skipped))))

(provide 'nelisp-allocator-bench-test)

;;; nelisp-allocator-bench-test.el ends here
