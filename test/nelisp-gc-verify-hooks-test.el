;;; nelisp-gc-verify-hooks-test.el --- ERTs for §6.10 oracle wire-up -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT smoke tests for `nelisp-gc-verify-hooks':
;;
;;   1. Hooks fire when `nelisp-gc-verify-debug-mode' is on
;;      (per-event counter increments through `run-sweep-phase',
;;      `run-minor-gc-with-promotion', `flip-semispace',
;;      `run-mark-phase' with `-double-scan' opt-in).
;;   2. Hooks do *not* fire when debug-mode is off (counters stay 0).
;;   3. Synthetic violations are caught + logged (severity = `log').
;;   4. Severity = `signal' surfaces the violation as an `error'.
;;   5. The recursion guard prevents the double-scan oracle from
;;      re-triggering itself when the inner mark phase runs.
;;   6. The ring-buffer cap drops oldest entries.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-gc-inner)
(require 'nelisp-gc-verify)
(require 'nelisp-gc-verify-hooks)


;;; Helpers ---------------------------------------------------------

(defun nelisp-gc-verify-hooks-test--reset (&optional debug-on)
  "Reset GC + verify state for an isolated test.
DEBUG-ON non-nil installs the advice; nil leaves it disabled."
  (nelisp-gc-inner-init-mark-state)
  (nelisp-gc-inner--reset-free-lists)
  (nelisp-gc-verify-poison-reset)
  (nelisp-gc-verify-debug-log-clear)
  (nelisp-gc-verify-hooks-disable)
  (setq nelisp-gc-verify-debug-mode nil
        nelisp-gc-verify-debug-double-scan nil
        nelisp-gc-verify-debug-severity 'log)
  (when debug-on
    (setq nelisp-gc-verify-debug-mode t)
    (nelisp-gc-verify-hooks-enable)))

(defun nelisp-gc-verify-hooks-test--mk-clean-region ()
  "A trivially-valid Doc 29 §1.4 region with no objects."
  (list :region-id 1
        :start 0
        :end 4096
        :generation :tenured
        :family :cons-pool
        :objects nil))


;;; 1. Hook firing while debug-mode is ON ---------------------------

(ert-deftest nelisp-gc-verify-hooks-sweep-fires-when-debug-on ()
  "Sweep advice runs `verify-heap' once per `run-sweep-phase' call."
  (nelisp-gc-verify-hooks-test--reset t)
  (unwind-protect
      (let ((rs (list (nelisp-gc-verify-hooks-test--mk-clean-region))))
        (nelisp-gc-inner-run-sweep-phase rs)
        (should (= 1 (nelisp-gc-verify-debug-event-count 'verify-heap))))
    (nelisp-gc-verify-hooks-test--reset)))

(ert-deftest nelisp-gc-verify-hooks-flip-fires-when-debug-on ()
  "Flip advice runs `verify-poison-from-space' once per flip."
  (nelisp-gc-verify-hooks-test--reset t)
  (unwind-protect
      (let* ((from (list :region-id 1 :start 0     :end 4096
                         :generation :nursery :family :cons-pool))
             (to   (list :region-id 2 :start 4096  :end 8192
                         :generation :nursery :family :cons-pool))
             (semi (nelisp-gc-inner-init-semispace from to)))
        (nelisp-gc-inner-flip-semispace semi)
        (should (= 1 (nelisp-gc-verify-debug-event-count
                      'verify-poison-from-space))))
    (nelisp-gc-verify-hooks-test--reset)))

(ert-deftest nelisp-gc-verify-hooks-double-scan-opt-in ()
  "Double-scan advice fires only when *both* defcustom flags are on."
  (nelisp-gc-verify-hooks-test--reset t)
  (unwind-protect
      (let ((rs (list (nelisp-gc-verify-hooks-test--mk-clean-region))))
        ;; debug-double-scan still off → mark phase runs, no oracle fire.
        (nelisp-gc-inner-run-mark-phase nil rs)
        (should (= 0 (nelisp-gc-verify-debug-event-count
                      'verify-double-scan)))
        ;; Flip the second flag on; next mark triggers the oracle.
        (setq nelisp-gc-verify-debug-double-scan t)
        (nelisp-gc-inner-run-mark-phase nil rs)
        (should (= 1 (nelisp-gc-verify-debug-event-count
                      'verify-double-scan))))
    (nelisp-gc-verify-hooks-test--reset)))


;;; 2. Hooks DO NOT fire when debug-mode is OFF --------------------

(ert-deftest nelisp-gc-verify-hooks-sweep-silent-when-debug-off ()
  "With debug-mode nil, sweep runs but the oracle does NOT fire."
  (nelisp-gc-verify-hooks-test--reset nil)
  (let ((rs (list (nelisp-gc-verify-hooks-test--mk-clean-region))))
    (nelisp-gc-inner-run-sweep-phase rs)
    (should (= 0 (nelisp-gc-verify-debug-event-count 'verify-heap)))
    (should (zerop (nelisp-gc-verify-debug-log-count)))))

(ert-deftest nelisp-gc-verify-hooks-flip-silent-when-debug-off ()
  "With debug-mode nil, flip runs but verify-poison oracle does NOT fire."
  (nelisp-gc-verify-hooks-test--reset nil)
  (let* ((from (list :region-id 1 :start 0     :end 4096
                     :generation :nursery :family :cons-pool))
         (to   (list :region-id 2 :start 4096  :end 8192
                     :generation :nursery :family :cons-pool))
         (semi (nelisp-gc-inner-init-semispace from to)))
    (nelisp-gc-inner-flip-semispace semi)
    (should (= 0 (nelisp-gc-verify-debug-event-count
                  'verify-poison-from-space)))
    (should (zerop (nelisp-gc-verify-debug-log-count)))))

(ert-deftest nelisp-gc-verify-hooks-mark-silent-when-debug-off ()
  "With debug-mode nil, even with double-scan flag on no oracle fires."
  (nelisp-gc-verify-hooks-test--reset nil)
  (let ((nelisp-gc-verify-debug-double-scan t)
        (rs (list (nelisp-gc-verify-hooks-test--mk-clean-region))))
    (nelisp-gc-inner-run-mark-phase nil rs)
    (should (= 0 (nelisp-gc-verify-debug-event-count
                  'verify-double-scan)))))


;;; 3. Synthetic violations are caught + logged --------------------

(ert-deftest nelisp-gc-verify-hooks-sweep-logs-malformed-region ()
  "Sweep on a malformed region surfaces a `verify-heap' violation."
  (nelisp-gc-verify-hooks-test--reset t)
  (unwind-protect
      (let* ((bogus-region
              (list :region-id 1 :start 0 :end 4096
                    :generation :tenured
                    :family :cons-pool
                    ;; Object overlaps itself end-out-of-bounds → catches.
                    :objects '((100 . 8000))))
             ;; Must be a valid `region-p' so the sweep itself accepts
             ;; it, only the verify oracle should reject the geometry.
             (rs (list bogus-region)))
        (nelisp-gc-inner-run-sweep-phase rs)
        (should (= 1 (nelisp-gc-verify-debug-event-count 'verify-heap)))
        (should (>= (nelisp-gc-verify-debug-log-count) 1))
        (let* ((rec (car nelisp-gc-verify-debug-log))
               (v (plist-get rec :violation)))
          (should (eq (plist-get rec :event) 'verify-heap))
          (should (eq (plist-get v :severity) 'fatal))))
    (nelisp-gc-verify-hooks-test--reset)))

(ert-deftest nelisp-gc-verify-hooks-flip-logs-poison-touch ()
  "After a flip, a recorded poison touch becomes a logged violation."
  (nelisp-gc-verify-hooks-test--reset t)
  (unwind-protect
      (let* ((from (list :region-id 20 :start 0     :end 4096
                         :generation :nursery :family :cons-pool))
             (to   (list :region-id 21 :start 4096  :end 8192
                         :generation :nursery :family :cons-pool))
             (semi (nelisp-gc-inner-init-semispace from to)))
        ;; Poison the about-to-be-dead semispace half *before* flip,
        ;; record a forbidden touch, then flip → oracle fires + logs.
        (nelisp-gc-verify-poison-mark from)
        (nelisp-gc-verify-poison-touch 100 'simulated-bug)
        (nelisp-gc-inner-flip-semispace semi)
        (should (= 1 (nelisp-gc-verify-debug-event-count
                      'verify-poison-from-space)))
        (should (= 1 (nelisp-gc-verify-debug-log-count)))
        (let* ((rec (car nelisp-gc-verify-debug-log))
               (v (plist-get rec :violation)))
          (should (eq (plist-get rec :event) 'verify-poison-from-space))
          (should (eq (plist-get v :region-id) 20))))
    (nelisp-gc-verify-hooks-test--reset)))

(ert-deftest nelisp-gc-verify-hooks-double-scan-logs-divergence ()
  "Non-deterministic walker → double-scan oracle logs the divergence.

The hook runs the oracle *after* the outer `run-mark-phase' that
consumes the first walker invocation, so we burn the first call
on a stable response and have the inner two re-runs (calls #2/#3)
yield divergent children — that is the pair the oracle compares."
  (nelisp-gc-verify-hooks-test--reset t)
  (setq nelisp-gc-verify-debug-double-scan t)
  (unwind-protect
      (let* ((calls (cons 0 nil))
             (walker (lambda (_addr)
                       (cl-incf (car calls))
                       ;; Outer (call #1): stable.  Inner pair (calls
                       ;; #2/#3+): diverge so the oracle sees the
                       ;; mark-state mismatch.
                       (cond
                        ((= (car calls) 1) nil)
                        ((= (car calls) 2) '(200))
                        (t '(300)))))
             (rs (list (list :region-id 1 :start 0 :end 1024
                             :generation :tenured :family :cons-pool
                             :children-of walker))))
        (nelisp-gc-inner-run-mark-phase '(100) rs)
        (should (= 1 (nelisp-gc-verify-debug-event-count
                      'verify-double-scan)))
        (should (>= (nelisp-gc-verify-debug-log-count) 1))
        (let* ((rec (car nelisp-gc-verify-debug-log))
               (v (plist-get rec :violation)))
          (should (eq (plist-get rec :event) 'verify-double-scan))
          (should (eq (plist-get v :severity) 'fatal))))
    (nelisp-gc-verify-hooks-test--reset)))


;;; 4. Severity = signal surfaces an error -------------------------

(ert-deftest nelisp-gc-verify-hooks-severity-signal-aborts ()
  "When severity is `signal', a violation `error's instead of just logging."
  (nelisp-gc-verify-hooks-test--reset t)
  (setq nelisp-gc-verify-debug-severity 'signal)
  (unwind-protect
      (let* ((bogus-region
              (list :region-id 1 :start 0 :end 4096
                    :generation :tenured :family :cons-pool
                    :objects '((100 . 8000))))
             (rs (list bogus-region)))
        (should-error (nelisp-gc-inner-run-sweep-phase rs)
                      :type 'error))
    (nelisp-gc-verify-hooks-test--reset)))


;;; 5. Recursion guard --------------------------------------------

(ert-deftest nelisp-gc-verify-hooks-double-scan-recursion-guarded ()
  "Inner re-run of mark-phase from the oracle does NOT bump the counter again."
  (nelisp-gc-verify-hooks-test--reset t)
  (setq nelisp-gc-verify-debug-double-scan t)
  (unwind-protect
      (let ((rs (list (nelisp-gc-verify-hooks-test--mk-clean-region))))
        (nelisp-gc-inner-run-mark-phase nil rs)
        ;; The oracle re-runs mark-phase twice internally, but the
        ;; recursion guard means the per-event counter only increments
        ;; once for the outer call.
        (should (= 1 (nelisp-gc-verify-debug-event-count
                      'verify-double-scan))))
    (nelisp-gc-verify-hooks-test--reset)))


;;; 6. Ring-buffer cap --------------------------------------------

(ert-deftest nelisp-gc-verify-hooks-log-cap-truncates ()
  "Exceeding `nelisp-gc-verify-debug-log-cap' drops oldest entries."
  (nelisp-gc-verify-hooks-test--reset)
  (let ((nelisp-gc-verify-debug-log-cap 3))
    (dotimes (i 7)
      (nelisp-gc-verify-hooks--record
       'verify-heap
       (list :check 'verify-region :severity 'fatal
             :reason (format "synthetic %d" i))))
    (should (= 3 (nelisp-gc-verify-debug-log-count)))
    ;; Newest entry is at the head; the three retained should be 6/5/4.
    (let ((reasons (mapcar (lambda (rec)
                             (plist-get (plist-get rec :violation) :reason))
                           nelisp-gc-verify-debug-log)))
      (should (equal reasons '("synthetic 6"
                               "synthetic 5"
                               "synthetic 4"))))))


;;; 7. Enable / disable idempotency -------------------------------

(ert-deftest nelisp-gc-verify-hooks-enable-disable-idempotent ()
  "Double enable / double disable do not error and leave install-state sane."
  (nelisp-gc-verify-hooks-test--reset)
  (nelisp-gc-verify-hooks-enable)
  (nelisp-gc-verify-hooks-enable)
  (should nelisp-gc-verify-hooks--installed)
  (nelisp-gc-verify-hooks-disable)
  (nelisp-gc-verify-hooks-disable)
  (should-not nelisp-gc-verify-hooks--installed))


(provide 'nelisp-gc-verify-hooks-test)
;;; nelisp-gc-verify-hooks-test.el ends here
