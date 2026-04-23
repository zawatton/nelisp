;;; nelisp-bench-test.el --- Bench equivalence ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Verify every Phase 3b.7 bench case produces the same result via
;; the interpreter and the VM, and surface the current speedup
;; ratios via `(message ...)' so test logs preserve a longitudinal
;; record across commits.  The 10x merge gate itself isn't enforced
;; here — see docs/design/08-bytecode-vm.org §3b.7 for status and
;; the optimization roadmap.

;;; Code:

(require 'ert)
(require 'nelisp-bench)

(defconst nelisp-bench-test--small-cases
  '((fib       (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
               8)
    (fact      (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))
               6)
    (sum-to    (lambda (n acc) (if (< n 1) acc (sum-to (- n 1) (+ acc n))))
               20 0)
    (let-chain (lambda (n)
                 (let ((a 1)) (let ((b (+ a 1))) (let ((c (+ b 1)))
                   (+ a b c n)))))
               0))
  "Smaller variants of `nelisp-bench--cases' so the equivalence ERT
runs quickly even when the host interpreter recurses naively.
Bigger cases (fib(20) etc.) live in `nelisp-bench--cases' and run
via `make bench' where `max-lisp-eval-depth' is left to the user.")

(ert-deftest nelisp-bench-equivalence ()
  "Every bench case produces an `equal' result through interp and VM."
  (dolist (case nelisp-bench-test--small-cases)
    (let ((r (nelisp-bench-run-case case 1)))
      (should (plist-get r :bcl))
      (should (plist-get r :equal)))))

(ert-deftest nelisp-bench-numbers-recorded ()
  "Run every small bench case and surface the speedups in the test
log.  Does not assert the merge gate (see design doc §3b.7)."
  (dolist (case nelisp-bench-test--small-cases)
    (let ((r (nelisp-bench-run-case case 1)))
      (message "BENCH %s interp=%.4fs vm=%.4fs speedup=%.2fx"
               (plist-get r :name)
               (plist-get r :interp-time)
               (plist-get r :vm-time)
               (plist-get r :speedup)))))

(provide 'nelisp-bench-test)

;;; nelisp-bench-test.el ends here
