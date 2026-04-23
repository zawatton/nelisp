;;; nelisp-gc-bench.el --- Phase 3c.6 GC mark-pass bench  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3c.6 merge-gate companion: measure the wall-clock cost of a
;; full `nelisp-gc-reachable-set' walk over synthetic global-table
;; populations of 100 / 1000 / 10000 entries.  Numbers are advisory,
;; not gated — the Phase 3c merge gate (design doc §9) only requires
;; functional correctness, not a speed floor.
;;
;; `make gc-bench' is the entry point.

;;; Code:

(require 'nelisp)
(require 'nelisp-gc)

(defun nelisp-gc-bench--populate (n)
  "Stuff N synthetic entries into `nelisp--globals' for a bench run.
Values are small cons cells so the walker actually has edges to
traverse — bare integers would be leaves and understate the cost."
  (dotimes (i n)
    (puthash (intern (format "nelisp-gc-bench--k-%d" i))
             (cons i (cons (format "v-%d" i) nil))
             nelisp--globals)))

(defun nelisp-gc-bench--cleanup (n)
  "Remove the synthetic entries inserted by `nelisp-gc-bench--populate'."
  (dotimes (i n)
    (remhash (intern (format "nelisp-gc-bench--k-%d" i))
             nelisp--globals)))

(defun nelisp-gc-bench--time-walk (n)
  "Populate N, time one full `nelisp-gc-reachable-set' walk, return
`(:entries N :reached M :seconds S)'.  Always cleans up its synthetic
entries so a subsequent run starts from the same state."
  (nelisp-gc-bench--populate n)
  (unwind-protect
      (let* ((t0 (current-time))
             (live (nelisp-gc-reachable-set))
             (dt (float-time (time-since t0))))
        (list :entries n
              :reached (hash-table-count live)
              :seconds dt))
    (nelisp-gc-bench--cleanup n)))

;;;###autoload
(defun nelisp-gc-bench-batch ()
  "`make gc-bench' entry point.
Runs the walk under 100 / 1000 / 10000 synthetic entries and prints
one summary line per size.  No assertion — if numbers regress, the
bench line shows it for the reader."
  (message "==== nelisp-gc-bench Phase 3c.6 ====")
  (dolist (n '(100 1000 10000))
    (let ((r (nelisp-gc-bench--time-walk n)))
      (message "gc-walk n=%5d reached=%6d elapsed=%.4fs"
               (plist-get r :entries)
               (plist-get r :reached)
               (plist-get r :seconds))))
  (message "==== done ===="))

(provide 'nelisp-gc-bench)

;;; nelisp-gc-bench.el ends here
