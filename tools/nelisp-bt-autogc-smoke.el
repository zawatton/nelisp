;;; nelisp-bt-autogc-smoke.el --- Doc 214 S8 automatic GC while a thread runs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S8 regression check, run from the repository root:
;;   nelisp --load tools/nelisp-bt-autogc-smoke.el
;; One thread, started before the debt trigger is armed, allocates garbage
;; only and never calls `garbage-collect'.  The allocation-debt trigger is
;; not armed until two sweeps have run, and 209e639f5 made a parallel
;; section trigger on the debt alone, so such a thread never collected and
;; the heap grew without bound (9.7 GB at 100000 iterations).  Passing
;; requires at least one parked collection while the thread runs, and the
;; thread's result to be right.
;; Prints BT-AUTOGC-PASS.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(defconst nl-bt-autogc-smoke--iterations 4000
  "Loop count; each iteration drops a 64-element list.")

(let* ((before (nth 20 (nelisp--debug-switch 0)))
       (t0 (float-time))
       (thread
        (nl-bt-make-thread
         (lambda ()
           (let ((i 0) (acc 0))
             (while (< i nl-bt-autogc-smoke--iterations)
               (setq acc (+ acc (length (make-list 64 i))))
               (setq i (1+ i)))
             acc))))
       (total (nl-bt-join-thread thread))
       (collects (- (nth 20 (nelisp--debug-switch 0)) before)))
  (princ (format "BT-AUTOGC total=%S parked-collects=%d secs=%.1f\n"
                 total collects (- (float-time) t0)))
  (princ (if (and (equal total (* 64 nl-bt-autogc-smoke--iterations))
                  (> collects 0))
             "BT-AUTOGC-PASS\n" "BT-AUTOGC-FAIL\n")))

;;; nelisp-bt-autogc-smoke.el ends here
