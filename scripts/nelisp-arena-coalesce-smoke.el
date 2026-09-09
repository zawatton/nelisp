;;; nelisp-arena-coalesce-smoke.el --- deterministic arena free-run smoke -*- lexical-binding: t; -*-

;; This is a standalone-reader probe.  It deliberately disables free-list
;; reuse while making a fixed number of equal-size vectors, then collects.
;; The allocations therefore form long adjacent dead runs.  A coalescing
;; collector must preserve the arena walk and reduce the free-block count by
;; merging those runs; a collector that only links each dead block leaves the
;; count close to the number of blocks allocated by the workload.

(nelisp--debug-switch 9)
(setq nelisp-arena-coalesce-before (nelisp--arena-walk-verify))
(let ((i 0) (last nil))
  (while (< i 20000)
    (setq last (make-vector 64 nil))
    (setq i (1+ i)))
  nil)
(setq nelisp-arena-coalesce-allocated (nelisp--arena-walk-verify))
(garbage-collect)
(setq nelisp-arena-coalesce-after (nelisp--arena-walk-verify))
(setq nelisp-arena-coalesce-used-before (nth 11 (nelisp--arena-stats)))
(nelisp--debug-switch 10)
(let ((i 0) (last nil))
  (while (< i 20000)
    (setq last (make-vector 64 nil))
    (setq i (1+ i)))
  nil)
(setq nelisp-arena-coalesce-used-after (nth 11 (nelisp--arena-stats)))
(let* ((allocated (nth 0 nelisp-arena-coalesce-allocated))
       (after-live (nth 1 nelisp-arena-coalesce-after))
       (after-free (nth 2 nelisp-arena-coalesce-after))
       (freed (- allocated after-live))
       (wellformed (= (nth 4 nelisp-arena-coalesce-after) 1))
       (reuse-stable (<= (- nelisp-arena-coalesce-used-after
                            nelisp-arena-coalesce-used-before)
                         65536))
       ;; A single free block per long run is expected.  Keep the assertion
       ;; relative to the workload so boot-image block-count changes do not
       ;; make the smoke machine-specific.
       (coalesced (< (* 4 after-free) freed)))
  (princ (format "ARENA-COALESCE before=%S allocated=%S after=%S used-before=%d used-after=%d freed=%d pass=%S\n"
                 nelisp-arena-coalesce-before
                 nelisp-arena-coalesce-allocated
                 nelisp-arena-coalesce-after
                 nelisp-arena-coalesce-used-before
                 nelisp-arena-coalesce-used-after
                 freed
                 (and wellformed coalesced reuse-stable))))
