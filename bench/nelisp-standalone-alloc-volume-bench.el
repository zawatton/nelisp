;;; nelisp-standalone-alloc-volume-bench.el --- what the evaluator allocates, and what collecting it costs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `nelisp-standalone-call-floor-bench.el' measures what a construct costs in
;; wall-clock.  Twice in a row that measurement produced results that
;; contradicted each other on the same question -- does the cost of
;; allocating while a binding is live scale with the live heap? -- once
;; answering 6.9 -> 49.3 us as the heap grew and once answering a flat
;; 16.4 - 16.9 us over the same range.  Neither run was wrong.  The question
;; was unanswerable as posed, because under the shipped collector settings
;; the answer is not stable, and this file exists to show why and to give the
;; two stable numbers underneath it.
;;
;; It measures two things the timing bench cannot.
;;
;; 1. ALLOCATION VOLUME, which has no noise in it at all.  With recycling
;;    disabled (`nelisp--debug-switch' 9) every allocation bumps the arena
;;    cursor, so (bump-after - bump-before) / iterations is exactly how many
;;    bytes the evaluator allocated per iteration -- repeatable to the byte,
;;    and identical at every live-heap size.
;;
;; 2. WHAT COLLECTION COSTS, by running the same loops under three collector
;;    configurations.  The debt threshold is max(FLOOR, live * PCT / 100)
;;    with FLOOR 16 MiB and PCT 300; switches 30 and 31 override them.  So
;;    the same work can be timed with the trigger pushed out of reach, as
;;    shipped, and made aggressive, and the difference is collection.
;;
;; NOT used as a control: `(nelisp--debug-switch 7)', documented as making
;; collect a no-op.  It was checked first and does not freeze the collector
;; -- live-bytes-after-last-gc still moved across a forced `garbage-collect'
;; with it set.  Any experiment that trusted it as "GC off" was not
;; controlled.
;;
;; First measurement (Linux x86_64, target/nelisp at 0dff99ccc):
;;
;;   bytes/iteration, recycling off      over bare loop
;;     loop      1573.1
;;     builtin   2709.6                  +1136.6
;;     call0     3149.6                  +1576.6
;;     call1     4085.6                  +2512.6
;;     call2     4981.6                  +3408.6   (+896.0 per argument,
;;     call3     5877.6                  +4304.6    exactly linear)
;;     let1      3909.6                  +2336.6
;;     let2      5021.6                  +3448.6   (+1112.0 per binding)
;;     cons      2501.6                   +928.6   (the cons itself is 32 B)
;;
;; A bare `while' iteration that only increments a counter allocates about
;; 1.5 KB.  Each further argument to a call costs 896 bytes, where the
;; evaluator's own commentary describes the per-argument work as "a 32-byte
;; cons allocation".  Each further `let' binding costs 1112 bytes.
;;
;; None of it accumulates: measured after a collection, with recycling back
;; on, the net bump over the same loops is 0.0 bytes per iteration in every
;; case.  It is not a leak.  It is traffic -- every one of those bytes is
;; allocated and recycled again, and the collector's debt trigger counts it,
;; so this churn schedules collections even though the live set never moves.
;;
;;   us/iteration, mean of 3 repeats (n=4000), with the spread
;;     live 17 MB  default   loop   9.90         call1  36.37 (205.0%)  let1  34.52 (249.7%)
;;                 debt-off  loop   9.92         call1  23.40  (20.2%)  let1  22.91   (3.2%)
;;                 debt-hot  loop  82.70         call1 215.78   (2.0%)  let1 206.07  (10.5%)
;;     live 31 MB  default   loop  10.91         call1  22.15   (1.6%)  let1  54.25 (497.6%)
;;                 debt-off  loop  10.61         call1  22.02   (1.6%)  let1  22.31   (5.5%)
;;                 debt-hot  loop 116.66         call1 344.32   (1.3%)  let1 296.72  (24.4%)
;;     live 59 MB  default   loop  11.11         call1  22.32   (4.0%)  let1  19.43   (6.1%)
;;                 debt-off  loop  11.11         call1  22.32   (3.9%)  let1  19.39   (6.0%)
;;                 debt-hot  loop 159.95         call1 422.23  (45.7%)  let1 420.38  (46.9%)
;;
;; The spread column is the finding.  Under the shipped settings the same
;; loop, on the same binary, at the same live heap, measured 18.52 and 120.85
;; us/iteration in consecutive repeats -- 6.5x, with nothing changed between
;; them.  That is the answer to why two careful runs disagreed about
;; heap-scaling: 6.9 and 49.3 both sit inside that.  Neither run was wrong;
;; the measurement was not repeatable, so no conclusion about scaling could
;; be drawn from it at all.  (Each row here begins with its own
;; `garbage-collect', so this is not the first row paying for the heap build:
;; with that control in place the default spread is unchanged.)
;;
;; Read the three configurations at each heap size together:
;;
;;   debt-off is FLAT in live heap and repeatable.  22.0 - 23.4 us for
;;   `call1' across a 3.5x change in live bytes, spreads of 1.6 - 20%.  The
;;   evaluator's own cost does not scale with the heap.
;;
;;   debt-hot scales CLEANLY with live heap -- 216 -> 344 -> 422 us for
;;   `call1' -- because a sweep costs the whole heap and a tiny threshold
;;   makes it happen constantly.  This is what collection costs when it is
;;   the bottleneck.
;;
;;   default is neither flat nor monotonic.  The threshold grows with live
;;   (live*3) while the sweep also grows with live, so the two partly cancel
;;   and what is left depends on where in the debt cycle a run happens to
;;   begin.  At 59 MB it happens to coincide with debt-off to the second
;;   decimal; at 31 MB the same `let1' varies 6x.
;;
;; What this points at, and what it does not.  It does not point at tuning
;; the threshold: debt-hot and debt-off are the two ends of that dial and
;; the good end is already close to where the default sits.  It points at
;; the 1.5 KB, the 896 bytes per argument and the 1112 bytes per binding,
;; because those set both the allocator traffic and how often the collector
;; is asked to run.  Lower them and both fall together.
;;
;; Run it against the binary, not the host:
;;   make standalone-alloc-volume-bench          (or)
;;   ./target/nelisp --load bench/nelisp-standalone-alloc-volume-bench.el

;;; Code:

(defvar nelisp-standalone-alloc-volume-bench-volume-iterations 2000
  "Iterations per volume case.

Deliberately small.  A volume measurement needs recycling disabled, so every
iteration bumps the arena and a large count can exhaust it or trip a
growth-triggered collection -- either of which silently corrupts the figure.
At 32000 iterations the `let1' case here measured 0.0 bytes per iteration,
which is why `nelisp-standalone-alloc-volume-bench--volume' validates rather
than trusts its own subtraction.")

(defvar nelisp-standalone-alloc-volume-bench-time-iterations 4000
  "Iterations per timing case.")

(defvar nelisp-standalone-alloc-volume-bench-repeats 3
  "Timing repeats per row.  The spread is the point under the shipped
settings, so a single sample would hide exactly what this file reports.")

(defvar nelisp-standalone-alloc-volume-bench--held nil
  "Retained list that sets the live-heap size for a timing row.")

(defun nelisp-standalone-alloc-volume-bench--bump ()
  (nth 2 (nelisp--arena-stats)))

(defun nelisp-standalone-alloc-volume-bench--live ()
  (nth 4 (nelisp--arena-stats)))

(defun nelisp-standalone-alloc-volume-bench--leaf0 () 1)
(defun nelisp-standalone-alloc-volume-bench--leaf1 (x) x)
(defun nelisp-standalone-alloc-volume-bench--leaf2 (x y) x)
(defun nelisp-standalone-alloc-volume-bench--leaf3 (x y z) x)

(defun nelisp-standalone-alloc-volume-bench--loop (n)
  (let ((i 0)) (while (< i n) (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--builtin (n)
  (let ((i 0) (a 0)) (while (< i n) (setq a (+ i 1)) (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--call0 (n)
  (let ((i 0) (a 0))
    (while (< i n)
      (setq a (nelisp-standalone-alloc-volume-bench--leaf0))
      (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--call1 (n)
  (let ((i 0) (a 0))
    (while (< i n)
      (setq a (nelisp-standalone-alloc-volume-bench--leaf1 i))
      (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--call2 (n)
  (let ((i 0) (a 0))
    (while (< i n)
      (setq a (nelisp-standalone-alloc-volume-bench--leaf2 i 1))
      (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--call3 (n)
  (let ((i 0) (a 0))
    (while (< i n)
      (setq a (nelisp-standalone-alloc-volume-bench--leaf3 i 1 2))
      (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--let1 (n)
  (let ((i 0) (a 0))
    (while (< i n) (setq a (let ((k i)) k)) (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--let2 (n)
  (let ((i 0) (a 0))
    (while (< i n) (setq a (let ((k i) (m 1)) k)) (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--cons (n)
  (let ((i 0) (a nil)) (while (< i n) (setq a (cons i nil)) (setq i (1+ i)))))

(defun nelisp-standalone-alloc-volume-bench--volume (fn n)
  "Bytes FN allocates per iteration, or nil if the measurement was not valid.

Valid means: recycling was off, so the bump moved; and no sweep ran
underneath, so the bump delta is the whole allocation and not what survived
a collection.  `live-bytes-after-last-gc' only changes when a sweep actually
runs, which makes it the detector."
  (funcall fn 50)                       ; warm: first pass pays one-time work
  (let* ((live0 (nelisp-standalone-alloc-volume-bench--live))
         (b0 (nelisp-standalone-alloc-volume-bench--bump))
         (_ (funcall fn n))
         (b1 (nelisp-standalone-alloc-volume-bench--bump))
         (live1 (nelisp-standalone-alloc-volume-bench--live)))
    (cond ((/= live0 live1) nil)        ; a collection intervened
          ((<= b1 b0) nil)              ; recycling was not actually off
          (t (/ (float (- b1 b0)) n)))))

(defun nelisp-standalone-alloc-volume-bench--time (fn n)
  (funcall fn 50)
  (let* ((t0 (float-time)) (_ (funcall fn n)) (t1 (float-time)))
    (/ (* 1000000.0 (- t1 t0)) n)))

(defun nelisp-standalone-alloc-volume-bench--cases ()
  (list (cons "loop" #'nelisp-standalone-alloc-volume-bench--loop)
        (cons "builtin" #'nelisp-standalone-alloc-volume-bench--builtin)
        (cons "call0" #'nelisp-standalone-alloc-volume-bench--call0)
        (cons "call1" #'nelisp-standalone-alloc-volume-bench--call1)
        (cons "call2" #'nelisp-standalone-alloc-volume-bench--call2)
        (cons "call3" #'nelisp-standalone-alloc-volume-bench--call3)
        (cons "let1" #'nelisp-standalone-alloc-volume-bench--let1)
        (cons "let2" #'nelisp-standalone-alloc-volume-bench--let2)
        (cons "cons" #'nelisp-standalone-alloc-volume-bench--cons)))

(defun nelisp-standalone-alloc-volume-bench-volume ()
  "Print bytes allocated per iteration for each case.  Returns an alist."
  (let ((n nelisp-standalone-alloc-volume-bench-volume-iterations)
        (base nil)
        (rows nil))
    (princ (format "ALLOC-VOLUME iterations=%d\n" n))
    (nelisp--debug-switch 9)            ; no reuse: every allocation bumps
    (dolist (case (nelisp-standalone-alloc-volume-bench--cases))
      (let ((v (nelisp-standalone-alloc-volume-bench--volume (cdr case) n)))
        (cond
         ((null v)
          (princ (format "ALLOC-VOLUME %-8s   INVALID (a collection ran under it)\n"
                         (car case))))
         (t
          (unless base (setq base v))
          (princ (format "ALLOC-VOLUME %-8s %9.1f bytes/iter  (over loop %+9.1f)\n"
                         (car case) v (- v base)))
          (setq rows (cons (cons (intern (car case)) v) rows))))))
    (nelisp--debug-switch 10)
    ;; How much of that volume is churn the allocator already recycles?  With
    ;; recycling on, bump only moves for what the free lists could not
    ;; satisfy.  The `garbage-collect' is load-bearing and was learned the
    ;; hard way: the volume phase above ran with recycling off and nothing
    ;; freed, so it leaves the free lists EMPTY.  Measuring net growth in
    ;; that state measures an exhausted allocator, not the steady state -- and
    ;; measuring it after a collection that happened to intervene returns a
    ;; flat 0.0, which is how an earlier probe of mine concluded "none of it
    ;; accumulates".  Force the collection, then measure.
    (garbage-collect)
    (dolist (case (nelisp-standalone-alloc-volume-bench--cases))
      (let* ((b0 (nelisp-standalone-alloc-volume-bench--bump))
             (_ (funcall (cdr case) n))
             (net (/ (float (- (nelisp-standalone-alloc-volume-bench--bump) b0)) n)))
        (princ (format "ALLOC-VOLUME %-8s net %9.1f bytes/iter (recycling on, after a collection)\n"
                       (car case) net))))
    (nreverse rows)))

(defun nelisp-standalone-alloc-volume-bench--config (name)
  "Install collector configuration NAME.  See the commentary."
  (cond
   ;; As shipped: FLOOR 16 MiB, PCT 300 (0 selects each documented default).
   ((eq name 'default) (nelisp--debug-switch 30 0) (nelisp--debug-switch 31 0))
   ;; Threshold out of reach, so the debt trigger does not fire.
   ((eq name 'debt-off) (nelisp--debug-switch 30 1099511627776)
    (nelisp--debug-switch 31 100000))
   ;; Threshold tiny, so it fires constantly.
   ((eq name 'debt-hot) (nelisp--debug-switch 30 1048576)
    (nelisp--debug-switch 31 10))))

(defun nelisp-standalone-alloc-volume-bench--row (tag config cases n repeats)
  (nelisp-standalone-alloc-volume-bench--config config)
  ;; Every configuration must start from the same arena state, or the first
  ;; row at each heap size silently pays for building that heap and the
  ;; comparison is an ordering artifact rather than a measurement.  Sweep,
  ;; which also zeroes the accumulated debt, so each row begins its own cycle.
  (garbage-collect)
  (dolist (case cases)
    (let ((samples nil) (r 0))
      (while (< r repeats)
        (setq samples
              (cons (nelisp-standalone-alloc-volume-bench--time (cdr case) n)
                    samples))
        (setq r (1+ r)))
      (let ((lo (car samples)) (hi (car samples)) (sum 0.0))
        (dolist (s samples)
          (when (< s lo) (setq lo s))
          (when (> s hi) (setq hi s))
          (setq sum (+ sum s)))
        (princ (format "ALLOC-VOLUME %-22s %-8s %8.2f us/iter  (min %8.2f max %8.2f spread %5.1f%%)\n"
                       tag (car case) (/ sum repeats) lo hi
                       (if (> lo 0) (* 100.0 (/ (- hi lo) lo)) 0.0))))))
  (nelisp-standalone-alloc-volume-bench--config 'default))

(defun nelisp-standalone-alloc-volume-bench-collection ()
  "Print time per iteration under three collector configurations, per heap size."
  (let ((n nelisp-standalone-alloc-volume-bench-time-iterations)
        (repeats nelisp-standalone-alloc-volume-bench-repeats)
        (cases (list (cons "loop" #'nelisp-standalone-alloc-volume-bench--loop)
                     (cons "call1" #'nelisp-standalone-alloc-volume-bench--call1)
                     (cons "let1" #'nelisp-standalone-alloc-volume-bench--let1))))
    (princ (format "ALLOC-VOLUME timing iterations=%d repeats=%d\n" n repeats))
    (dolist (size '(0 200000 600000))
      (setq nelisp-standalone-alloc-volume-bench--held nil)
      (let ((i 0))
        (while (< i size)
          (setq nelisp-standalone-alloc-volume-bench--held
                (cons i nelisp-standalone-alloc-volume-bench--held))
          (setq i (1+ i))))
      (garbage-collect)
      (let ((live (nelisp-standalone-alloc-volume-bench--live)))
        ;; Volume again at this heap size.  The commentary claims the figure
        ;; is independent of the live heap; this is where that is shown
        ;; rather than asserted, and it is what makes the timing rows above
        ;; attributable -- if the same loop allocated more at a bigger heap,
        ;; the timing difference would not have to be collection at all.
        (nelisp--debug-switch 9)
        (let ((v (nelisp-standalone-alloc-volume-bench--volume
                  #'nelisp-standalone-alloc-volume-bench--call1
                  nelisp-standalone-alloc-volume-bench-volume-iterations)))
          (nelisp--debug-switch 10)
          (princ (format "ALLOC-VOLUME live=%dMB %-14s %-8s %9s bytes/iter\n"
                         (/ live 1048576) "volume-check" "call1"
                         (if v (format "%.1f" v) "INVALID"))))
        (garbage-collect)
        (dolist (config '(default debt-off debt-hot))
          (nelisp-standalone-alloc-volume-bench--row
           (format "live=%dMB %s" (/ live 1048576) config)
           config cases n repeats))))))

(defun nelisp-standalone-alloc-volume-bench-run ()
  (nelisp-standalone-alloc-volume-bench-volume)
  (nelisp-standalone-alloc-volume-bench-collection)
  (princ "ALLOC-VOLUME done\n"))

(nelisp-standalone-alloc-volume-bench-run)

(provide 'nelisp-standalone-alloc-volume-bench)

;;; nelisp-standalone-alloc-volume-bench.el ends here
