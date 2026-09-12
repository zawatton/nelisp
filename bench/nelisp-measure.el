;;; nelisp-measure.el --- measure the standalone's allocation, from the REPL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Measuring what a construct costs in `target/nelisp' needs the same six
;; steps every time: stop recycling so allocations bump the arena, sweep
;; first so the run starts from a known state, read the cursor before and
;; after, put recycling back, and -- the step that is easy to leave out --
;; check that no collection ran underneath, because if one did the
;; subtraction is meaningless rather than merely noisy.
;;
;; That harness was written five times in one day on 2026-09-12, and the
;; first version left the last step out: it reported "0.0 bytes per
;; iteration, nothing accumulates", which was not a result but an artifact
;; of a collection having refilled the free lists just before.  A helper
;; everyone shares cannot forget the check.
;;
;; Load it in a REPL session and measure a form in one line:
;;
;;   (load "bench/nelisp-measure.el" nil t t)
;;   (nelisp-measure-report "cons" (lambda (n) (let ((i 0) (a nil))
;;                                               (while (< i n)
;;                                                 (setq a (cons i nil))
;;                                                 (setq i (1+ i))))))
;;
;; which prints bytes per iteration, the split by block-size class, and the
;; time with its spread across repeats.  Every function here takes a FN of
;; one argument N and is expected to run its body N times.
;;
;; Read the numbers the way the bench file does: allocation volume is exact
;; and repeatable to the byte, time under the shipped collector settings is
;; not repeatable at all, which is why the time row always reports a spread
;; and never a single sample.

;;; Code:

(defvar nelisp-measure-default-iterations 500
  "Iterations per measurement.

Deliberately small.  A volume measurement runs with recycling off, so every
iteration bumps real memory that nothing can hand back; a large count, or
many measurements back to back, eventually crosses a growth trigger whose
collection voids the figure.  When that happens the measurement returns nil
rather than a number.")

(defun nelisp-measure--bump ()
  "Arena bump cursor."
  (nth 2 (nelisp--arena-stats)))

(defun nelisp-measure--live ()
  "Live bytes as of the last sweep.

This only changes when a sweep actually runs, which makes it the detector
for \"did a collection happen underneath my measurement\"."
  (nth 4 (nelisp--arena-stats)))

(defun nelisp-measure-reset ()
  "Put the arena into a known state: recycling on, swept, debt zeroed."
  (nelisp--debug-switch 10)
  (garbage-collect))

(defun nelisp-measure-alloc (fn &optional n)
  "Bytes FN allocates per iteration, or nil if the measurement was not valid.

Valid means recycling was off, so the cursor moved, and no sweep ran
underneath, so the difference is the whole allocation rather than what
survived a collection."
  (let ((n (or n nelisp-measure-default-iterations)))
    (funcall fn 50)                     ; warm: the first pass pays one-time work
    (nelisp-measure-reset)
    (nelisp--debug-switch 9)            ; no reuse: every allocation bumps
    (let* ((live0 (nelisp-measure--live))
           (b0 (nelisp-measure--bump))
           (_ (funcall fn n))
           (b1 (nelisp-measure--bump))
           (live1 (nelisp-measure--live)))
      (nelisp--debug-switch 10)
      (cond ((/= live0 live1) nil)      ; a collection intervened
            ((<= b1 b0) nil)            ; recycling was not actually off
            (t (/ (float (- b1 b0)) n))))))

(defun nelisp-measure-net (fn &optional n)
  "Bytes FN grows the heap by per iteration, with recycling ON.

The gap between this and `nelisp-measure-alloc' is how much of the traffic
the allocator already recycles.  The sweep first is load-bearing: measured
straight after a volume run the free lists are empty, and this then measures
an exhausted allocator instead of the steady state."
  (let ((n (or n nelisp-measure-default-iterations)))
    (funcall fn 50)
    (nelisp-measure-reset)
    (let* ((b0 (nelisp-measure--bump))
           (_ (funcall fn n))
           (b1 (nelisp-measure--bump)))
      (/ (float (- b1 b0)) n))))

(defun nelisp-measure-census (fn &optional n)
  "Per-iteration allocation of FN split by block-size class, or nil if invalid.

Returns a plist: :bytes total, :small bytes in blocks of 32 or less,
:small-blocks how many of those, :slot bytes in the 33-64 class, :medium
bytes in 65-256.

The 33-64 class is the one to watch.  BLOCK_TOTAL includes an 8-byte header,
so an ordinary 32-byte Sexp slot lands there, not in :small -- that class is
where the interpreter's scratch slots are."
  (let ((n (or n nelisp-measure-default-iterations)))
    (funcall fn 50)
    (nelisp-measure-reset)
    (nelisp--debug-switch 9)
    (let* ((c0 (nelisp--size-census))
           (live0 (nelisp-measure--live))
           (b0 (nelisp-measure--bump))
           (_ (funcall fn n))
           (b1 (nelisp-measure--bump))
           (live1 (nelisp-measure--live))
           (c1 (nelisp--size-census)))
      (nelisp--debug-switch 10)
      (if (/= live0 live1)
          nil
        (let ((d (lambda (i) (/ (float (- (nth i c1) (nth i c0))) n))))
          (list :bytes (/ (float (- b1 b0)) n)
                :small (funcall d 3)
                :small-blocks (funcall d 4)
                :slot (funcall d 5)
                :medium (funcall d 6)))))))

(defun nelisp-measure-time (fn &optional n repeats)
  "Time FN per iteration: a plist of :mean :min :max :spread (percent).

Never a single sample.  Under the shipped collector settings the same loop
at the same live heap has measured 18.5 and 124.2 microseconds in
consecutive repeats, so one number carries no information about the next
one; the spread is what says whether it does."
  (let* ((n (or n nelisp-measure-default-iterations))
         (repeats (or repeats 3))
         (samples nil)
         (r 0))
    (funcall fn 50)
    (while (< r repeats)
      (let* ((t0 (float-time)) (_ (funcall fn n)) (t1 (float-time)))
        (setq samples (cons (/ (* 1000000.0 (- t1 t0)) n) samples)))
      (setq r (1+ r)))
    (let ((lo (car samples)) (hi (car samples)) (sum 0.0))
      (dolist (s samples)
        (when (< s lo) (setq lo s))
        (when (> s hi) (setq hi s))
        (setq sum (+ sum s)))
      (list :mean (/ sum repeats) :min lo :max hi
            :spread (if (> lo 0) (* 100.0 (/ (- hi lo) lo)) 0.0)))))

;;; Collector configuration.
;;
;; The debt threshold is max(FLOOR, live * PCT / 100), FLOOR 16 MiB and PCT
;; 300, and switches 30 and 31 override them.  Holding the trigger off is how
;; a timing measurement becomes repeatable; making it aggressive is how to
;; see what collection costs when it dominates.
;;
;; `(nelisp--debug-switch 7)' is NOT a control here.  It is documented as
;; making collect a no-op and does not: live-bytes still moved across a
;; forced `garbage-collect' with it set.

(defun nelisp-measure-collector (config)
  "Install collector CONFIG: `default', `debt-off' or `debt-hot'."
  (cond
   ((eq config 'default) (nelisp--debug-switch 30 0) (nelisp--debug-switch 31 0))
   ((eq config 'debt-off) (nelisp--debug-switch 30 1099511627776)
    (nelisp--debug-switch 31 100000))
   ((eq config 'debt-hot) (nelisp--debug-switch 30 1048576)
    (nelisp--debug-switch 31 10))
   (t (error "nelisp-measure: unknown collector config"))))

(defmacro nelisp-measure-with-collector (config &rest body)
  "Run BODY with collector CONFIG installed, then restore the shipped one."
  (declare (indent 1))
  (list 'progn
        (list 'nelisp-measure-collector config)
        (list 'garbage-collect)
        (cons 'prog1 (append body (list '(nelisp-measure-collector 'default))))))

(defun nelisp-measure-report (label fn &optional n)
  "Print one line for FN: bytes per iteration, its split, and the time.

The line a measurement session actually wants, so that asking the question
costs one form rather than a file."
  (let* ((n (or n nelisp-measure-default-iterations))
         (census (nelisp-measure-census fn n))
         (time (nelisp-measure-time fn n 3)))
    (if (null census)
        (princ (format "MEASURE %-20s INVALID (a collection ran under it)\n" label))
      (princ (format "MEASURE %-20s %9.1f B/iter  (<=32 %6.1f in %4.1f blk, 33-64 %8.1f, 65-256 %7.1f)  %8.2f us/iter (spread %5.1f%%)\n"
                     label (plist-get census :bytes)
                     (plist-get census :small) (plist-get census :small-blocks)
                     (plist-get census :slot) (plist-get census :medium)
                     (plist-get time :mean) (plist-get time :spread))))
    (list :label label :census census :time time)))

(defun nelisp-measure-diff (label-a fn-a label-b fn-b &optional n)
  "Print what FN-B costs over FN-A, which is how a single unit gets priced.

Neighbouring bodies that differ by exactly one construct are the only way to
price that construct here; comparing two DIFFERENT functions instead prices
their difference as well.  Doing that -- `(+ i 1)' against `(1+ i)' -- put a
per-argument figure at 352 bytes on 2026-09-12 when the real one, from one
function at two arities, is 112."
  (let* ((n (or n nelisp-measure-default-iterations))
         (a (nelisp-measure-alloc fn-a n))
         (b (nelisp-measure-alloc fn-b n)))
    (if (or (null a) (null b))
        (princ (format "MEASURE %s -> %s  INVALID\n" label-a label-b))
      (princ (format "MEASURE %-14s -> %-14s %+9.1f B/iter  (%.1f -> %.1f)\n"
                     label-a label-b (- b a) a b)))
    (and a b (- b a))))

(provide 'nelisp-measure)

;;; nelisp-measure.el ends here
