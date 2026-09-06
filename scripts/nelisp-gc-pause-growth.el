;;; nelisp-gc-pause-growth.el --- GC pause growth workload for target/nelisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 201 section 6.14 workload, runnable on the standalone reader:
;;
;;   target/nelisp --load scripts/nelisp-gc-pause-growth.el
;;
;; A large persistent dictionary (`gcw-entries' vector slots, each holding
;; a small list with a string and a cons, roughly five arena blocks per
;; entry) is built once and kept live.  Then `gcw-iterations' turns of a
;; loop each allocate `gcw-inner' short-lived strings and lists -- the
;; shape of one IME keystroke: many small allocations, none retained.
;;
;; The loop records a wall-clock timestamp (`nl-unix-time-usec') and the
;; mid-form collection counter (`nelisp--debug-switch' index 7) at the
;; start of every turn.  A turn whose counter moved contained one or more
;; collections; its pause is the turn's elapsed time minus the median
;; elapsed time of the turns that did not collect.  The summary groups
;; the collections into thirds (start / middle / end of the session) and
;; prints median and 95th-percentile pause for each, so growth over the
;; session is visible without ever quoting a maximum.
;;
;; Parameters are `defvar's so a driver file can `setq' them before
;; loading this one; `gcw-run' is called at the end unless
;; `gcw-no-autorun' is non-nil.  The Makefile smoke
;; `standalone-reader-gc-pause-growth-smoke' runs a short configuration
;; and asserts on the RATIO line.
;;
;; Everything printed is a plain `princ' line so a shell can grep it;
;; the last line is `GCW-SUMMARY' followed by a readable plist.

;;; Code:

(defvar gcw-iterations 300
  "Number of allocation turns after the dictionary is built.")
(defvar gcw-entries 65536
  "Dictionary entries kept live for the whole run.")
(defvar gcw-inner 400
  "Short-lived string+list pairs allocated per turn (~2.7 MB at 400).")
(defvar gcw-no-autorun nil
  "Non-nil: do not call `gcw-run' when this file is loaded.")
(defvar gcw-dict nil
  "The persistent dictionary; a vector kept reachable from this global.")

(defvar gcw-base (nth 0 (nelisp--arena-stats))
  "Arena chunk-0 base; the fixed control slots live at small offsets.")

(defun gcw-reserved ()
  "Bytes of arena reserved across all chunks (slot +728)."
  (ptr-read-u64 (+ gcw-base 728) 0))

(defun gcw-chunks ()
  "Number of mapped arena chunks (slot +720)."
  (ptr-read-u64 (+ gcw-base 720) 0))

(defun gcw-fired ()
  "Cumulative mid-form collections (nl_gc_loop_ctx+32, `nelisp--debug-switch' index 7)."
  (nth 7 (nelisp--debug-switch 0)))

(defun gcw-build-dict (n)
  "Build and return a vector of N live entries."
  (let ((v (make-vector n nil)) (i 0))
    (while (< i n)
      (aset v i (list (number-to-string i) (cons i (1+ i)) (make-string 8 ?a) i))
      (setq i (1+ i)))
    v))

(defun gcw-step (i n)
  "One turn: N short-lived string/list pairs, nothing retained."
  (let ((acc nil) (j 0))
    (while (< j n)
      (setq acc (cons (concat "k" (number-to-string (+ i j))) acc))
      (setq acc (cons (list j i (cons j i)) acc))
      (setq j (1+ j)))
    (length acc)))

(defun gcw-sorted (list)
  "LIST of integers, ascending."
  (sort (copy-sequence list) '<))

(defun gcw-pct (sorted p)
  "The P-th percentile (0-100) of SORTED, or 0 when SORTED is empty."
  (if (null sorted) 0
    (nth (/ (* p (1- (length sorted))) 100) sorted)))

(defun gcw-sum (list)
  (let ((s 0))
    (while list (setq s (+ s (car list))) (setq list (cdr list)))
    s))

(defun gcw-third (list k)
  "The K-th (0, 1 or 2) third of LIST, by position."
  (let* ((n (length list))
         (lo (/ (* k n) 3))
         (hi (if (= k 2) n (/ (* (1+ k) n) 3)))
         (i 0) (out nil))
    (while list
      (when (and (>= i lo) (< i hi)) (setq out (cons (car list) out)))
      (setq i (1+ i))
      (setq list (cdr list)))
    (nreverse out)))

(defun gcw-run ()
  "Run the workload and print the summary."
  (let ((t-build (nl-unix-time-usec)))
    (setq gcw-dict (gcw-build-dict gcw-entries))
    (princ (format "GCW-DICT entries=%d build-us=%d\n"
                   gcw-entries (- (nl-unix-time-usec) t-build))))
  (garbage-collect)
  (let ((wv (nelisp--arena-walk-verify)))
    (princ (format "GCW-LIVE blocks=%d live=%d free=%d reserved=%d chunks=%d\n"
                   (nth 0 wv) (nth 1 wv) (nth 2 wv) (gcw-reserved) (gcw-chunks))))
  (let* ((n gcw-iterations)
         (ts (make-vector (1+ n) 0))
         (fired (make-vector (1+ n) 0))
         (heap (make-vector (1+ n) 0))
         (i 0))
    (aset ts 0 (nl-unix-time-usec))
    (aset fired 0 (gcw-fired))
    (aset heap 0 (gcw-reserved))
    (while (< i n)
      (gcw-step i gcw-inner)
      (setq i (1+ i))
      (aset ts i (nl-unix-time-usec))
      (aset fired i (gcw-fired))
      (aset heap i (gcw-reserved)))
    ;; Split turns into quiet turns and collecting turns.
    (let ((quiet nil) (coll nil) (k 0))
      (while (< k n)
        (let ((el (- (aref ts (1+ k)) (aref ts k))))
          (if (> (aref fired (1+ k)) (aref fired k))
              (setq coll (cons (list k el (aref heap (1+ k))
                                     (- (aref fired (1+ k)) (aref fired k)))
                               coll))
            (setq quiet (cons el quiet))))
        (setq k (1+ k)))
      (setq coll (nreverse coll))
      (let* ((qmed (gcw-pct (gcw-sorted quiet) 50))
             (pauses nil))
        ;; Pause = turn elapsed minus the quiet median.  Printed per
        ;; collection so the raw series is on record.
        (let ((c coll))
          (while c
            (let* ((e (car c))
                   (p (- (nth 1 e) qmed)))
              (setq pauses (cons (if (< p 0) 0 p) pauses))
              (princ (format "GCW-COLL turn=%d pause-us=%d heap=%d fires=%d\n"
                             (nth 0 e) p (nth 2 e) (nth 3 e))))
            (setq c (cdr c))))
        (setq pauses (nreverse pauses))
        (let* ((p0 (gcw-sorted (gcw-third pauses 0)))
               (p1 (gcw-sorted (gcw-third pauses 1)))
               (p2 (gcw-sorted (gcw-third pauses 2)))
               (m0 (gcw-pct p0 50)) (m1 (gcw-pct p1 50)) (m2 (gcw-pct p2 50))
               (total (gcw-sum pauses))
               (ratio (if (> m0 0) (/ (* 100 m2) m0) 0)))
          (princ (format "GCW-THIRD start  n=%d p50-us=%d p95-us=%d\n"
                         (length p0) m0 (gcw-pct p0 95)))
          (princ (format "GCW-THIRD middle n=%d p50-us=%d p95-us=%d\n"
                         (length p1) m1 (gcw-pct p1 95)))
          (princ (format "GCW-THIRD end    n=%d p50-us=%d p95-us=%d\n"
                         (length p2) m2 (gcw-pct p2 95)))
          (princ (format "GCW-TOTAL collections=%d gc-us=%d gc-us-per-1000-turns=%d quiet-median-us=%d turns=%d wall-us=%d\n"
                         (length pauses) total
                         (if (> n 0) (/ (* total 1000) n) 0)
                         qmed n (- (aref ts n) (aref ts 0))))
          (princ (format "GCW-HEAP start=%d end=%d chunks=%d\n"
                         (aref heap 0) (aref heap n) (gcw-chunks)))
          ;; RATIO = p50(end third) as a percentage of p50(start third).
          (princ (format "GCW-RATIO end-over-start-pct=%d\n" ratio))
          (princ "GCW-SUMMARY ")
          (prin1 (list :collections (length pauses)
                       :p50 (list m0 m1 m2)
                       :p95 (list (gcw-pct p0 95) (gcw-pct p1 95) (gcw-pct p2 95))
                       :ratio-pct ratio
                       :gc-us total
                       :heap-start (aref heap 0)
                       :heap-end (aref heap n)))
          (princ "\n")
          ratio)))))

(unless gcw-no-autorun (gcw-run))

;;; nelisp-gc-pause-growth.el ends here
