;;; nelisp-gc-pause-budget-driver.el --- one scenario run for the GC pause budget check  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Companion to `test/nelisp-gc-pause-budget.sh'.  That script never edits
;; this file's parameters directly; it writes a tiny config file of `setq'
;; forms (one per scenario) and loads it before this one, exactly the
;; pattern `scripts/nelisp-gc-pause-growth.el' documents for its own
;; parameters:
;;
;;   target/nelisp --load CONFIG.el --load test/nelisp-gc-pause-budget-driver.el
;;
;; This driver deliberately does not reuse `nelisp-gc-pause-growth.el' or
;; its `gcw-' helpers -- that file is another session's mid-form GC pause
;; growth harness (Doc 201 §6.14) with its own tuning; this one is a
;; smaller, purpose-built budget probe with independently chosen scenario
;; sizes (see the shell wrapper for why).
;;
;; Measurement method (same primitives the existing probes and
;; `nelisp-gc-pause-growth.el' use -- there is no other exposed pause-time
;; signal on the standalone reader):
;;   - `nl-unix-time-usec' timestamps each turn boundary.
;;   - `(nth 7 (nelisp--debug-switch 0))' is the cumulative mid-form
;;     collection counter; a turn whose counter moved contains >=1 GC.
;;   - A turn's "pause" = its elapsed time minus the median elapsed time of
;;     turns that did not collect (the same turns, running the same code,
;;     minus GC is the least-bias baseline available without a dedicated
;;     GC-time counter).
;;
;; Two workload modes:
;;   'cons -- N cons cells built and dropped per turn (scenario a and the
;;            live-set scenarios c/d, which add a persistent vector).
;;   'str  -- N `split-string' calls per turn (scenario b).
;;
;; Prints one `GCB-RESULT' line the wrapper parses; also prints
;; `GCB-BUILD' (live-set construction cost) and one `GCB-COLL' line per
;; collecting turn, so a captured log is human-legible too.

;;; Code:

(defvar gcb-mode 'cons
  "'cons or 'str; which per-turn workload `gcb-step' runs.")
(defvar gcb-live-n 0
  "Persistent live entries built once before the loop; 0 = garbage-only.")
(defvar gcb-turn-alloc 20000
  "Per-turn workload size (cons cells, or split-string calls).")
(defvar gcb-max-turns 20
  "Fixed turn count; the scenario is sized so this reliably clears the
caller's minimum-collections requirement inside its time budget.")
(defvar gcb-sample-str "alpha beta gamma delta epsilon zeta eta theta"
  "Fixed input to `split-string' in 'str mode.")
(defvar gcb-live nil
  "Holds the live-set vector so it stays reachable for the whole run.")

(defvar gcb-base (nth 0 (nelisp--arena-stats))
  "Arena chunk-0 base; matches `nelisp-gc-pause-growth.el' (gcw-base).")

(defun gcb-fired ()
  "Cumulative mid-form collections (`nelisp--debug-switch' index 7)."
  (nth 7 (nelisp--debug-switch 0)))

(defun gcb-reserved ()
  "Bytes of arena reserved across all chunks (slot +728)."
  (ptr-read-u64 (+ gcb-base 728) 0))

(defun gcb-build-live (n)
  "Build and return a vector of N small live entries."
  (let ((v (make-vector (max n 1) nil)) (i 0))
    (while (< i n)
      (aset v i (list (number-to-string i) (cons i (1+ i)) (make-string 8 ?a)))
      (setq i (1+ i)))
    v))

(defun gcb-step-cons (n)
  "One turn: N cons cells, nothing retained."
  (let ((acc nil) (j 0))
    (while (< j n) (setq acc (cons j acc)) (setq j (1+ j)))
    (length acc)))

(defun gcb-step-str (n)
  "One turn: N `split-string' calls, nothing retained."
  (let ((j 0) (total 0))
    (while (< j n)
      (setq total (+ total (length (split-string gcb-sample-str " "))))
      (setq j (1+ j)))
    total))

(defun gcb-sorted (list) (sort (copy-sequence list) '<))

(defun gcb-median (sorted)
  (let ((n (length sorted)))
    (if (= n 0) 0 (nth (/ n 2) sorted))))

(defun gcb-max (list)
  (let ((m 0))
    (while list (when (> (car list) m) (setq m (car list))) (setq list (cdr list)))
    m))

(defun gcb-run ()
  "Run the configured scenario once and print GCB-BUILD / GCB-COLL / GCB-RESULT."
  (let ((t0 (nl-unix-time-usec)))
    (when (> gcb-live-n 0) (setq gcb-live (gcb-build-live gcb-live-n)))
    (garbage-collect)
    (princ (format "GCB-BUILD live-n=%d build-us=%d\n"
                   gcb-live-n (- (nl-unix-time-usec) t0))))
  (let* ((n gcb-max-turns)
         (ts (make-vector (1+ n) 0))
         (fired (make-vector (1+ n) 0))
         (i 0))
    (aset ts 0 (nl-unix-time-usec))
    (aset fired 0 (gcb-fired))
    (while (< i n)
      (if (eq gcb-mode 'str) (gcb-step-str gcb-turn-alloc) (gcb-step-cons gcb-turn-alloc))
      (setq i (1+ i))
      (aset ts i (nl-unix-time-usec))
      (aset fired i (gcb-fired)))
    (let ((quiet nil) (coll nil) (k 0))
      (while (< k n)
        (let ((el (- (aref ts (1+ k)) (aref ts k))))
          (if (> (aref fired (1+ k)) (aref fired k))
              (setq coll (cons (list k el) coll))
            (setq quiet (cons el quiet))))
        (setq k (1+ k)))
      (setq coll (nreverse coll))
      (let* ((qmed (gcb-median (gcb-sorted quiet)))
             (pauses nil)
             (c coll))
        (while c
          (let* ((e (car c)) (p (- (nth 1 e) qmed)))
            (setq pauses (cons (if (< p 0) 0 p) pauses))
            (princ (format "GCB-COLL turn=%d pause-us=%d\n" (nth 0 e) p)))
          (setq c (cdr c)))
        (setq pauses (nreverse pauses))
        (princ (format "GCB-RESULT collections=%d median-pause-us=%d max-pause-us=%d turns=%d wall-us=%d heap-end=%d quiet-median-us=%d\n"
                       (length pauses)
                       (gcb-median (gcb-sorted pauses))
                       (gcb-max pauses)
                       n
                       (- (aref ts n) (aref ts 0))
                       (gcb-reserved)
                       qmed))))))

(gcb-run)

;;; nelisp-gc-pause-budget-driver.el ends here
