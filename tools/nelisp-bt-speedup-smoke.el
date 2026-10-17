;;; nelisp-bt-speedup-smoke.el --- Doc 214 S8 real parallel speed-up -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214 S8.2 acceptance, run from the repository root:
;;   nelisp --load tools/nelisp-bt-speedup-smoke.el
;; The same CPU-bound job, a fixnum loop (no allocation, no shared state),
;; is split over 1 thread and then over 8.  BT-SPEEDUP-PASS needs the
;; 8-thread run to be at least 3x faster in wall-clock time, with the
;; same total.  Run on a machine with at least 8 idle cores.

;;; Code:

(load (expand-file-name "packages/nelisp-bt/src/nelisp-bt.el") nil t)

(defconst nl-bt-speedup--total 400000 "Loop iterations in all.")

(defun nl-bt-speedup--work (from to)
  "Return the sum of (i * 3) mod 7 for i in [FROM, TO)."
  (let ((i from) (acc 0))
    (while (< i to)
      (setq acc (+ acc (% (* i 3) 7)))
      (setq i (1+ i)))
    acc))

(defun nl-bt-speedup--run (n)
  "Run the job on N threads; return (SECONDS . TOTAL)."
  (let* ((t0 (float-time))
         (chunk (/ nl-bt-speedup--total n))
         (threads
          (let (ths (k 0))
            (while (< k n)
              (let ((from (* k chunk)) (to (* (1+ k) chunk)))
                (push (nl-bt-make-thread
                       (lambda () (nl-bt-speedup--work from to)))
                      ths))
              (setq k (1+ k)))
            ths))
         (total (apply #'+ (mapcar #'nl-bt-join-thread threads))))
    (cons (- (float-time) t0) total)))

(let* ((one (nl-bt-speedup--run 1))
       (eight (nl-bt-speedup--run 8))
       (ratio (/ (car one) (max (car eight) 0.001))))
  (princ (format "BT-SPEEDUP 1-thread %.2fs 8-thread %.2fs speedup %.2fx totals %S %S\n"
                 (car one) (car eight) ratio (cdr one) (cdr eight)))
  (princ (if (and (= (cdr one) (cdr eight)) (>= ratio 3.0))
             "BT-SPEEDUP-PASS\n" "BT-SPEEDUP-FAIL\n")))

;;; nelisp-bt-speedup-smoke.el ends here
