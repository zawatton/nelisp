;;; fx-call-bench.el --- word VM call cost -*- lexical-binding: t -*-
;; Usage: BENCH=vm-vm|interp-vm [LIMIT=us] [N=calls] nelisp --load test/perf/fx-call-bench.el
;; tools/ai/eval-core-progress.org S2.3 / S2.4: microseconds per call of a
;; 3-argument function the VM runs, called from VM code (vm-vm) or from an
;; interpreted loop (interp-vm), beyond the same loop without the call.  The
;; best of five base runs and the best of five call runs are subtracted, so
;; a collection landing in one run does not decide the answer.
(defun fx-call-bench-g (a _b _c) a)
(defun fx-call-bench-vm (n)
  (let ((i 0)) (while (< i n) (fx-call-bench-g i 2 3) (setq i (1+ i))) i))
(defun fx-call-bench-vm0 (n)
  (let ((i 0)) (while (< i n) (setq i (1+ i))) i))
(defun fx-call-bench-best (thunk)
  (let ((best nil) (k 0))
    (while (< k 5)
      (let ((t0 (float-time)))
        (funcall thunk)
        (let ((d (- (float-time) t0))) (when (or (null best) (< d best)) (setq best d))))
      (setq k (1+ k)))
    best))
(defun fx-call-bench-covered-p (name)
  (vectorp (gethash (nthcdr 3 (symbol-function name)) nelisp--fx-table)))
(let* ((name (or (getenv "BENCH") ""))
       (limit (and (getenv "LIMIT") (string-to-number (getenv "LIMIT"))))
       (n (if (getenv "N") (string-to-number (getenv "N")) 100000))
       (us
        (cond
         ((not (and (fx-call-bench-covered-p 'fx-call-bench-g)
                    (fx-call-bench-covered-p 'fx-call-bench-vm)))
          (princ "FX-CALL-BENCH the VM does not run the bench functions\n") nil)
         ((equal name "vm-vm")
          (/ (* 1e6 (- (fx-call-bench-best (lambda () (fx-call-bench-vm n)))
                       (fx-call-bench-best (lambda () (fx-call-bench-vm0 n)))))
             n))
         ((equal name "interp-vm")
          (let ((call `(let ((i 0)) (while (< i ,n) (fx-call-bench-g i 2 3) (setq i (1+ i)))))
                (empty `(let ((i 0)) (while (< i ,n) (setq i (1+ i))))))
            (/ (* 1e6 (- (fx-call-bench-best (lambda () (eval call t)))
                         (fx-call-bench-best (lambda () (eval empty t)))))
               n)))
         (t (princ (format "FX-CALL-BENCH unknown BENCH=%s; one of vm-vm interp-vm\n" name)) nil)))
       (ok (and us (or (null limit) (<= us limit)))))
  (when us
    (princ (format "FX-CALL-BENCH %s %.3f us (n %d, limit %s) %s\n" name us n limit
                   (if ok "PASS" "FAIL"))))
  (kill-emacs (if ok 0 1)))
