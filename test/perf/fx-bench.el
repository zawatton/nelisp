;;; fx-bench.el --- word VM loop speed -*- lexical-binding: t -*-
;; Usage: BENCH=fx-loop [LIMIT=us] nelisp --load test/perf/fx-bench.el
;; tools/ai/eval-core-progress.org S1.3: microseconds per iteration of
;; (while (< i n) (setq i (1+ i))) run on the word VM, best of three.
(let* ((n 2000000)
       (form `(let ((i 0) (n ,n)) (while (< i n) (setq i (1+ i))) i))
       (c (nelisp--fx-compile form))
       (best nil))
  (unless c (error "fx-bench: the VM does not cover the bench form"))
  (dotimes (_ 3)
    (let ((t0 (float-time)))
      (unless (= (nelisp--fx-run (car c) (cdr c)) n) (error "fx-bench: wrong result"))
      (let ((us (/ (* 1e6 (- (float-time) t0)) n)))
        (when (or (null best) (< us best)) (setq best us)))))
  (let* ((limit (and (getenv "LIMIT") (string-to-number (getenv "LIMIT"))))
         (ok (or (null limit) (<= best limit))))
    (princ (format "FX-BENCH fx-loop %.4f us (limit %s) %s\n" best limit (if ok "PASS" "FAIL")))
    (kill-emacs (if ok 0 1))))
