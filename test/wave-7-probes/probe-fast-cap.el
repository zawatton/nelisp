;;; probe-fast-cap.el --- Wave 9 captured-vars diff verifier  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_BENCH_N=5 \
;;     ./target/release/nelisp --batch -l test/wave-7-probes/probe-fast-cap.el
;;
;; probe-fast.el の lambda noi 版 と対比、closure capture が必要な場合
;; (= depth = 1+) 専用。R7 観測 (2026-05-23、main ed6659b9):
;;   - captured-vars N=100 = 95.2s
;;   - captured-vars N=5 (推定) = ~25s
;;
;; 出力 = N 個の captured lambda、shell 側 time で計測。

(let ((n (string-to-number (or (getenv "NELISP_BENCH_N") "5")))
      (i 0))
  (while (< i n)
    (setq i (1+ i))
    (let ((captured i)) (lambda () captured))))
(princ "done\n")
