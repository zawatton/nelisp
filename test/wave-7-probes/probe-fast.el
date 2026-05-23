;;; probe-fast.el --- Wave 9 fast diff verifier (~21s baseline)  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_BENCH_N=5 \
;;     ./target/release/nelisp --batch -l test/wave-7-probes/probe-fast.el
;;
;; 観測 (2026-05-23、main ed6659b9):
;;   - lambda special form overhead = ~21s 一回 (= N=5/10/20 で同じ)
;;   - lambda is the actual cost driver、N=3-5 で差分計測十分
;;
;; 出力 = 単に N 個の lambda 構築、shell 側で `time` か $SECONDS で
;; 計測。N=5 default で baseline 21s、fix で 5x なら ~4s、2x なら ~10s。
;;
;; Wave 9 R7 系 fix の verify gate に使用 (= probe-stress / probe-208
;; のような cumulative bench は 100s+ 必要、本 probe は 21s 1 回で済む)。

(let ((n (string-to-number (or (getenv "NELISP_BENCH_N") "5")))
      (i 0))
  (while (< i n)
    (setq i (1+ i))
    (lambda () 42)))
(princ "done\n")
