;;; probe-stress.el --- Wave 7 fset-loop stress (R6h hang reproducer)  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_STRESS_N=60 timeout 30 \
;;     ./target/release/nelisp --batch -l test/wave-7-probes/probe-stress.el
;;
;; 既知 hang trigger: N ≥ 14 で deterministic CPU 100% loop
;; (R6i subagent stress 確認、Phase-47 .o の latent bug、commit `fe3ae8f3`
;; 2026-05-18 以降の record-slot-set rsp-alignment trampoline が原因候補)。
;;
;; 出力: `[N] OK` を hang まで 1 行ずつ、最後に `hang or completed at N=X`。
;; 進捗 print が止まったら hang、最後の N 値が trigger 境界の指標。

(let ((n (string-to-number (or (getenv "NELISP_STRESS_N") "60")))
      (i 0))
  (while (< i n)
    (setq i (1+ i))
    (let ((sym (intern (format "stress-fn-%d" i))))
      (fset sym (lambda () i))
      (princ (format "[%d] OK\n" i))))
  (princ (format "completed all %d fset\n" n)))
