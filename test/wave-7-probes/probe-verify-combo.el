;;; probe-verify-combo.el --- Wave 9+ fast combined verify  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_BENCH_N=5 \
;;     ./target/release/nelisp --batch -l test/wave-7-probes/probe-verify-combo.el
;;
;; 1 nelisp invocation で correctness + lambda perf + chain require の
;; 主要 verify を順次。startup overhead (~5s) を全 stage で amortize、
;; 個別 probe を順次 invoke (60-90s) より圧倒的に速い (15-20s 想定)。
;;
;; 期待出力:
;;   correct: pass (4/4)
;;   lambda(N=5): X.XX s
;;   chain (= optional skip if hang detected)
;;   COMBO-DONE
;;
;; shell 側 `time` で全体 wall を計測、各 stage の per-stage 時間は
;; print する。NELISP_PROBE_REPO unset 時 default repo path。

(let ((root (or (getenv "NELISP_PROBE_REPO")
                "/home/madblack-21/Cowork/Notes/dev/nelisp")))
  (setq load-path (cons (concat root "/lisp") load-path)))

;; --- Stage 1: correctness (= 4 scenario) ---
(princ "[combo] correctness ... ")
(let ((c1 (let ((x 1)) (let ((x 2)) x)))         ; expected 2 (= shadowing)
      (c2 (funcall (let ((c 42)) (lambda () c)))) ; expected 42 (= closure)
      (c3 (let ((a 10)) (let ((b 20)) (+ a b))))  ; expected 30 (= nested let)
      (fact (lambda (n) (if (<= n 1) 1 (* n (funcall fact (- n 1)))))))
  (let ((c4 nil))
    (fset 'cfact fact)
    (setq c4 (cfact 6))                          ; expected 720 (= recursion)
    (unless (and (= c1 2) (= c2 42) (= c3 30) (= c4 720))
      (princ (format "FAIL c1=%S c2=%S c3=%S c4=%S\n" c1 c2 c3 c4))
      (kill-emacs 1))
    (princ "pass (4/4)\n")))

;; --- Stage 2: lambda bench ---
(princ "[combo] lambda(N=5) ... ")
(let ((n (string-to-number (or (getenv "NELISP_BENCH_N") "5")))
      (i 0))
  (while (< i n)
    (setq i (1+ i))
    (lambda () 42))
  (princ "done\n"))

;; --- Stage 3: chain require (= optional、failure 時 skip) ---
(princ "[combo] chain require ... ")
(condition-case e
    (progn
      (dolist (f '(nelisp-asm-arm64
                   nelisp-asm-x86_64
                   nelisp-elf-write
                   nelisp-sexp-layout
                   nelisp-phase47-compiler))
        (require f))
      (princ "5/5 ok\n"))
  (t (princ (format "FAIL %S (= skip)\n" e))))

(princ "COMBO-DONE\n")
