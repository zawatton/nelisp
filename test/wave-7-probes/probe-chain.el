;;; probe-chain.el --- Wave 7 require chain probe  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_PROBE_REPO=/home/madblack-21/Cowork/Notes/dev/nelisp \
;;     ./target/release/nelisp --batch -l test/wave-7-probes/probe-chain.el
;;
;; (NELISP_PROBE_REPO 未設定時は default = `pwd` の親) — 必ず env で
;; absolute path を渡す。relative path は default-directory=/tmp/ 罠あり。
;;
;; 期待: 5 feature ok、exit=0、hang なし (timeout 60s 推奨)。
;; 失敗 (hang) = R6h regression pattern (= Phase-47 .o の latent bug)。

(let* ((root (or (getenv "NELISP_PROBE_REPO")
                 "/home/madblack-21/Cowork/Notes/dev/nelisp"))
       (lisp (concat root "/lisp")))
  (setq load-path (cons lisp load-path))
  (princ (format "load-path[0] = %s\n" lisp))
  (dolist (f '(nelisp-asm-arm64
               nelisp-asm-x86_64
               nelisp-elf-write
               nelisp-sexp-layout
               nelisp-phase47-compiler))
    (princ (format "  require %S ... " f))
    (require f)
    (princ "ok\n")))
(princ "OK probe-chain\n")
