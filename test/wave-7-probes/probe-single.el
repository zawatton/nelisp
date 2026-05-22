;;; probe-single.el --- Wave 7 単体 probe (Record 系最小確認)  -*- lexical-binding: t -*-
;;
;; Usage: ./target/release/nelisp --batch -l test/wave-7-probes/probe-single.el
;; (絶対 path で呼ぶこと、relative path は default-directory=/tmp/ 罠あり)
;;
;; 期待: 3 行 OK 表示、exit=0。
;; 失敗 = Gate-WTA 未解決 (= R6h/R6i/R6j 系列で fix 必要)。

(let ((h (make-hash-table :test 'equal)))
  (princ (format "1. type-of hash-table = %S\n" (type-of h)))
  (princ (format "2. prin1 hash-table = %s\n" (prin1-to-string h)))
  (puthash "k" 42 h)
  (princ (format "3. puthash+gethash = %S\n" (gethash "k" h))))
(princ "OK probe-single\n")
