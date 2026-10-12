;;; standalone-string-primitive-bench.el --- string primitive timings -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-string-primitive-bench.el
;;
;; Times 20000 calls of each string primitive the SKK merge code leans
;; on, on short ASCII and Japanese strings, and prints one
;; STRING-BENCH line per primitive with microseconds per call.

(defvar string-bench--a "abcdefghij/klmnop")
(defvar string-bench--b (copy-sequence "abcdefghij/klmnop"))
(defvar string-bench--ja "かんじ/漢字/ふりがな")
(defvar string-bench--jb (copy-sequence "かんじ/漢字/ふりがな"))

;; Each body runs inside a compiled-once function: top-level loop forms
;; in a loaded file re-expand their macros on every iteration, which
;; would swamp the primitive being timed.
(defmacro string-bench--time (label &rest body)
  `(progn
     (defalias 'string-bench--fn
       (lambda ()
         (let ((a string-bench--a) (b string-bench--b)
               (ja string-bench--ja) (jb string-bench--jb) (i 0))
           (ignore a b ja jb)
           (while (< i 20000) ,@body (setq i (1+ i))))))
     (let ((t0 (float-time)))
       (string-bench--fn)
       (princ (format "STRING-BENCH %-16s %8.2f us/call\n" ,label
                      (/ (* 1e6 (- (float-time) t0)) 20000))))))

(progn
  (string-bench--time "empty-loop" nil)
  (string-bench--time "substring" (substring a 3 9))
  (string-bench--time "substring-ja" (substring ja 2 6))
  (string-bench--time "string=" (string= a b))
  (string-bench--time "string=-ja" (string= ja jb))
  (string-bench--time "equal" (equal a b))
  (string-bench--time "string-prefix-p" (string-prefix-p "abc" a))
  (string-bench--time "string-search" (string-search "/" a))
  (string-bench--time "string-search-ja" (string-search "/" ja))
  (string-bench--time "concat" (concat a "x"))
  (string-bench--time "split-string" (split-string a "/" t))
  (string-bench--time "split-string-ja" (split-string ja "/" t))
  (string-bench--time "format" (format "%s-%d" a 3)))

;;; standalone-string-primitive-bench.el ends here
