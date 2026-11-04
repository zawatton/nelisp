;;; hash-string-cases.el --- equal-table string-key differential cases  -*- lexical-binding: t -*-
;; Each case prints "NAME => RESULT".  Ends with "DONE n=N".  Targets the
;; string fast path in key comparison: word-wise compare, byte tail, lengths,
;; multibyte/unibyte, text properties, mutation and rehash during use.

(defvar hs-count 0)
(defmacro hs-case (name form)
  `(progn (setq hs-count (1+ hs-count))
          (princ (format "%s => %S\n" ,name
                         (condition-case e (list 'ok ,form) (error (list 'err (car e))))))))

(defun hs-fresh (s) (concat s ""))
(defun hs-table (keys)
  (let ((h (make-hash-table :test 'equal)) (i 0))
    (dolist (k keys) (puthash k i h) (setq i (1+ i)))
    h))

;; Lengths around the 8-byte word boundary; a mismatch in every position.
(let ((i 0))
  (while (<= i 20)
    (let* ((s (make-string i ?a)) (h (hs-table (list s))))
      (hs-case (format "len%02d.hit" i) (gethash (hs-fresh s) h))
      (let ((j 0))
        (while (< j i)
          (let ((v (hs-fresh s)))
            (aset v j ?b)
            (hs-case (format "len%02d.miss@%02d" i j) (gethash v h 'none)))
          (setq j (1+ j)))))
    (setq i (1+ i))))

;; Prefix keys: one key a prefix of another.
(let ((h (hs-table '("key" "key0" "key00000" "key000000" "key0000000000000000"))))
  (dolist (k '("key" "key0" "key00000" "key000000" "key0000000000000000"
               "ke" "key000" "key0000000" "key000000000000000"))
    (hs-case (concat "prefix." k) (gethash (hs-fresh k) h 'none))))

;; Multibyte, unibyte, raw bytes, text properties, empty string.
(let ((h (hs-table (list "abc" "ÄÖü" "日本語キー" "" (string-to-unibyte "raw\377x")))))
  (hs-case "mb.hit" (gethash (hs-fresh "日本語キー") h 'none))
  (hs-case "mb.latin1" (gethash (hs-fresh "ÄÖü") h 'none))
  (hs-case "mb.near" (gethash "日本語キ" h 'none))
  (hs-case "ascii.unibyte-lookup" (gethash (string-to-unibyte "abc") h 'none))
  (hs-case "ascii.multibyte-lookup" (gethash (string-to-multibyte "abc") h 'none))
  (hs-case "raw.unibyte" (gethash (string-to-unibyte "raw\377x") h 'none))
  (hs-case "raw.multibyte" (gethash (string-to-multibyte (string-to-unibyte "raw\377x")) h 'none))
  (hs-case "empty" (gethash (hs-fresh "") h 'none))
  (hs-case "props" (gethash (propertize "abc" 'face 'bold) h 'none)))

;; Non-string keys keep the generic path.
(let ((h (hs-table (list '(1 "a") [1 "b"] 1.5 'sym "s"))))
  (hs-case "list" (gethash (list 1 (hs-fresh "a")) h 'none))
  (hs-case "vector" (gethash (vector 1 (hs-fresh "b")) h 'none))
  (hs-case "float" (gethash 1.5 h 'none))
  (hs-case "symbol" (gethash 'sym h 'none))
  (hs-case "string-vs-symbol" (gethash "sym" h 'none)))

;; Mutation and growth (rehash) between lookups.
(let ((h (make-hash-table :test 'equal :size 4)) (i 0) (sum 0) (miss 0))
  (while (< i 3000)
    (puthash (format "k%05d" i) i h)
    (when (= (% i 7) 0) (remhash (format "k%05d" (/ i 2)) h))
    (let ((v (gethash (format "k%05d" (/ i 3)) h)))
      (if v (setq sum (+ sum v)) (setq miss (1+ miss))))
    (setq i (1+ i)))
  (hs-case "grow.count" (hash-table-count h))
  (hs-case "grow.sum" sum)
  (hs-case "grow.miss" miss)
  (hs-case "overwrite" (progn (puthash (hs-fresh "k00010") 'new h) (gethash "k00010" h))))

;; bench-hash-get1024 shape.
(let ((h (make-hash-table :test 'equal)) (j 0) (s 0))
  (while (< j 1024) (puthash (format "key%04d" j) (logand (+ (* 17 j) 3) 65535) h) (setq j (1+ j)))
  (setq j 0)
  (while (< j 4096)
    (setq s (logand (+ s (gethash (format "key%04d" (logand (+ (* 73 j) 19) 1023)) h)) 16777215))
    (setq j (1+ j)))
  (hs-case "bench-shape" s))

;; Long keys (sampled hash above 128 bytes), unibyte raw bytes, mixed representations.
(let* ((a200 (make-string 200 ?a))
       (u300 (let ((s (make-string 300 ?x))) (aset s 150 ?y) (string-to-unibyte s)))
       (raw (string-to-unibyte (concat (make-string 140 ?r) "\377\200")))
       (mb (concat (make-string 130 ?m) "日本語"))
       (h (hs-table (list a200 u300 raw mb))))
  (hs-case "long.ascii.hit" (gethash (hs-fresh a200) h 'none))
  (hs-case "long.ascii.miss@150" (gethash (let ((v (hs-fresh a200))) (aset v 150 ?b) v) h 'none))
  (hs-case "long.ascii.miss@199" (gethash (let ((v (hs-fresh a200))) (aset v 199 ?b) v) h 'none))
  (hs-case "long.unibyte.hit" (gethash (string-to-unibyte (let ((s (make-string 300 ?x))) (aset s 150 ?y) s)) h 'none))
  (hs-case "long.unibyte.as-multibyte" (gethash (let ((s (make-string 300 ?x))) (aset s 150 ?y) s) h 'none))
  (hs-case "long.raw.hit" (gethash (string-to-unibyte (concat (make-string 140 ?r) "\377\200")) h 'none))
  (hs-case "long.raw.multibyte" (gethash (string-to-multibyte raw) h 'none))
  (hs-case "long.mb.hit" (gethash (hs-fresh mb) h 'none))
  (hs-case "long.mb.near" (gethash (concat (make-string 130 ?m) "日本") h 'none))
  (hs-case "long.remhash" (progn (remhash (hs-fresh a200) h) (list (gethash a200 h 'gone) (hash-table-count h))))
  (hs-case "sxhash.copy-equal" (= (sxhash-equal mb) (sxhash-equal (hs-fresh mb))))
  (hs-case "sxhash.unibyte-ascii" (= (sxhash-equal "abc") (sxhash-equal (string-to-unibyte "abc")))))

;; A custom test still calls its callbacks exactly once per lookup.
(defvar hs-calls 0)
(define-hash-table-test 'hs-ci
  (lambda (a b) (setq hs-calls (1+ hs-calls)) (string= (downcase a) (downcase b)))
  (lambda (k) (setq hs-calls (1+ hs-calls)) (sxhash-equal (downcase k))))
(let ((h (make-hash-table :test 'hs-ci)))
  (puthash "Alpha" 1 h)
  (setq hs-calls 0)
  (hs-case "custom.hit" (gethash "ALPHA" h 'none))
  (hs-case "custom.calls" hs-calls))

(princ (format "DONE n=%d\n" hs-count))
(kill-emacs 0)
