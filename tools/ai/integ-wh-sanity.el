;;; integ-wh-sanity.el --- fib, a cons-heavy loop and a string-heavy loop  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1; both must print the same
;; bytes.  The workloads exist to catch a segfault or a wrong result after
;; the word-heap line is merged onto batch F, not to time anything.

(defun iwh-fib (n)
  (if (< n 2) n (+ (iwh-fib (- n 1)) (iwh-fib (- n 2)))))

(princ (format "fib(24)=%d\n" (iwh-fib 24)))

;; Cons-heavy: 200000 nested cells, a walk, nreverse and a tail lookup.
(let ((acc nil) (sum 0))
  (dotimes (i 200000)
    (push (cons i (list i (1+ i))) acc))
  (dolist (c acc)
    (setq sum (+ sum (car c) (nth 2 c))))
  (princ (format "cons-sum=%d len=%d\n" sum (length acc)))
  (setq acc (nreverse acc))
  (princ (format "cons-first=%S last=%S\n" (car acc) (car (last acc)))))

;; String-heavy: repeated concat, number-to-string, substring and format.
(let ((s "") (n 0))
  (dotimes (i 20000)
    (setq s (concat s (number-to-string i) ","))
    (when (> (length s) 2000)
      (setq n (+ n (length s)))
      (setq s "")))
  (princ (format "string-n=%d rest=%d\n" n (length s))))

(let ((total 0))
  (dotimes (i 50000)
    (let ((x (format "item-%05d:%s" i (substring "abcdefghij" (% i 5) (+ 5 (% i 5))))))
      (setq total (+ total (length x) (string-to-number (substring x 5 10))))))
  (princ (format "format-total=%d\n" total)))

;; A hash table and symbol names, since the strings and hashtab lines touch them.
(let ((h (make-hash-table :test 'equal)) (hits 0))
  (dotimes (i 5000)
    (puthash (format "k%d" i) i h))
  (dotimes (i 5000)
    (when (eq (gethash (format "k%d" (- 4999 i)) h) (- 4999 i))
      (setq hits (1+ hits))))
  (princ (format "hash-count=%d hits=%d name=%s\n"
                 (hash-table-count h) hits (symbol-name 'iwh-fib))))

(princ "INTEG-WH-SANITY DONE\n")
