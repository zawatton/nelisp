;;; standalone-hash-table-growth-check.el --- hash tables grow and still answer correctly -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-hash-table-growth-check.el
;;
;; Tables start with 8 buckets and double once the count passes twice the
;; bucket count.  The count that drives this lives in the marker as an
;; immediate word; it used to be written into a boxed copy and stay 0, so no
;; table ever grew.  Checked here:
;;
;; - growth: a fresh table has at most 8 buckets, inserting 70 integers
;;   grows it to at least 32 (the native doubling policy reaches 64; a line
;;   whose elisp `puthash' manages its own capacity starts at 1 and grows
;;   faster), and the marker's count is 70;
;; - correctness: 3000 seeded random puthash/remhash/gethash operations per
;;   test (eq, eql, equal) over integer, symbol, string, cons and float keys
;;   answer exactly like an alist reference, through growth, `clrhash',
;;   `copy-hash-table' and `maphash' (the entry set, not the order).
;;
;; Prints one HASH-GROWTH line and exits non-zero on failure.

(defvar hash-growth-check--seed 12345)

(defun hash-growth-check--rand (n)
  (setq hash-growth-check--seed
        (% (+ (* hash-growth-check--seed 1103515245) 12345) 2147483648))
  (% (/ hash-growth-check--seed 65536) n))

(defun hash-growth-check--key (test i)
  ;; Keys that are identical objects for eq, so every test can find them.
  (let ((k (% i 5)))
    (cond ((= k 0) i)
          ((= k 1) (intern (format "hgc-sym-%d" i)))
          ((eq test 'eq) i)
          ((= k 2) (format "str-%d" i))
          ((= k 3) (list 'pair i))
          (t (/ i 4.0)))))

(defun hash-growth-check--assoc (test key alist)
  (let ((rest alist) (hit nil))
    (while (and rest (not hit))
      (when (cond ((eq test 'eq) (eq key (car (car rest))))
                  ((eq test 'eql) (eql key (car (car rest))))
                  (t (equal key (car (car rest)))))
        (setq hit (car rest)))
      (setq rest (cdr rest)))
    hit))

(defun hash-growth-check--entries (table)
  (let ((out nil))
    (maphash (lambda (k v) (setq out (cons (cons k v) out))) table)
    out))

(defun hash-growth-check--same-set-p (test a b)
  (and (= (length a) (length b))
       (let ((ok t))
         (dolist (e a ok)
           (let ((f (hash-growth-check--assoc test (car e) b)))
             (unless (and f (equal (cdr f) (cdr e))) (setq ok nil)))))))

(defun hash-growth-check--run (test)
  (let* ((keys (let ((v (make-vector 400 nil)) (i 0))
                 (while (< i 400) (aset v i (hash-growth-check--key test i)) (setq i (1+ i)))
                 v))
         (table (make-hash-table :test test))
         (ref nil) (bad nil) (op 0))
    (while (and (not bad) (< op 3000))
      (let* ((key (aref keys (hash-growth-check--rand 400)))
             (r (hash-growth-check--rand 10)))
        (cond
         ((< r 5)
          (puthash key op table)
          (let ((cell (hash-growth-check--assoc test key ref)))
            (if cell (setcdr cell op) (setq ref (cons (cons key op) ref)))))
         ((< r 7)
          (remhash key table)
          (let ((cell (hash-growth-check--assoc test key ref)))
            (when cell (setq ref (delq cell ref)))))
         (t
          (let ((cell (hash-growth-check--assoc test key ref)))
            (unless (equal (gethash key table 'missing) (if cell (cdr cell) 'missing))
              (setq bad (list 'gethash op key))))))
        (unless (or bad (= (hash-table-count table) (length ref)))
          (setq bad (list 'count op (hash-table-count table) (length ref))))
        (when (and (not bad) (= (% op 500) 499))
          (unless (hash-growth-check--same-set-p test (hash-growth-check--entries table) ref)
            (setq bad (list 'maphash op)))
          (let ((copy (copy-hash-table table)))
            (unless (hash-growth-check--same-set-p test (hash-growth-check--entries copy) ref)
              (setq bad (list 'copy op)))))
        (setq op (1+ op))))
    (unless bad
      (clrhash table)
      (unless (and (= (hash-table-count table) 0)
                   (null (hash-growth-check--entries table)))
        (setq bad (list 'clrhash))))
    bad))

(let* ((h (make-hash-table)) (i 0) (fresh (length (cdr h))))
  (while (< i 70)
    (puthash i i h)
    (setq i (1+ i)))
  (let* ((growth-ok (and (<= 1 fresh 8)
                         (>= (length (cdr h)) 32)
                         (equal (aref (car h) 1) 70)))
         (results (mapcar (lambda (test) (cons test (hash-growth-check--run test)))
                          '(eq eql equal)))
         (ok (and growth-ok (not (delq nil (mapcar #'cdr results))))))
    (princ (format "HASH-GROWTH growth=%S results=%S %s\n"
                   growth-ok results (if ok "PASS" "FAIL")))
    (kill-emacs (if ok 0 1))))

;;; standalone-hash-table-growth-check.el ends here
