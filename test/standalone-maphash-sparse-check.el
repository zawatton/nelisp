;;; standalone-maphash-sparse-check.el --- maphash cost follows the entry count -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-maphash-sparse-check.el
;;
;; A table that grew to 4000 entries and then lost 3998 of them keeps its
;; 2048-bucket vector (tables start small and grow, but never shrink).  `maphash' over it must cost at most 5x a loop
;; that funcalls the same function over a 2-element list: the cost must
;; follow the entry count, not the bucket count.  Also checks that `maphash' visits
;; entries in the order of the bucket walk it replaced (buckets ascending,
;; each bucket's nodes in order) and that FN may set the current entry's
;; value.  Prints one MAPHASH-SPARSE line and exits non-zero on failure.

(defun maphash-sparse--walk (table)
  "The entry order of the plain bucket walk."
  (let ((data (cdr table)) (i 0) (out nil))
    (while (< i (length data))
      (let ((node (aref data i)))
        (while (consp node)
          (when (consp (car node))
            (setq out (cons (cons (car (car node)) (cdr (car node))) out)))
          (setq node (cdr node))))
      (setq i (1+ i)))
    (nreverse out)))

(defun maphash-sparse--order (table)
  (let ((out nil))
    (maphash (lambda (k v) (setq out (cons (cons k v) out))) table)
    (nreverse out)))

(defun maphash-sparse--time (table)
  (garbage-collect)
  (let ((t0 (float-time)) (k 0) (sum 0))
    (while (< k 20)
      (maphash (lambda (_k v) (setq sum (+ sum v))) table)
      (setq k (1+ k)))
    (cons (- (float-time) t0) sum)))

(defun maphash-sparse--list-time (entries)
  (garbage-collect)
  (let ((t0 (float-time)) (k 0) (sum 0))
    (while (< k 20)
      (let ((fn (lambda (_k v) (setq sum (+ sum v)))) (rest entries))
        (while rest
          (funcall fn (car (car rest)) (cdr (car rest)))
          (setq rest (cdr rest))))
      (setq k (1+ k)))
    (cons (- (float-time) t0) sum)))

(let* ((sparse (make-hash-table :test 'equal))
       (mixed (make-hash-table :test 'equal))
       (i 0) (ok t))
  (while (< i 4000) (puthash i i sparse) (setq i (1+ i)))
  (setq i 2)
  (while (< i 4000) (remhash i sparse) (setq i (1+ i)))
  (setq i 0)
  (while (< i 60)
    (puthash (if (= (% i 3) 0) (format "k%d" i) i) (* i 10) mixed)
    (setq i (1+ i)))
  (setq i 0)
  (while (< i 60) (when (= (% i 7) 0) (remhash i mixed)) (setq i (1+ i)))
  (let* ((size (length (cdr sparse)))
         (order-ok (and (equal (maphash-sparse--order sparse) (maphash-sparse--walk sparse))
                        (equal (maphash-sparse--order mixed) (maphash-sparse--walk mixed))
                        (= (hash-table-count sparse) 2)
                        (>= size 2048)))
         (_ (maphash (lambda (k v) (puthash k (+ v 100) mixed)) mixed))
         (update-ok (let ((good t))
                      (maphash (lambda (k v)
                                 (unless (= v (+ 100 (* 10 (if (stringp k)
                                                                (string-to-number (substring k 1))
                                                              k))))
                                   (setq good nil)))
                               mixed)
                      good))
         (a (min (car (maphash-sparse--time sparse)) (car (maphash-sparse--time sparse))))
         (b (min (car (maphash-sparse--list-time '((0 . 0) (1 . 1))))
                 (car (maphash-sparse--list-time '((0 . 0) (1 . 1))))))
         (ratio (/ a (max 1e-6 b))))
    (setq ok (and order-ok update-ok (= (cdr (maphash-sparse--time sparse)) 20) (<= ratio 5.0)))
    (princ (format "MAPHASH-SPARSE buckets=%d maphash=%.3fms list-loop=%.3fms ratio=%.2f (limit 5) order=%s update=%s %s\n"
                   size (* 1000 (/ a 20)) (* 1000 (/ b 20)) ratio order-ok update-ok
                   (if ok "PASS" "FAIL")))
    (kill-emacs (if ok 0 1))))

;;; standalone-maphash-sparse-check.el ends here
