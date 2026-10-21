;; -*- lexical-binding: t; -*-
;; Doc 225 P3 invariant: the standard case table's canonicalize table maps
;; every Unicode scalar exactly as the matcher's built-in fold does, so
;; case-table--search-canon may answer nil for it.  Run on a built binary:
;;   BIN --load tools/ai/syntax-rt-canon-scan.el   (about 7 minutes)
;; Does the standard canonicalize table equal the matcher's built-in fold?
(let* ((canon (case-table--get-extra-slot (standard-case-table) 1))
       (c 0) (bad 0) (first nil) (t0 (float-time)))
  (while (<= c #x10ffff)
    (let* ((v (aref canon c)) (a (if (and (integerp v) (>= v 0)) v c)) (b (nlre--translate-char c nil)))
      (unless (eq a b) (setq bad (1+ bad)) (unless first (setq first (list c a b)))))
    (setq c (1+ c)))
  (princ (format "checked=%d mismatches=%d first=%S %.1fs\n" c bad first (- (float-time) t0))))
