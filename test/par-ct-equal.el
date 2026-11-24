;;; -*- lexical-binding: t; -*-
;; `equal' on char-tables (fns.c internal_equal: every slot, sub-char-tables
;; element by element).  Includes tables whose derived ranges view was rebuilt
;; on one side only (map-char-table) and tables with the same values but
;; different sub-char-table structure.  Spec = GNU 31.1 (par-ct-equal.gnu).
(defmacro row (n f) `(princ (format "%-26s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defun ce-fill (tb) (dotimes (i 40) (aset tb (- 70000 (* 997 i)) (% i 7))) tb)
(row "Q1 fresh" (equal (make-char-table 'ce) (make-char-table 'ce)))
(row "Q2 same writes" (equal (ce-fill (make-char-table 'ce)) (ce-fill (make-char-table 'ce))))
(row "Q3 one char differs" (let ((a (ce-fill (make-char-table 'ce))) (b (ce-fill (make-char-table 'ce)))) (aset b 50000 'x) (equal a b)))
(row "Q4 copy" (let ((a (ce-fill (make-char-table 'ce)))) (equal a (copy-sequence a))))
(row "Q5 map one side" (let* ((a (ce-fill (make-char-table 'ce))) (b (copy-sequence a))) (map-char-table #'ignore a) (equal a b)))
(row "Q6 map then differ" (let* ((a (ce-fill (make-char-table 'ce))) (b (copy-sequence a))) (map-char-table #'ignore a) (aset b 300 'y) (equal a b)))
(row "Q7 ascii differs" (let ((a (make-char-table 'ce)) (b (make-char-table 'ce))) (aset a ?x 1) (equal a b)))
(row "Q8 default differs" (equal (make-char-table 'ce 1) (make-char-table 'ce 2)))
(row "Q9 parent differs" (let ((a (make-char-table 'ce)) (b (make-char-table 'ce))) (set-char-table-parent a (make-char-table 'ce 3)) (equal a b)))
(row "Q10 subtype differs" (equal (make-char-table 'ce) (make-char-table 'ce2)))
(row "Q11 range vs per-char" (let ((a (make-char-table 'ce)) (b (make-char-table 'ce))) (set-char-table-range a '(#x3000 . #x30FF) 1) (let ((c #x3000)) (while (<= c #x30FF) (aset b c 1) (setq c (1+ c)))) (equal a b)))
(row "Q12 after optimize" (let ((a (make-char-table 'ce)) (b (make-char-table 'ce))) (set-char-table-range a '(#x3000 . #x30FF) 1) (let ((c #x3000)) (while (<= c #x30FF) (aset b c 1) (setq c (1+ c)))) (optimize-char-table b) (equal a b)))
(row "Q13 unaligned range" (let ((a (make-char-table 'ce)) (b (make-char-table 'ce))) (set-char-table-range a '(#x3010 . #x3020) 1) (let ((c #x3010)) (while (<= c #x3020) (aset b c 1) (setq c (1+ c)))) (equal a b)))
