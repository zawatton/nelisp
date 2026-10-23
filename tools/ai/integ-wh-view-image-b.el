;;; integ-wh-view-image-b.el --- string views in containers, restore side  -*- lexical-binding: t -*-

;; Run: BIN --cold-load-from PATH --load tools/ai/integ-wh-view-image-b.el   (PATH from integ-wh-view-image-a.el)
;; Prints one line per case, "VIEW-IMAGE name ok|FAIL", and a last line "VIEW-IMAGE PASS n/n" or "VIEW-IMAGE FAIL k".
;; The codes of (nelisp--debug-switch 81 CONS) are checked before any collection: 2 is a String WORD, 3 a view
;; that does not validate (a stale view of the dumping process), so a dump that left one fails here, not in a crash.

(defvar viewimg-fails 0)
(defvar viewimg-checks 0)
(defun viewimg-check (name ok)
  (setq viewimg-checks (1+ viewimg-checks))
  (unless ok (setq viewimg-fails (1+ viewimg-fails)))
  (princ (format "VIEW-IMAGE %s %s\n" name (if ok "ok" "FAIL"))))

(let ((c1 (nelisp--debug-switch 81 viewimg-one))
      (c2 (nelisp--debug-switch 81 viewimg-chain))
      (c3 (nelisp--debug-switch 81 (cdr viewimg-chain)))
      (c4 (nelisp--debug-switch 81 (cdr (cdr viewimg-chain))))
      (c5 (nelisp--debug-switch 81 viewimg-pair))
      (c6 (nelisp--debug-switch 81 (cdr viewimg-pair))))
  (viewimg-check "codes-no-stale-view" (not (memq 3 (list c1 c2 c3 c4 c6))))
  (viewimg-check "codes-are-strings" (equal (list c1 c2 c3 c4) '(2 2 2 2))))
(when (equal (nelisp--debug-switch 81 viewimg-one) 2)
  (viewimg-check "one-content" (equal (car viewimg-one) "view-canonical"))
  (viewimg-check "chain-content" (equal (list (car viewimg-chain) (car (cdr viewimg-chain))
                                              (car (cdr (cdr viewimg-chain))))
                                        (list "qqq" (string 960 961) "shared")))
  (viewimg-check "multibyte" (multibyte-string-p (car (cdr viewimg-chain))))
  (viewimg-check "unibyte" (not (multibyte-string-p (car viewimg-chain)))))
(when (equal (nelisp--debug-switch 81 viewimg-pair) 2)
  (viewimg-check "pair-content" (equal (car viewimg-pair) "alias")))
(princ "VIEW-IMAGE gc1\n")
(garbage-collect)
(let ((l nil) (i 0)) (while (< i 100000) (setq l (cons (make-string 6 ?a) l)) (setq i (1+ i))) (garbage-collect))
(viewimg-check "survives-collections"
               (and (equal (car viewimg-one) "view-canonical") (equal (car (cdr (cdr viewimg-chain))) "shared")))
;; the library's failing shape: the printed standard case table, a copied table with a modified pair, set-case-table
(viewimg-check "standard-case-table-prints" (> (length (format "%S" (standard-case-table))) 100000))
(set-case-table viewimg-ctab)
(viewimg-check "modified-pair-upcase" (and (= (upcase ?z) ?Q) (equal (upcase "z") "Q")))
(set-case-table (copy-case-table (standard-case-table)))
(garbage-collect)
(princ (if (= viewimg-fails 0)
           (format "VIEW-IMAGE PASS %d/%d\n" viewimg-checks viewimg-checks)
         (format "VIEW-IMAGE FAIL %d\n" viewimg-fails)))
