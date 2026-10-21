;;; regex-syncache-speed.el --- identical warm/measurement loop -*- lexical-binding: nil; -*-
;; The driver sets COUNT and PATTERN before loading this file.  COUNT is the
;; only difference between the low/high process pair; Callgrind counts startup
;; and GC in both, then the driver subtracts the process totals.
(let ((case-fold-search nil)
      (search-spaces-regexp nil)
      (text "unsigned_long")
      (i 0))
  (set-syntax-table (standard-syntax-table))
  (unless (equal (string-match regex-syncache-pattern text) 0)
    (error "SYNCACHE-SPEED: warm match failed"))
  (while (< i regex-syncache-count)
    (unless (equal (string-match regex-syncache-pattern text) 0)
      (error "SYNCACHE-SPEED: measured match failed"))
    (setq i (1+ i)))
  (princ (format "REGEXP-SYNCACHE speed %d PASS\n" i)))
nil
