;;; standalone-regexp-tramp-check.el --- GNU path-handler parity -*- lexical-binding: nil; -*-
(load (expand-file-name "test/regex-tramp-corpus.el"))
(let ((count 0))
  (dolist (row rt-corpus)
    (when (< count 2)
      (unless (null (string-match (nth 1 row) (car (nth 2 row))))
        (error "Tramp handler accepted an ordinary absolute .el path"))
      (setq count (1+ count))))
  (unless (= count 2) (error "Missing pinned tramp handler patterns"))
  (princ (format "REGEXP-TRAMP %d cases PASS\n" count)))
nil
