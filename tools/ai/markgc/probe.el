;;; -*- lexical-binding: t; -*-
;; markgc: unreferenced markers must stop costing anything.
;; Prints "markers=N edit-secs=S" for 10k dropped match-data markers + 1k edits.
(defun markgc--count (b)
  (let ((m (nelisp-buffer-markers b)))
    (cond ((hash-table-p m) (hash-table-count m)) (t (length m)))))
(with-temp-buffer
  (insert (make-string 200 ?a))
  (goto-char 1)
  (string-match "\\(a\\)\\(a\\)" "aab")
  (re-search-forward "\\(a\\)\\(a\\)" nil t)
  (let ((i 0)) (while (< i 10000) (match-data) (setq i (1+ i))))
  (garbage-collect) (garbage-collect)
  (let* ((b nelisp--current-buffer) (n (markgc--count b))
         (t0 (float-time)) (i 0))
    (while (< i 1000)
      (delete-region 10 11) (goto-char 10) (insert "x") (setq i (1+ i)))
    (let ((dt (- (float-time) t0)))
      (princ (format "markers=%d edit-secs=%.4f\n" n dt))
      (kill-emacs (if (and (< n 100) (< dt 0.5)) 0 1)))))
