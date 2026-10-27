;;; -*- lexical-binding: t; -*-
(let ((d (file-name-directory (or load-file-name buffer-file-name)))) (load (expand-file-name "../pre3.el" d) nil t))

(defun sv3-hl-re-search-count-buf (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (with-temp-buffer (insert sv3-text) (goto-char 1) (let ((c 0)) (while (re-search-forward "\\<beta\\>" nil t) (setq c (1+ c))) c))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (sv3-hl-re-search-count-buf 2)
  (let* ((start (float-time)) (r (sv3-hl-re-search-count-buf n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
