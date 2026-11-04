;;; -*- lexical-binding: t; -*-
(let ((d (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "src.el" d) nil t))
(let* ((mode (getenv "PC_MODE")) (n (string-to-number (getenv "BENCH_N")))
       (fn (intern (concat "fl-r-" mode))))
  (with-temp-buffer
    (setq fl-bl 70)
    (funcall fn 50)
    (princ (format "PC-CHECK mode=%s n=%d result=%S\n" mode n (funcall fn n)))))
(kill-emacs 0)
