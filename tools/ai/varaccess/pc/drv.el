;;; -*- lexical-binding: t; -*-
(let ((d (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name (if (equal (getenv "PC_FLAV") "bc") "bc.el" "src.el") d) nil t))
(let* ((mode (getenv "PC_MODE")) (n (string-to-number (getenv "BENCH_N")))
       (fn (intern (concat "a34-r-" mode)))
       (f (a34-mk)))
  (funcall fn 50 f)
  (princ (format "PC-CHECK mode=%s n=%d result=%S\n" mode n (funcall fn n f))))
(kill-emacs 0)
