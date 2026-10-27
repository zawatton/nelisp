;;; -*- lexical-binding: t; -*-
(let ((d (file-name-directory (or load-file-name buffer-file-name)))) (load (expand-file-name "../pre3.el" d) nil t))
(defvar sv3-bigu (apply (function concat) (make-list 140000 "日本語abc\n"))) (defvar sv3-mbbuf (with-current-buffer (generate-new-buffer " mb") (insert sv3-bigu) (current-buffer))) (defvar sv3-asbuf (with-current-buffer (generate-new-buffer " as") (dotimes (_ 100000) (insert "abcdefghi\n")) (current-buffer)))
(defun sv3-mb-mbstr-string-match-far (n) (let ((a 0)) (dotimes (i n) (setq a (+ a (string-match "c\n日" sv3-bigu (+ 900000 (% i 1000)))))) a))
(let* ((n (string-to-number (getenv "BENCH_N"))))
  (sv3-mb-mbstr-string-match-far 2)
  (let* ((start (float-time)) (r (sv3-mb-mbstr-string-match-far n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
