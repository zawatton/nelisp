;;; buffer-modified-parity-cases.el --- modified flag basics -*- lexical-binding: t; -*-
;; One probe form per line for buffer-modified-parity.sh; groups keep each run under the meter cap.
(with-temp-buffer (buffer-modified-p))
(with-temp-buffer (insert "x") (buffer-modified-p))
(with-temp-buffer (insert "x") (set-buffer-modified-p nil) (buffer-modified-p))
(with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (delete-region 1 2) (buffer-modified-p))
(with-temp-buffer (insert "x") (with-temp-buffer (buffer-modified-p)))
(with-temp-buffer (let ((a (current-buffer))) (insert "x") (with-temp-buffer (list (buffer-modified-p) (buffer-modified-p a)))))
(with-temp-buffer (set-buffer-modified-p t) (buffer-modified-p))
(with-temp-buffer (set-buffer-modified-p t) (set-buffer-modified-p nil) (buffer-modified-p))
(with-temp-buffer (insert "x") (set-buffer-modified-p nil) (let ((a (current-buffer))) (with-temp-buffer (insert "y") (buffer-modified-p a))))
(with-temp-buffer (list (set-buffer-modified-p 5) (buffer-modified-p)))
