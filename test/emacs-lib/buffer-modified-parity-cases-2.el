;;; buffer-modified-parity-cases-2.el --- flag returns, erase, restore and silent edits -*- lexical-binding: t; -*-
;; One probe form per line for buffer-modified-parity.sh; groups keep each run under the meter cap.
(with-temp-buffer (insert "x") (set-buffer-modified-p nil) (erase-buffer) (buffer-modified-p))
(with-temp-buffer (erase-buffer) (buffer-modified-p))
(with-temp-buffer (insert "a") (set-buffer-modified-p nil) (insert "") (buffer-modified-p))
(with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (goto-char 1) (buffer-modified-p))
(with-temp-buffer (insert "abc") (list (restore-buffer-modified-p nil) (buffer-modified-p)))
(with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (list (restore-buffer-modified-p t) (buffer-modified-p)))
(with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (with-silent-modifications (insert "z")) (list (buffer-modified-p) (buffer-string)))
(with-temp-buffer (insert "abc") (with-silent-modifications (insert "z")) (buffer-modified-p))
(let ((b (generate-new-buffer "bm"))) (prog1 (list (buffer-modified-p b) (progn (with-current-buffer b (insert "q")) (buffer-modified-p b)) (buffer-modified-p)) (kill-buffer b)))
(with-temp-buffer (set-buffer-auto-saved) (recent-auto-save-p))
