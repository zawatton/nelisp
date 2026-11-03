;;; buffer-modified-parity-cases-3.el --- text properties and auto-save marks -*- lexical-binding: t; -*-
;; One probe form per line for buffer-modified-parity.sh; groups keep each run under the meter cap.
(with-temp-buffer (set-buffer-auto-saved) (insert "changed") (recent-auto-save-p))
(with-temp-buffer (insert "x") (set-buffer-auto-saved) (recent-auto-save-p))
(with-temp-buffer (insert "x") (set-buffer-auto-saved) (set-buffer-modified-p nil) (recent-auto-save-p))
(with-temp-buffer (insert "x") (set-buffer-auto-saved) (insert "y") (recent-auto-save-p))
(with-temp-buffer (insert "x") (set-buffer-auto-saved) (set-buffer-modified-p nil) (insert "y") (recent-auto-save-p))
(with-temp-buffer (insert "x") (set-buffer-modified-p nil) (set-buffer-modified-p t) (set-buffer-auto-saved) (recent-auto-save-p))
(with-temp-buffer (recent-auto-save-p))
(with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (put-text-property 1 2 'face 'bold) (buffer-modified-p))
(with-temp-buffer (insert "abc") (set-buffer-modified-p nil) (with-silent-modifications (put-text-property 1 2 'face 'bold)) (list (buffer-modified-p) (get-text-property 1 'face)))
