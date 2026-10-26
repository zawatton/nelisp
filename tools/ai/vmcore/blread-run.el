;; -*- lexical-binding: t -*-
(load (expand-file-name "blread-bc.el" (file-name-directory load-file-name)) nil t)
(dolist (r (vb-run)) (prin1 r) (terpri))
(let ((n 0) (s (vb-loop 1000))) (prin1 (list 'loop1000 s)) (terpri))
(kill-emacs 0)
