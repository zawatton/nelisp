;;; -*- lexical-binding: t; -*-

(let ((face 'par-init-face-store)
      (alias 'par-init-face-alias)
      (frame (selected-frame)))
  (make-face face)
  (put alias 'face-alias face)
  (internal-set-lisp-face-attribute alias :weight 'bold t)
  (internal-set-lisp-face-attribute face :weight 'light frame)
  (prin1 (list 'separate
               (internal-get-lisp-face-attribute face :weight t)
               (internal-get-lisp-face-attribute alias :weight frame)))
  (terpri)
  (set-face-attribute alias nil :weight 'normal)
  (prin1 (list 'broadcast
               (internal-get-lisp-face-attribute face :weight t)
               (internal-get-lisp-face-attribute alias :weight frame)))
  (terpri)
  (prin1 (list 'invalid
               (condition-case err
                   (internal-set-lisp-face-attribute alias :weight 42 frame)
                 (error (car err)))
               (internal-get-lisp-face-attribute alias :weight frame)))
  (terpri)
  (put face 'face-alias alias)
  (prin1 (list 'cycle
               (condition-case err
                   (internal-set-lisp-face-attribute face :weight 'bold t)
                 (error (car err)))))
  (terpri))
