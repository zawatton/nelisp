;;; nemacs-startup-mode-smoke.el --- Scratch/new-buffer isolation -*- lexical-binding: t; -*-

;; Run on GNU with emacs -Q --batch -l FILE, or on the C-core heap image.
;; The same observable state must survive leaving and returning to scratch.
(setq init-file-user nil inhibit-startup-screen t)
(if (fboundp 'nelisp--repr)
    (nemacs-init t)
  (lisp-interaction-mode))
(let ((scratch (current-buffer))
      (fresh (generate-new-buffer " *startup-plain*")))
  (unwind-protect
      (progn
        (unless (and (eq major-mode 'lisp-interaction-mode)
                     (equal mode-name "Lisp Interaction")
                     (eq (default-value 'major-mode) 'fundamental-mode))
          (error "Scratch changed the default major mode"))
        (set-buffer fresh)
        (unless (and (eq major-mode 'fundamental-mode)
                     (equal mode-name "Fundamental"))
          (error "New buffer inherited scratch mode state"))
        (set-buffer scratch)
        (unless (and (eq major-mode 'lisp-interaction-mode)
                     (equal mode-name "Lisp Interaction"))
          (error "Returning to scratch lost its mode state"))
        (princ "STARTUP-MODE-ISOLATION|PASS\n"))
    (kill-buffer fresh)))
t
