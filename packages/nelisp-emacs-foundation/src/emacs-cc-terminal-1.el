;;; emacs-cc-terminal-1.el --- terminal.c primitives -*- lexical-binding: t; -*-

(defun emacs-cc-terminal-1--check-terminal (terminal)
  "Signal GNU-compatible error unless TERMINAL is live."
  (unless (or (and (fboundp 'terminal-live-p) (terminal-live-p terminal))
              (framep terminal))
    (signal 'wrong-type-argument (list 'terminal-live-p terminal))))

(defun emacs-cc-terminal-1--terminal-for-frame (frame)
  (or (and (fboundp 'frame-terminal) (frame-terminal frame)) frame))

(unless (fboundp 'frame-initial-p)
  (defun frame-initial-p (&optional frame)
    "Return non-nil if FRAME is the initial frame."
    (setq frame (or frame (selected-frame)))
    (and (framep frame)
         (or (eq frame (car (last (frame-list))))
             (eq frame (car (frame-list))))))
)

(unless (fboundp 'terminal-list)
  (defun terminal-list ()
    "Return a list of all terminal devices."
    (let* ((frame (selected-frame))
           (terminal (and frame
                          (emacs-cc-terminal-1--terminal-for-frame frame))))
      (list (or terminal t))))
)

(unless (fboundp 'terminal-name)
  (defun terminal-name (&optional terminal)
    "Return the name of terminal device TERMINAL."
    (setq terminal (or terminal (selected-frame)))
    (when (framep terminal)
      (setq terminal (emacs-cc-terminal-1--terminal-for-frame terminal)))
    (emacs-cc-terminal-1--check-terminal terminal)
    "initial_terminal"))

(unless (fboundp 'terminal-parameters)
  (defun terminal-parameters (&optional terminal)
    "Return the parameter-alist of terminal TERMINAL."
    (setq terminal (or terminal (selected-frame)))
    (when (framep terminal)
      (setq terminal (emacs-cc-terminal-1--terminal-for-frame terminal)))
    (emacs-cc-terminal-1--check-terminal terminal)
    '((normal-erase-is-backspace . 0) (keyboard-coding-saved-meta-mode t))))

(provide 'emacs-cc-terminal-1)
