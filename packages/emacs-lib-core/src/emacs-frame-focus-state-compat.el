;;; emacs-frame-focus-state-compat.el --- GNU frame focus state Lisp -*- lexical-binding: t; -*-

(unless (fboundp 'frame-focus-state)
  (defun frame-focus-state (&optional frame)
    "Return FRAME's last known focus state, or `unknown'.

FRAME defaults to the selected frame.  This follows GNU frame.el's
focus-state rule: use the recorded frame state when no TTY top frame
exists, otherwise combine TTY top-frame, visibility, and terminal state."
    (let* ((target (or frame (selected-frame)))
           (top-frame (tty-top-frame target)))
      (if (not top-frame)
          (frame-parameter target 'last-focus-update)
        (cond ((not (eq top-frame target)) nil)
              ((not (frame-visible-p target)) nil)
              (t (let ((tty-focus-state
                        (terminal-parameter target 'tty-focus-state)))
                   (cond ((eq tty-focus-state 'focused) t)
                         ((eq tty-focus-state 'defocused) nil)
                         (t 'unknown)))))))))

(provide 'emacs-frame-focus-state-compat)
;;; emacs-frame-focus-state-compat.el ends here
