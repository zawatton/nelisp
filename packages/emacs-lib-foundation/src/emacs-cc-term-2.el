;;; emacs-cc-term-2.el --- term.c C primitive replacements -*- lexical-binding: t; -*-

(unless (fboundp 'tty-type)
  (defun tty-type (&optional terminal)
    "Return the type of the tty device that TERMINAL uses, or nil if it is not on a tty device. TERMINAL can be a terminal object, a frame, or nil (meaning the selected frame's terminal)."
    (if (null terminal)
        nil
      (if (or (framep terminal)
              (and (fboundp 'terminal-live-p)
                   (terminal-live-p terminal)))
          nil
        (signal 'wrong-type-argument (list 'terminal-live-p terminal)))))
)

(provide 'emacs-cc-term-2)
