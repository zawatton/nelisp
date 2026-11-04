;;; emacs-cc-census-chars-w101.el --- Keyboard input coding system  -*- lexical-binding: t; -*-

(unless (fboundp 'keyboard-coding-system)
  (defun keyboard-coding-system (&optional terminal)
    "Return the coding system used to decode keyboard input on TERMINAL.
TERMINAL may be a live terminal, a live frame, or nil for the selected
frame's terminal.  A nil keyboard coding setting means `no-conversion'."
    (unless (or (null terminal)
                (and (fboundp 'terminal-live-p)
                     (terminal-live-p terminal))
                (and (framep terminal)
                     (fboundp 'frame-live-p)
                     (frame-live-p terminal)))
      (signal 'wrong-type-argument (list 'terminal-live-p terminal)))
    ;; The standalone coding setter stores its setting in this variable.
    ;; Its frame backend currently shares one keyboard input stream.
    (if (boundp 'keyboard-coding-system)
        (or (symbol-value 'keyboard-coding-system) 'no-conversion)
      'utf-8-unix)))

(provide 'emacs-cc-census-chars-w101)
;;; emacs-cc-census-chars-w101.el ends here
