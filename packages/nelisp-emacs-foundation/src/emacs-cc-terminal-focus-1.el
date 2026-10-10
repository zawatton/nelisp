;;; emacs-cc-terminal-focus-1.el --- terminal focus C primitive providers -*- lexical-binding: t; -*-

(defun emacs-cc-terminal-focus-1--install-provider-p (function)
  (or (not (fboundp function))
      (get function 'emacs-stub-bulk)))

(defun emacs-cc-terminal-focus-1--check-terminal (terminal)
  ;; GNU accepts a live frame or a live terminal object (as returned by
  ;; `frame-terminal'); evil's minibuffer setup passes the latter.
  (unless (or (and (fboundp 'frame-live-p) (frame-live-p terminal))
              (and terminal (fboundp 'terminal-live-p)
                   (terminal-live-p terminal)))
    (signal 'wrong-type-argument (list 'terminal-live-p terminal)))
  terminal)

(when (emacs-cc-terminal-focus-1--install-provider-p 'tty-top-frame)
  (defun tty-top-frame (&optional terminal)
    "Return the top frame on text TERMINAL, or nil when it is not a TTY.

The standalone stub frame backend has no attached text terminal.  An
interactive backend without a top-frame provider is rejected explicitly."
    (let ((frame (or terminal (selected-frame))))
      (emacs-cc-terminal-focus-1--check-terminal frame)
      (cond
       ((window-system) nil)
       ((and (fboundp 'emacs-frame-current-backend)
             (eq (emacs-frame-current-backend) 'stub))
        nil)
       (noninteractive nil)
       (t (error "TTY top-frame state is unavailable for this backend"))))))

(when (emacs-cc-terminal-focus-1--install-provider-p 'terminal-parameter)
  (defun terminal-parameter (terminal parameter)
    "Return TERMINAL's value for PARAMETER from its parameter alist."
    (let ((target (emacs-cc-terminal-focus-1--check-terminal
                   (or terminal (selected-frame)))))
      (unless (fboundp 'terminal-parameters)
        (error "Terminal parameter provider is unavailable"))
      (cdr (assq parameter (terminal-parameters target))))))

(provide 'emacs-cc-terminal-focus-1)
;;; emacs-cc-terminal-focus-1.el ends here
