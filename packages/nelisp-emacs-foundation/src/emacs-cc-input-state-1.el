;;; emacs-cc-input-state-1.el --- Batch input state and command events -*- lexical-binding: t; -*-

(defvar emacs-cc-input--lossage-size 300)
(defvar emacs-command-loop--waiting-for-input nil)

(unless (fboundp 'lossage-size)
  (defun lossage-size (&optional size)
    "Return the keystroke history limit, or change it to SIZE."
    (when size
      (unless (and (integerp size) (>= size 0))
        (user-error "Value must be a positive integer"))
      (when (< size 100) (user-error "Value must be >= 100"))
      (setq emacs-cc-input--lossage-size size))
    emacs-cc-input--lossage-size))

;; The bootstrap has a batch terminal, not a controlling tty.  GNU changes
;; none of these terminal fields in batch mode, even for an invalid QUIT.
(unless (fboundp 'set-input-interrupt-mode)
  (defun set-input-interrupt-mode (interrupt)
    "Set input interrupt mode when a controlling tty exists."
    (ignore interrupt)
    nil))

(defun emacs-cc-input--check-terminal (terminal)
  "Validate TERMINAL before a tty-only operation."
  (unless (or (null terminal) (frame-live-p terminal)
              (and (fboundp 'terminal-live-p) (terminal-live-p terminal)))
    (signal 'wrong-type-argument (list 'terminal-live-p terminal))))

(unless (fboundp 'set-input-meta-mode)
  (defun set-input-meta-mode (meta &optional terminal)
    "Set META handling for a tty TERMINAL."
    (emacs-cc-input--check-terminal terminal)
    (ignore meta)
    nil))

(unless (fboundp 'set-output-flow-control)
  (defun set-output-flow-control (flow &optional terminal)
    "Set output FLOW control for a tty TERMINAL."
    (emacs-cc-input--check-terminal terminal)
    (ignore flow)
    nil))

(unless (fboundp 'set-quit-char)
  (defun set-quit-char (quit)
    "Set the quitting character on a controlling tty."
    (ignore quit)
    nil))

(unless (fboundp 'waiting-for-user-input-p)
  (defun waiting-for-user-input-p ()
    "Return whether the command loop is awaiting user input."
    (and emacs-command-loop--waiting-for-input t)))

(unless (fboundp 'set--this-command-keys)
  (defun set--this-command-keys (keys)
    "Replace the command loop's accumulated key string with KEYS."
    (unless (stringp keys)
      (signal 'wrong-type-argument (list 'stringp keys)))
    (setq emacs-command-loop--this-command-keys (copy-sequence keys))
    nil))

(unless (fboundp 'insert-special-event)
  (defun insert-special-event (event)
    "Enqueue a supported special EVENT."
    (unless (consp event)
      (signal 'wrong-type-argument (list 'consp event)))
    (unless (memq (car event) '(delete-frame iconify-frame make-frame-visible
                              focus-in focus-out))
      (signal 'error (list "Invalid special event kind" (car event))))
    (emacs-command-loop-feed-events event)
    nil))

(provide 'emacs-cc-input-state-1)
;;; emacs-cc-input-state-1.el ends here
