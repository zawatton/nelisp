;;; emacs-cc-read-character-1.el --- Exclusive character input -*- lexical-binding: t; -*-

(require 'emacs-command-loop)
(defvar inhibit-interaction nil)
(unless (get 'inhibited-interaction 'error-conditions)
  (define-error 'inhibited-interaction "User interaction is inhibited"))

(defun emacs-cc-read-character-1--ascii-event (event)
  "Convert symbolic EVENT to its ASCII equivalent when it has one."
  (if (not (symbolp event)) event
    (let* ((mask (get event 'event-symbol-element-mask))
           (base (if mask (car mask) event))
           (character (or (get base 'ascii-character)
                          (cdr (assq base '((return . 13) (tab . 9)
                                           (linefeed . 10) (escape . 27)
                                           (space . 32) (backspace . 8)
                                           (delete . 127)))))))
      (if (fixnump character)
          (logior character (if (and mask (fixnump (cadr mask))) (cadr mask) 0))
        event))))

(defun emacs-cc-read-character-1--read-exclusive (arguments)
    "Read a character, skipping other events and honoring input methods."
    (when (> (length arguments) 3)
      (signal 'wrong-number-of-arguments
              (list 'read-char-exclusive (length arguments))))
    (when (and (boundp 'inhibit-interaction) inhibit-interaction)
      (signal 'inhibited-interaction nil))
    (let* ((prompt (car arguments))
           (inherit (cadr arguments))
           (seconds (caddr arguments))
           (deadline (and (numberp seconds) (+ (float-time) seconds)))
           (done nil) event delayed-switch-frame)
      (when prompt (message "%s" prompt))
      (while (not done)
        (let ((remaining (and deadline (max 0 (- deadline (float-time))))))
          (condition-case nil
              (setq event
                    (emacs-cc-read-character-1--ascii-event
                     (emacs-command-loop-read-event
                      prompt inherit
                      (if deadline remaining
                        (and emacs-command-loop-input-poll-function 0.05)))))
            (emacs-command-loop-no-input
             (setq event nil)
             (cond
              ((and deadline (<= (- deadline (float-time)) 0)) (setq done t))
              ((not emacs-command-loop-input-poll-function)
               (signal 'end-of-file nil))
              (t (sleep-for 0.001)))))
          (when (fixnump event)
            (setq event (char-resolve-modifiers event) done t))
          (when (and (consp event) (eq (car event) 'switch-frame))
            (setq delayed-switch-frame event))))
      (when delayed-switch-frame
        (setq emacs-command-loop--unread-events
              (cons delayed-switch-frame emacs-command-loop--unread-events)))
      event))

(unless (fboundp 'read-char-exclusive)
  (defun read-char-exclusive (&rest arguments)
    "Read a character from the shared command input substrate."
    (emacs-cc-read-character-1--read-exclusive arguments)))

(provide 'emacs-cc-read-character-1)
;;; emacs-cc-read-character-1.el ends here
