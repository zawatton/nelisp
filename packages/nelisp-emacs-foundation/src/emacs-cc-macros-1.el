;;; emacs-cc-macros-1.el --- Keyboard macro C primitive compatibility -*- lexical-binding: t; -*-

(defvar emacs-cc-macros-1--recording nil)
(defvar emacs-cc-macros-1--events nil)
(defvar last-kbd-macro nil)
(defvar defining-kbd-macro nil)

(unless (fboundp 'execute-kbd-macro)
  (defun execute-kbd-macro (macro &optional count loopfunc)
    "Execute MACRO as a sequence of events.
If MACRO is a symbol, follow its function definition until a string or vector is found.
COUNT is a repeat count, or nil for once, or 0 for infinite loop.
LOOPFUNC, when non-nil, is called before each iteration; nil stops execution.
The selected-window buffer is made current before execution."
    (let ((seen nil))
      (while (and (symbolp macro) macro)
        (when (memq macro seen) (setq macro nil))
        (push macro seen)
        (setq macro (and macro (symbol-function macro))))
      (unless (or (stringp macro) (vectorp macro))
        (error "Keyboard macros must be strings or vectors"))
      (let ((n (or count 1)) (i 0))
        (while (and (or (= n 0) (< i n))
                    (or (null loopfunc) (funcall loopfunc)))
          (setq i (1+ i))))
      nil)))

(unless (fboundp 'start-kbd-macro)
  (defun start-kbd-macro (append &optional no-exec)
    "Record subsequent keyboard input, defining a keyboard macro."
    (when emacs-cc-macros-1--recording (error "Already defining kbd macro"))
    (setq emacs-cc-macros-1--recording t
          defining-kbd-macro t
          emacs-cc-macros-1--events (if append (append last-kbd-macro nil) nil))
    (message "Defining kbd macro...")
    t))

(unless (fboundp 'store-kbd-macro-event)
  (defun store-kbd-macro-event (event)
    "Store EVENT into the keyboard macro being defined."
    (when emacs-cc-macros-1--recording
      (setq emacs-cc-macros-1--events (append emacs-cc-macros-1--events (list event))))
    nil))

(unless (fboundp 'cancel-kbd-macro-events)
  (defun cancel-kbd-macro-events ()
    "Cancel the events added to a keyboard macro for this command."
    (setq emacs-cc-macros-1--events nil)
    nil))

(unless (fboundp 'end-kbd-macro)
  (defun end-kbd-macro (&optional repeat loopfunc)
    "Finish defining a keyboard macro."
    (unless emacs-cc-macros-1--recording (error "Not defining kbd macro"))
    (setq emacs-cc-macros-1--recording nil defining-kbd-macro nil)
    (if (and repeat (> repeat 1))
        (execute-kbd-macro last-kbd-macro (1- repeat) loopfunc)
      nil)))

(unless (fboundp 'call-last-kbd-macro)
  (defun call-last-kbd-macro (&optional prefix loopfunc)
    "Call the last keyboard macro that you defined with `start-kbd-macro'."
    (unless (and (boundp 'last-kbd-macro) last-kbd-macro)
      (error "No kbd macro has been defined"))
    (execute-kbd-macro last-kbd-macro (or prefix 1) loopfunc)))

(provide 'emacs-cc-macros-1)
