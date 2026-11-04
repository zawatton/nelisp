;;; emacs-cc-pgtkim-1.el --- pgtkim primitives -*- lexical-binding: t; -*-

(unless (fboundp 'pgtk-use-im-context)
  (cl-defun pgtk-use-im-context (use-p &optional (terminal nil terminal-supplied-p))
    "Set whether to use GtkIMContext."
    (ignore use-p)
    (when terminal
      (unless (or (and (fboundp 'selected-frame)
                       (eq terminal (selected-frame)))
                  (and (fboundp 'frame-live-p) (frame-live-p terminal))
                  (framep terminal)
                  (and (fboundp 'terminal-live-p)
                       (terminal-live-p terminal)))
        ;; Standalone frame proxies are not recognized by the frame
        ;; predicates, but GNU accepts the selected frame and rejects it
        ;; because batch mode has no window-system frame.
        (unless (and (not (numberp terminal))
                     (not (symbolp terminal)))
          (signal 'wrong-type-argument (list 'frame-live-p terminal)))
        (error "Window system frame should be used"))
      (error "Window system frame should be used"))
    (if terminal-supplied-p
        (error "Window system frame should be used")
      (error "Frames are not in use or not initialized"))))

(provide 'emacs-cc-pgtkim-1)
