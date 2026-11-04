;;; nelisp-display-batch.el --- honest batch display query -*- lexical-binding: t; -*-

;; This supports only NeLisp's implicit headless batch display.  It reports
;; zero only when the runtime confirms no selected frame, GUI, or color TTY;
;; richer display queries require real frame and terminal metadata.

(defun nl-display-batch-color-cells (&optional display)
  "Return zero for NeLisp's default headless batch display.
Signal an error for explicit displays or whenever the runtime cannot
establish that its implicit display has neither graphics nor a color TTY."
  (when display
    (error "Explicit display color count is unsupported: %S" display))
  (unless (and (boundp 'window-system)
               (null window-system)
               (fboundp 'selected-frame)
               (null (selected-frame))
               (fboundp 'display-graphic-p)
               (not (display-graphic-p))
               (fboundp 'tty-display-color-p)
               (not (tty-display-color-p)))
    (error "Cannot establish color count for this display"))
  0)

(unless (fboundp 'display-color-cells)
  (defalias 'display-color-cells #'nl-display-batch-color-cells))

(provide 'nelisp-display-batch)
;;; nelisp-display-batch.el ends here
