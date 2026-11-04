;;; emacs-cc-fringe-1.el --- Fringe C-core primitives -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'fringe-bitmaps-at-pos)
  (defun fringe-bitmaps-at-pos (&optional pos window)
    "Return fringe bitmaps of row containing position POS in window WINDOW.
If WINDOW is nil, use selected window.  If POS is nil, use value of point
in that window.  Return value is a list (LEFT RIGHT OV), where LEFT
is the symbol for the bitmap in the left fringe (or nil if no bitmap),
RIGHT is similar for the right fringe, and OV is non-nil if there is an
overlay arrow in the left fringe.
Return nil if POS is not visible in WINDOW."
    (let* ((win (or window (selected-window)))
           (_display (and (boundp 'window-system) window-system))
           (buffer (condition-case nil (window-buffer win) (error nil)))
           (buffer (if (and (fboundp 'bufferp) (bufferp buffer))
                       buffer
                     (current-buffer)))
           (position (or pos (if (and (fboundp 'window-point) (window-point win))
                                 (window-point win)
                               (with-current-buffer buffer (point)))))
           (minimum (with-current-buffer buffer (point-min)))
           (maximum (with-current-buffer buffer (point-max))))
      (unless (or (integerp position) (and (markerp position) (marker-position position)))
        (signal 'wrong-type-argument (list 'integer-or-marker-p position)))
      (unless _display
        (setq buffer nil))
      (when buffer
      (unless (and (>= position minimum) (<= position maximum))
        (signal 'args-out-of-range (list win position)))
      (when (and (or (not (fboundp 'window-live-p)) (window-live-p win))
                 (or (not (fboundp 'pos-visible-in-window-p))
                     (condition-case nil (pos-visible-in-window-p position win)
                       (error nil))))
        (list nil nil nil))))))

(provide 'emacs-cc-fringe-1)
