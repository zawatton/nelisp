;;; emacs-cc-pgtkfns-1.el --- pgtkfns primitives -*- lexical-binding: t; -*-

(unless (fboundp 'pgtk-backend-display-class)
  (defun pgtk-backend-display-class (&optional terminal)
    "Return the name of the Gdk backend display class of TERMINAL."
    (if (and terminal (not (framep terminal)) (not (stringp terminal))
             (not (and (fboundp 'terminal-live-p) (terminal-live-p terminal))))
        (signal 'wrong-type-argument (list 'frame-live-p terminal))
      (error "Frames are not in use or not initialized"))))

(unless (fboundp 'pgtk-display-monitor-attributes-list)
  (defun pgtk-display-monitor-attributes-list (&optional terminal)
    "Return physical monitor attributes for TERMINAL."
    (if (and terminal (not (framep terminal)) (not (stringp terminal))
             (not (and (fboundp 'terminal-live-p) (terminal-live-p terminal))))
        (signal 'wrong-type-argument (list 'frame-live-p terminal))
      (error "Frames are not in use or not initialized"))))

(unless (fboundp 'pgtk-font-name)
  (defun pgtk-font-name (name)
    "Determine font PostScript or family name for font NAME."
    (unless (stringp name) (signal 'wrong-type-argument (list 'stringp name)))
    name))

(unless (fboundp 'pgtk-frame-edges)
  (defun pgtk-frame-edges (&optional frame type)
    "Return edge coordinates of FRAME."
    (let ((f (or frame (selected-frame))))
      (unless (frame-live-p f) (signal 'wrong-type-argument (list 'frame-live-p f)))
      (unless (memq type '(nil native-edges outer-edges inner-edges))
        (signal 'error (list "Invalid frame edge type" type)))
      (error "Window system frame should be used"))))

(unless (fboundp 'pgtk-frame-geometry)
  (defun pgtk-frame-geometry (&optional frame)
    "Return geometric attributes of FRAME."
    (let ((f (or frame (selected-frame))))
      (unless (frame-live-p f) (signal 'wrong-type-argument (list 'frame-live-p f)))
      (error "Window system frame should be used"))))

(unless (fboundp 'pgtk-frame-restack)
  (defun pgtk-frame-restack (frame1 frame2 &optional above)
    "Restack FRAME1 below FRAME2, or above if ABOVE is non-nil."
    (unless (frame-live-p frame1) (signal 'wrong-type-argument (list 'frame-live-p frame1)))
    (unless (frame-live-p frame2) (signal 'wrong-type-argument (list 'frame-live-p frame2)))
    (error "Window system frame should be used")))

(unless (fboundp 'pgtk-get-page-setup)
  (defun pgtk-get-page-setup ()
    "Return the current page setup."
    '((orientation . portrait) (width . 559.2755905511812)
      (height . 783.5697637795276) (left-margin . 18.0) (right-margin . 18.0)
      (top-margin . 18.0) (bottom-margin . 40.32000000000001))))

(unless (fboundp 'pgtk-mouse-absolute-pixel-position)
  (defun pgtk-mouse-absolute-pixel-position ()
    "Return absolute position of mouse cursor in pixels."
    (error "Window system frame should be used")))

(unless (fboundp 'pgtk-page-setup-dialog)
  (defun pgtk-page-setup-dialog ()
    "Pop up a page setup dialog."
    (error "Window system frame should be used")))

(unless (fboundp 'pgtk-print-frames-dialog)
  (defun pgtk-print-frames-dialog (&optional frames)
    "Pop up a print dialog for FRAMES."
    (let ((fs (cond ((null frames) (list (selected-frame))) ((framep frames) (list frames))
                    ((consp frames) frames) (t (signal 'wrong-type-argument (list 'framep frames))))))
      (dolist (f fs) (unless (frame-live-p f) (signal 'wrong-type-argument (list 'frame-live-p f))))
      (error "Window system frame should be used"))))

(unless (fboundp 'pgtk-set-monitor-scale-factor)
  (defun pgtk-set-monitor-scale-factor (monitor-model scale-factor)
    "Set MONITOR-MODEL's scale factor to SCALE-FACTOR."
    (unless (stringp monitor-model) (signal 'wrong-type-argument (list 'stringp monitor-model)))
    scale-factor))

(unless (fboundp 'pgtk-set-mouse-absolute-pixel-position)
  (defun pgtk-set-mouse-absolute-pixel-position (x y)
    "Move mouse pointer to absolute pixel position (X, Y)."
    (unless (numberp x) (signal 'wrong-type-argument (list 'numberp x)))
    (unless (numberp y) (signal 'wrong-type-argument (list 'numberp y)))
    (error "Window system frame should be used")))

(provide 'emacs-cc-pgtkfns-1)
