;;; emacs-cc-dispnew-1.el --- dispnew.c batch primitives -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-dispnew-1--cursor-state (make-hash-table :test #'eq))

(defun emacs-cc-dispnew-1--check-frame (frame)
  "Signal the framep type error for FRAME."
  (unless (framep frame)
    (signal 'wrong-type-argument (list 'framep frame))))

(defun emacs-cc-dispnew-1--check-live-frame (frame)
  "Signal the frame-live-p type error for FRAME."
  (unless (frame-live-p frame)
    (signal 'wrong-type-argument (list 'frame-live-p frame))))

(defun emacs-cc-dispnew-1--check-window (window)
  "Signal the windowp type error for WINDOW."
  (unless (windowp window)
    (signal 'wrong-type-argument (list 'windowp window))))

(unless (fboundp 'display--update-for-mouse-movement)
  (defun display--update-for-mouse-movement (mouse-frame mouse-x mouse-y)
    "Handle mouse movement detected by Lisp code.

This function should be called when Lisp code detects the mouse has
moved, even if `track-mouse' is nil.  This handles updates that do not
rely on input events such as updating display for mouse-face
properties or updating the help echo text."
    (emacs-cc-dispnew-1--check-frame mouse-frame)
    (unless (integerp mouse-x)
      (signal 'wrong-type-argument (list 'integerp mouse-x)))
    (unless (integerp mouse-y)
      (signal 'wrong-type-argument (list 'integerp mouse-y)))
    nil))

(unless (fboundp 'frame-or-buffer-changed-p)
  (defun frame-or-buffer-changed-p (&optional variable)
    "Return non-nil if the frame and buffer state appears to have changed.
VARIABLE is a variable name whose value is either nil or a state vector
that will be updated to contain all frames and buffers,
aside from buffers whose names start with space,
along with the buffers' read-only and modified flags.  This allows a fast
check to see whether buffer menus might need to be recomputed.
If this function returns non-nil, it updates the internal vector to reflect
the current state.

If VARIABLE is nil, an internal variable is used.  Users should
not pass nil for VARIABLE."
    (let* ((symbol (or variable 'emacs-cc-dispnew-1--saved-state))
           (_ (unless (symbolp symbol)
                (signal 'wrong-type-argument (list 'symbolp symbol))))
           (state
            (vconcat
             (append
              (list (and (fboundp 'frame-list)
                         (mapcar (lambda (frame)
                                   (list frame
                                         (and (fboundp 'frame-visible-p)
                                              (frame-visible-p frame))))
                                 (frame-list))))
              (cl-loop for buffer in (buffer-list)
                       unless (string-prefix-p " " (buffer-name buffer))
                       collect
                       (with-current-buffer buffer
                         (list buffer buffer-read-only
                               (buffer-modified-p)))))))
           (old (and (boundp symbol) (symbol-value symbol))))
      (unless (boundp symbol)
        (set symbol nil))
      (if (equal old state)
          nil
        (set symbol state)
        t))))

(unless (fboundp 'frame--z-order-lessp)
  (defun frame--z-order-lessp (a b)
    "Internal frame sorting function A < B."
    (emacs-cc-dispnew-1--check-frame a)
    (emacs-cc-dispnew-1--check-frame b)
    nil))

(unless (fboundp 'internal-show-cursor)
  (defun internal-show-cursor (window show)
    "Set the cursor-visibility flag of WINDOW to SHOW.
WINDOW nil means use the selected window.  SHOW non-nil means
show a cursor in WINDOW in the next redisplay.  SHOW nil means
don't show a cursor."
    (let ((target (or window (selected-window))))
      (emacs-cc-dispnew-1--check-window target)
      (puthash target (and show t) emacs-cc-dispnew-1--cursor-state))
    nil))

(unless (fboundp 'internal-show-cursor-p)
  (defun internal-show-cursor-p (&optional window)
    "Value is non-nil if next redisplay will display a cursor in WINDOW.
WINDOW nil or omitted means report on the selected window."
    (let ((target (or window (selected-window))))
      (emacs-cc-dispnew-1--check-window target)
      (if (gethash target emacs-cc-dispnew-1--cursor-state 'emacs-cc-dispnew-1--unset)
          t
        (gethash target emacs-cc-dispnew-1--cursor-state)))))

(unless (fboundp 'redraw-frame)
  (defun redraw-frame (&optional frame)
    "Clear frame FRAME and output again what is supposed to appear on it.
If FRAME is omitted or nil, the selected frame is used."
    (emacs-cc-dispnew-1--check-live-frame (or frame (selected-frame)))
    nil))

(provide 'emacs-cc-dispnew-1)
