;;; emacs-cc-census-display-w202.el --- Frame and terminal setters  -*- lexical-binding: t; -*-

;;; Commentary:

;; Window setters and horizontal scrolling require the window provider to
;; accept native buffers and expose its dedicated, display-table and scroll
;; state.  Those providers are outside this unit.

;;; Code:

(defun emacs-cc-census-display-w202--live-frame (frame default)
  "Validate FRAME, using the selected frame when DEFAULT is non-nil."
  (let ((frame (if default (or frame (selected-frame)) frame)))
    (unless (frame-live-p frame)
      (signal 'wrong-type-argument (list 'frame-live-p frame)))
    frame))

(defun emacs-cc-census-display-w202--integer (value)
  "Return VALUE if it is an integer, otherwise signal a type error."
  (unless (integerp value)
    (signal 'wrong-type-argument (list 'integerp value)))
  value)

(defun emacs-cc-census-display-w202--terminal (terminal)
  "Resolve TERMINAL to the live terminal represented by the provider."
  (let ((target (or terminal (selected-frame))))
    (unless (or (frame-live-p target) (terminal-live-p target))
      (signal 'wrong-type-argument (list 'terminal-live-p terminal)))
    (if (frame-live-p target)
        (or (frame-terminal target) target)
      target)))

(unless (fboundp 'set-frame-height)
  (defun set-frame-height (frame height &optional pretend pixelwise)
    "Set FRAME's text height to HEIGHT lines, or pixels with PIXELWISE.
FRAME defaults to the selected frame.  PRETEND changes redisplay size only;
the initial batch terminal has fixed dimensions in either case."
    (interactive (list nil (prefix-numeric-value current-prefix-arg)))
    (let ((frame (emacs-cc-census-display-w202--live-frame frame t)))
      (emacs-cc-census-display-w202--integer height)
      ;; The size provider implements the initial terminal's fixed size.
      ;; A pretend resize needs no backend request on that terminal.
      (unless (and pretend (eq (framep frame) t))
        (set-frame-size frame
                        (if pixelwise (frame-pixel-width frame)
                          (frame-width frame))
                        height pixelwise))
      nil)))

(unless (fboundp 'set-frame-width)
  (defun set-frame-width (frame width &optional pretend pixelwise)
    "Set FRAME's text width to WIDTH columns, or pixels with PIXELWISE.
FRAME defaults to the selected frame.  PRETEND changes redisplay size only;
the initial batch terminal has fixed dimensions in either case."
    (interactive (list nil (prefix-numeric-value current-prefix-arg)))
    (let ((frame (emacs-cc-census-display-w202--live-frame frame t)))
      (emacs-cc-census-display-w202--integer width)
      (unless (and pretend (eq (framep frame) t))
        (set-frame-size frame width
                        (if pixelwise (frame-pixel-height frame)
                          (frame-height frame))
                        pixelwise))
      nil)))

(unless (fboundp 'set-mouse-position)
  (defun set-mouse-position (frame x y)
    "Move the mouse to the center of character cell X, Y in FRAME.
FRAME must be live, and X and Y must be integers.  Text terminals do not
support moving the mouse pointer."
    (emacs-cc-census-display-w202--live-frame frame nil)
    (emacs-cc-census-display-w202--integer x)
    (emacs-cc-census-display-w202--integer y)
    (when (window-system frame)
      (let ((width (frame-char-width frame))
            (height (frame-char-height frame)))
        (set-mouse-pixel-position frame
                                  (+ (* x width) (/ width 2))
                                  (+ (* y height) (/ height 2)))))
    nil))

(unless (fboundp 'set-terminal-parameter)
  (defun set-terminal-parameter (terminal parameter value)
    "Set TERMINAL's PARAMETER to VALUE and return its previous value.
TERMINAL can be a live terminal, a live frame, or nil for the selected
frame's terminal.  PARAMETER is compared by identity."
    (let* ((target (emacs-cc-census-display-w202--terminal terminal))
           ;; The terminal provider owns this alist; mutate its cells so
           ;; terminal-parameter observes the same state.
           (parameters (terminal-parameters target))
           (entry (assq parameter parameters))
           (previous (cdr entry)))
      (if entry
          (setcdr entry value)
        (if parameters
            (nconc parameters (list (cons parameter value)))
          (error "Terminal parameter storage is unavailable")))
      previous)))

(provide 'emacs-cc-census-display-w202)
;;; emacs-cc-census-display-w202.el ends here
