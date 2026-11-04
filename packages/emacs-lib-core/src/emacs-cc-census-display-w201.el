;;; emacs-cc-census-display-w201.el --- Batch display primitives  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implement display queries and validation for the standalone's headless
;; frame.  Operations with device backends retain their existing backend.
;; Stack inspection and window glyph lookup need substrate support and are
;; deliberately not replaced here.

;;; Code:

(defvar track-mouse nil)
(defvar mouse-position-function nil)
(defvar window-configuration-change-hook nil)

(defun emacs-cc-census-display-w201--arity (name arguments minimum maximum)
  "Check the number of ARGUMENTS to NAME against MINIMUM and MAXIMUM."
  (let ((count (length arguments)))
    (unless (and (>= count minimum) (<= count maximum))
      (signal 'wrong-number-of-arguments (list name count)))))

(defun emacs-cc-census-display-w201--live-frame (frame)
  "Return FRAME if it is live, otherwise signal GNU's type error."
  (unless (frame-live-p frame)
    (signal 'wrong-type-argument (list 'frame-live-p frame)))
  frame)

(unless (fboundp 'face-font)
  (defun face-font (&rest arguments)
    "Return FACE's font, or its default bold and italic attributes on FRAME t.
The optional CHARACTER selects a glyph on graphical frames."
    (emacs-cc-census-display-w201--arity 'face-font arguments 1 3)
    (let ((face (car arguments)) (frame (cadr arguments)))
      (unless (eq frame t)
        (emacs-cc-census-display-w201--live-frame
         (or frame (selected-frame))))
      (when (stringp face) (setq face (intern face)))
      (unless (and face (symbolp face))
        (signal 'error (if face (list "Invalid face" face)
                         (list "Invalid face"))))
      ;; Attribute lookup resolves aliases and signals for nonexistent faces.
      (let ((weight (internal-get-lisp-face-attribute face :weight t))
            (slant (internal-get-lisp-face-attribute face :slant t)))
        (when (eq frame t)
          (append (unless (memq slant '(normal unspecified)) '(italic))
                  (unless (memq weight '(normal unspecified)) '(bold))))))))

(unless (fboundp 'internal--track-mouse)
  (defun internal--track-mouse (&rest arguments)
    "Call BODYFUN with mouse movement events enabled, restoring the binding."
    (emacs-cc-census-display-w201--arity
     'internal--track-mouse arguments 1 1)
    (let ((track-mouse t))
      (funcall (car arguments)))))

(unless (fboundp 'mouse-position)
  (defun mouse-position (&rest arguments)
    "Return the mouse position, applying `mouse-position-function' if set.
A headless frame has no mouse coordinates."
    (emacs-cc-census-display-w201--arity 'mouse-position arguments 0 0)
    (let ((position (cons (selected-frame) (cons nil nil))))
      (if mouse-position-function
          (funcall mouse-position-function position)
        position))))

(defun emacs-cc-census-display-w201--frame-candidate-p (frame minibuffer)
  "Return non-nil when FRAME satisfies MINIBUFFER's frame selection rule."
  (cond ((null minibuffer)
         (not (eq (frame-parameter frame 'minibuffer) 'only)))
        ((eq minibuffer 'visible) (eq (frame-visible-p frame) t))
        ((eq minibuffer 0) (memq (frame-visible-p frame) '(t iconified)))
        ((windowp minibuffer)
         (or (eq frame (window-frame minibuffer))
             (eq (minibuffer-window frame) minibuffer)))
        (t t)))

(unless (fboundp 'next-frame)
  (defun next-frame (&rest arguments)
    "Return the next live frame on FRAME's terminal matching MINIFRAME."
    (emacs-cc-census-display-w201--arity 'next-frame arguments 0 2)
    (let* ((frame (emacs-cc-census-display-w201--live-frame
                   (or (car arguments) (selected-frame))))
           (minibuffer (cadr arguments))
           (frames (frame-list))
           (tail (memq frame frames))
           (candidates (append (cdr tail) frames))
           (remaining (1- (length frames)))
           (terminal (frame-terminal frame))
           (result frame) (found nil))
      (while (and (> remaining 0) candidates (not found))
        (let ((candidate (car candidates)))
          (when (and (frame-live-p candidate)
                     (eq (frame-terminal candidate) terminal)
                     (emacs-cc-census-display-w201--frame-candidate-p
                      candidate minibuffer))
            (setq result candidate found t)))
        (setq candidates (cdr candidates) remaining (1- remaining)))
      result)))

(unless (fboundp 'open-termscript)
  (defun open-termscript (&rest arguments)
    "Write terminal output to FILE, or close the termscript for nil FILE.
The batch frame is not attached to a tty output device."
    (emacs-cc-census-display-w201--arity 'open-termscript arguments 1 1)
    (signal 'error (list "Current frame is not on a tty device"))))

(defun emacs-cc-census-display-w201--sound-properties (sound)
  "Validate SOUND's keyword argument list and return its properties."
  (unless (and (consp sound) (eq (car sound) 'sound))
    (signal 'error (list "Invalid sound specification")))
  (let ((tail (cdr sound)) (seen nil))
    (while tail
      (unless (consp tail)
        (signal 'wrong-type-argument (list 'listp tail)))
      (when (memq tail seen)
        (signal 'circular-list (list (cdr tail))))
      (setq seen (cons tail seen))
      (unless (cdr tail)
        (signal 'malformed-keyword-arg-list nil))
      (unless (consp (cdr tail))
        (signal 'wrong-type-argument (list 'listp (cdr tail))))
      (setq tail (cddr tail))))
  (cdr sound))

(unless (fboundp 'play-sound-internal)
  (defvar emacs-cc-census-display-w201--sound-backend
    (symbol-function 'play-sound-internal)
    "The sound backend installed before this validation layer.")
  (defun play-sound-internal (&rest arguments)
    "Validate SOUND's specification and pass it to the installed sound backend."
    (emacs-cc-census-display-w201--arity 'play-sound-internal arguments 1 1)
    (let* ((sound (car arguments))
           (properties (emacs-cc-census-display-w201--sound-properties sound))
           (file (plist-get properties :file))
           (data (plist-get properties :data))
           (volume (plist-get properties :volume))
           (device (plist-get properties :device)))
      (unless (and (or (stringp file) (stringp data))
                   (or (null volume)
                       (and (integerp volume) (<= 0 volume) (<= volume 100))
                       (and (floatp volume) (<= 0 volume) (<= volume 1)))
                   (or (null device) (stringp device)))
        (signal 'error (list "Invalid sound specification")))
      ;; The pinned bundle has no working playback backend.  Preserve that
      ;; limitation instead of fabricating playback or sound format results.
      (funcall emacs-cc-census-display-w201--sound-backend sound))))

(unless (fboundp 'redirect-frame-focus)
  (defvar emacs-cc-census-display-w201--focus-backend
    (symbol-function 'redirect-frame-focus)
    "The focus backend installed before this validation layer.")
  (defun redirect-frame-focus (&rest arguments)
    "Redirect FRAME's keyboard focus to FOCUS-FRAME, or cancel it for nil."
    (emacs-cc-census-display-w201--arity 'redirect-frame-focus arguments 1 2)
    (let ((frame (or (car arguments) (selected-frame)))
          (focus-frame (cadr arguments)))
      (unless (framep frame)
        (signal 'wrong-type-argument (list 'framep frame)))
      (when focus-frame
        (emacs-cc-census-display-w201--live-frame focus-frame))
      ;; Actual event routing remains the responsibility of the frame backend.
      (funcall emacs-cc-census-display-w201--focus-backend frame focus-frame))))

(unless (fboundp 'run-window-configuration-change-hook)
  (defun run-window-configuration-change-hook (&rest arguments)
    "Run local window configuration hooks on FRAME, then the default hook."
    (emacs-cc-census-display-w201--arity
     'run-window-configuration-change-hook arguments 0 1)
    (let ((frame (emacs-cc-census-display-w201--live-frame
                  (or (car arguments) (selected-frame)))))
      (save-current-buffer
        (dolist (window (window-list frame 'nomini))
          (let ((buffer (window-buffer window)))
            (when (and buffer
                       (local-variable-p 'window-configuration-change-hook buffer))
              (with-selected-window window
                ;; The default hook runs once after all local hooks, even
                ;; when a local hook list contains the usual t sentinel.
                (let ((window-configuration-change-hook
                       (remq t window-configuration-change-hook)))
                  (run-hooks 'window-configuration-change-hook))))))
        (let ((window-configuration-change-hook
               (default-value 'window-configuration-change-hook)))
          (when window-configuration-change-hook
            (with-selected-window (frame-selected-window frame)
              (run-hooks 'window-configuration-change-hook)))))
      nil)))

(provide 'emacs-cc-census-display-w201)
;;; emacs-cc-census-display-w201.el ends here
