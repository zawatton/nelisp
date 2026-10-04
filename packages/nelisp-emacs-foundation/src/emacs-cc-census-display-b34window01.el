;;; emacs-cc-census-display-b34window01.el --- Window identity and redisplay bridges  -*- lexical-binding: t; -*-

(defun emacs-cc-census-display-b34window01--frame (frame)
  "Validate FRAME and return the selected frame when it is nil."
  (let ((target (or frame (selected-frame))))
    (unless (frame-live-p target)
      (signal 'wrong-type-argument (list 'frame-live-p target)))
    target))

(defun emacs-cc-census-display-b34window01--minibuffer-window (frame)
  "Return the stable minibuffer window associated with FRAME."
  (let* ((target (emacs-cc-census-display-b34window01--frame frame))
         (entry (and (emacs-frame-p target)
                     (assq 'minibuffer-window (emacs-frame-parameters target))))
         (window (cdr entry)))
    (unless (and window (window-valid-p window))
      (let ((root (or (and (emacs-frame-p target)
                           (emacs-frame-root-window target))
                      (and (eq target (selected-frame))
                           (bound-and-true-p emacs-window--root)))))
        (setq window
              (emacs-window--make
               :id (emacs-window--next-id)
               :buffer (get-buffer-create " *Minibuf-0*")
               :total-cols (if root
                               (emacs-window-total-cols root)
                             (frame-width target))
               :total-lines 1
               :top-line (if root
                             (+ (emacs-window-top-line root)
                                (emacs-window-total-lines root))
                           (1- (frame-height target)))
               :parameters (list (cons 'minibuffer t)))))
      (when (emacs-frame-p target)
        (setf (emacs-frame-parameters target)
              (cons (cons 'minibuffer-window window)
                    (assq-delete-all 'minibuffer-window
                                     (emacs-frame-parameters target))))))
      (when (eq target (selected-frame))
        (setq emacs-minibuffer--window window))
    window))

(unless (fboundp 'minibuffer-window)
  (defun minibuffer-window (&optional frame)
    "Return the minibuffer window belonging to FRAME."
    (emacs-cc-census-display-b34window01--minibuffer-window frame)))

(unless (fboundp 'window-minibuffer-p)
  (defun window-minibuffer-p (&optional window)
    "Return non-nil if WINDOW is a minibuffer window."
    (let ((target (or window (selected-window))))
      (unless (window-valid-p target)
        (signal 'wrong-type-argument (list 'window-valid-p target)))
      (and (eq t (window-parameter target 'minibuffer)) t))))

(defun emacs-cc-census-display-b34window01--window-minibuffer-p (window)
  "Return whether WINDOW is the minibuffer window after validating it."
  (let ((target (or window (selected-window))))
    (unless (window-valid-p target)
      (signal 'wrong-type-argument (list 'window-valid-p target)))
    (and (eq t (window-parameter target 'minibuffer)) t)))

(unless (fboundp 'set-frame-selected-window)
  (defun set-frame-selected-window (frame window &optional norecord)
    "Select WINDOW on FRAME and return WINDOW."
    (let ((target (emacs-cc-census-display-b34window01--frame frame)))
      (unless (window-live-p window)
        (signal 'wrong-type-argument (list 'window-live-p window)))
      (unless (eq target (window-frame window))
        (signal 'wrong-type-argument (list 'window-live-p window)))
      (if (eq target (selected-frame))
          (select-window window norecord)
        (unless (emacs-frame-p target)
          (signal 'wrong-type-argument (list 'frame-live-p target)))
        (setf (emacs-frame-parameters target)
              (cons (cons 'selected-window window)
                    (assq-delete-all 'selected-window
                                     (emacs-frame-parameters target)))))
      window)))

(unless (fboundp 'redisplay)
  (defun redisplay (&rest args)
    "Perform redisplay, returning t unless executing a keyboard macro."
    (when (cdr args)
      (signal 'wrong-number-of-arguments (list 'redisplay (length args))))
    (unless (and (boundp 'executing-kbd-macro) executing-kbd-macro)
      (let ((handle (emacs-redisplay-current-handle)))
        (when handle
          (emacs-redisplay-redisplay handle)))
      t)))

(unless (fboundp 'redraw-display)
  (defun redraw-display ()
    "Request a complete display refresh."
    (let ((handle (emacs-redisplay-current-handle)))
      (when handle
        (emacs-redisplay-redraw-display handle)))
    nil))

(provide 'emacs-cc-census-display-b34window01)
