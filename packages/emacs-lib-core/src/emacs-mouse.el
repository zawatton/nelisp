;;; emacs-mouse.el --- Shared positioned mouse events and commands -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'emacs-frame-pixels)
(require 'emacs-keymap)
(require 'emacs-command-loop)
(defvar emacs-mouse--down nil)
(defvar emacs-mouse--dragging nil)
(defvar emacs-mouse--last-posn nil)
(defvar emacs-mouse--motion-starts (make-hash-table :test 'eq))
(defvar emacs-mouse--marks (make-hash-table :test 'eq))

(defun emacs-mouse-mark (&optional buffer)
  "Return BUFFER's shared mouse mark for either supported buffer owner."
  (let ((buffer (or buffer (emacs-window-buffer (emacs-window-selected-window)))))
    (if (nelisp-ec-buffer-p buffer)
        (let ((marker (gethash buffer emacs-mouse--marks)))
          (and marker (nelisp-ec-marker-position marker)))
      (with-current-buffer buffer (mark t)))))

(defun emacs-mouse-transport-event (type button x y timestamp &optional _modifiers)
  "Convert raw press/release/motion to standard events using realized pixels.
TYPE is press, release or motion.  The reusable editor owns click/drag
classification and position construction; frontends supply transport only."
  (let ((posn (emacs-frame-pixels-hit x y timestamp)))
    (setq emacs-mouse--last-posn posn)
    (cond
     ((null posn) nil)
     ((and (eq type 'press) (memq button '(4 5)))
      (list (if (= button 4) 'wheel-up 'wheel-down) posn))
     ((and (eq type 'press) (= button 1))
      (setq emacs-mouse--down posn emacs-mouse--dragging nil)
      (list 'down-mouse-1 posn))
     ((and (eq type 'motion) emacs-mouse--down)
      (setq emacs-mouse--dragging
            (or emacs-mouse--dragging (not (equal (nth 2 posn) (nth 2 emacs-mouse--down)))))
      (let ((event (list 'mouse-movement posn)))
        ;; A whole transport batch can include release before dispatch runs.
        ;; Keep each canonical motion event associated with its own gesture.
        (puthash event emacs-mouse--down emacs-mouse--motion-starts)
        event))
     ((and (eq type 'release) (= button 1) emacs-mouse--down)
      (let ((start emacs-mouse--down) (drag emacs-mouse--dragging))
        (setq emacs-mouse--down nil emacs-mouse--dragging nil)
        (if drag (list 'drag-mouse-1 start posn) (list 'mouse-1 posn))))
     ((and (eq type 'press) (= button 3)) (list 'down-mouse-3 posn))
     (t nil))))

(defun emacs-mouse-set-point (event)
  "Select EVENT's window and move point to the hit buffer character."
  (interactive "e")
  (let* ((posn (nth 1 event)) (window (car posn)) (position (nth 1 posn)))
    (when (and (emacs-window-windowp window) (integerp position))
      (emacs-window-select-window window)
      (setq mark-active nil deactivate-mark t)
      (let ((buffer (emacs-window-buffer window)))
        (if (nelisp-ec-buffer-p buffer)
            (progn (nelisp-ec-set-buffer buffer) (nelisp-ec-goto-char position)
                   (emacs-window-set-window-point window position))
          (select-window window) (goto-char position) (set-window-point window position))))))

(defun emacs-mouse-set-region (event)
  "Set the region from EVENT's starting and ending character positions."
  (interactive "e")
  (let ((start (nth 1 event)) (end (nth 2 event)))
    (when (and end (eq (car start) (car end)))
      (emacs-mouse-set-point (list 'mouse-1 end))
      (let ((buffer (emacs-window-buffer (car end))))
        (if (nelisp-ec-buffer-p buffer)
            (let ((marker (or (gethash buffer emacs-mouse--marks) (nelisp-ec-make-marker))))
              (nelisp-ec-set-marker marker (nth 1 start) buffer)
              (puthash buffer marker emacs-mouse--marks))
          (set-mark (nth 1 start))))
      (setq mark-active t deactivate-mark nil))))

(defun emacs-mouse-track (event)
  "Update an active drag without reading transport or blocking input."
  (interactive "e")
  (let ((start (gethash event emacs-mouse--motion-starts)))
    (remhash event emacs-mouse--motion-starts)
    (when start
      (emacs-mouse-set-region (list 'drag-mouse-1 start (nth 1 event))))))

(defun emacs-mouse-scroll (event)
  "Scroll three shared window lines for a standard wheel EVENT."
  (interactive "e")
  (let ((window (car (nth 1 event))))
    (if (nelisp-ec-buffer-p (emacs-window-buffer window))
        (if (eq (car event) 'wheel-up) (emacs-window-scroll-down window 3)
          (emacs-window-scroll-up window 3))
      (select-window window)
      (if (eq (car event) 'wheel-up) (scroll-down 3) (scroll-up 3)))))

(defun emacs-mouse-install-bindings (keymap)
  "Install reusable positioned input commands in KEYMAP."
  (dolist (entry '((down-mouse-1 . emacs-mouse-set-point) (mouse-1 . emacs-mouse-set-point)
                   (drag-mouse-1 . emacs-mouse-set-region) (mouse-movement . emacs-mouse-track)
                   (wheel-up . emacs-mouse-scroll) (wheel-down . emacs-mouse-scroll)))
    (emacs-keymap-define-key keymap (vector (car entry)) (cdr entry))))

(provide 'emacs-mouse)
