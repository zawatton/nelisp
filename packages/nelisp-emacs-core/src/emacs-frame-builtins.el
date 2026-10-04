;;; emacs-frame-builtins.el --- Unprefixed frame.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.C'' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* frame builtins (= `make-frame',
;; `framep', `selected-frame', `frame-parameter', ...) to the existing
;; `emacs-frame-*' prefixed implementations in `emacs-frame.el',
;; mirroring the Phase 11.B' `emacs-search-builtins.el' pattern.
;;
;; Why this exists: until Phase 11.C'' the unprefixed names lived as
;; nil-stubs inside `emacs-stub.el', so consumers calling `make-frame'
;; got a `(cons 'frame nil)' sentinel even though `emacs-frame.el'
;; provides a real frame model with parameters / size / backend
;; dispatch.  Bridging unifies the two namespaces.
;;
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub.el' no-op shims.
;;
;; Bridgeable today (= covered by `emacs-frame.el'):
;;
;;   - `make-frame' / `framep' / `frame-live-p' / `frame-list'
;;   - `selected-frame' / `window-frame'
;;   - `delete-frame' / `delete-other-frames'
;;   - `frame-width' / `frame-height' / `frame-char-width' /
;;     `frame-char-height' / `frame-pixel-width' / `frame-pixel-height'
;;   - `set-frame-size' / `set-frame-position'
;;   - `frame-parameter' / `frame-parameters'
;;   - `set-frame-parameter' / `modify-frame-parameters'
;;   - `frame-visible-p' / `make-frame-visible' /
;;     `make-frame-invisible' / `raise-frame' / `lower-frame'
;;   - `select-frame' / `frame-focus'
;;   - `frame-windows' / `display-pixel-width' / `display-pixel-height'

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- frames are the nemacs headless frame model.
;;; Code:

(require 'emacs-frame)

;; T62: `tool-bar-local-item' had an fboundp-guarded on-demand loader
;; (the magit bridge tool-bar-runtime helper) only inside
;; `src/nelisp-emacs-magit-bridge.el', which is not on the default boot
;; path.  The load matrix hit `(void-function tool-bar-local-item)' for
;; `geiser-guile', which is not magit at all.  The vendored
;; `vendor/emacs-lisp/tool-bar.el' loads cleanly on the standalone
;; runtime and defines `tool-bar-local-item' with no further unresolved
;; dependency, so force-load the real vendor module here instead of
;; duplicating its logic as a local stub -- this file is already loaded
;; ahead of every vendor/user package, same reasoning as
;; `coding-system-get' / `button-buffer-map' (T52).
;;
;; A plain `(require 'tool-bar)' fails here with "Cannot open load
;; file" specifically because this form runs while the concatenated
;; bootstrap bundle itself is being replayed, and the bundle's own
;; contract deliberately sets `load-file-name' to nil throughout (see
;; `build/nemacs-bootstrap.el''s header comment) -- `default-directory'
;; (set to the repo root by `bin/nemacs' before the bundle loads) is the
;; bundle-safe way to resolve a sibling path, the same fallback
;; `emacs-stub--load-directory' already relies on.  Loading the vendor
;; file directly by that absolute path, bypassing `require''s
;; `load-path' search, is robust to the bundle-replay context; a bare
;; `(require 'tool-bar)' after the bundle has fully loaded (i.e. once
;; `load-path' driven lookups are safe again) works fine too, but this
;; form runs before that point.
(unless (featurep 'tool-bar)
  (load (expand-file-name "vendor/emacs-lisp/tool-bar.el" default-directory)
        nil 'no-message t t))

(defvar emacs-frame-builtins--adopt-blank-frame-owner
  (and (fboundp 'nelisp--write-stdout-bytes)
       (or (not (fboundp 'selected-frame)) (null (selected-frame))))
  "Non-nil when standalone prelude frame accessors have no initial frame.
Use the shared frame model for the compatibility family.")

(defvar emacs-frame-builtins--initial-geometry
  (and emacs-frame-builtins--adopt-blank-frame-owner
       (list (frame-width) (frame-height)))
  "Existing standalone terminal dimensions before frame owner adoption.")
(defvar emacs-frame-builtins--owner-initialized nil)
(when (and emacs-frame-builtins--adopt-blank-frame-owner
           (not emacs-frame-builtins--owner-initialized))
  (setq emacs-frame-builtins--owner-initialized t)
  (unless emacs-frame--backend-dispatch
    (let ((frame (emacs-frame-selected-frame)))
      (setf (emacs-frame-width frame) (car emacs-frame-builtins--initial-geometry)
            (emacs-frame-height frame) (cadr emacs-frame-builtins--initial-geometry)
            (emacs-frame-pixel-width frame)
            (* (emacs-frame-width frame) emacs-frame--char-width)
            (emacs-frame-pixel-height frame)
            (* (emacs-frame-height frame) emacs-frame--char-height)))))

(defun emacs-frame-builtins--kind (kind)
  "Map shared terminal backend KIND to GNU's terminal frame marker."
  (if (memq kind '(stub tui t)) t kind))
(defun emacs-frame-builtins--framep (object)
  (emacs-frame-builtins--kind (emacs-frame-framep object)))
(defun emacs-frame-builtins--frame-live-p (object)
  (emacs-frame-builtins--kind (emacs-frame-frame-live-p object)))
(defun emacs-frame-builtins--check-live-frame (frame)
  "Validate shared FRAME for standalone face operations."
  (unless (emacs-frame-builtins--frame-live-p frame)
    (signal 'wrong-type-argument (list 'frame-live-p frame))))
(when emacs-frame-builtins--adopt-blank-frame-owner
  ;; The prelude's headless validator rejects every non-nil frame.  Face
  ;; creation during bootstrap must accept the same owner as frame-list.
  (defalias 'nelisp--check-live-frame
    #'emacs-frame-builtins--check-live-frame))
(defun emacs-frame-builtins--terminal-p (frame)
  (eq (emacs-frame-builtins--framep frame) t))
(defun emacs-frame-builtins--frame-char-width (&optional frame)
  (let ((frame (emacs-frame--get frame)))
    (if (emacs-frame-builtins--terminal-p frame) 1
      (emacs-frame-frame-char-width frame))))
(defun emacs-frame-builtins--frame-char-height (&optional frame)
  (let ((frame (emacs-frame--get frame)))
    (if (emacs-frame-builtins--terminal-p frame) 1
      (emacs-frame-frame-char-height frame))))
(defun emacs-frame-builtins--frame-pixel-width (&optional frame)
  (let ((frame (emacs-frame--get frame)))
    (if (emacs-frame-builtins--terminal-p frame)
        (/ (emacs-frame-pixel-width frame) emacs-frame--char-width)
      (emacs-frame-frame-pixel-width frame))))
(defun emacs-frame-builtins--frame-pixel-height (&optional frame)
  (let ((frame (emacs-frame--get frame)))
    (if (emacs-frame-builtins--terminal-p frame)
        (/ (emacs-frame-pixel-height frame) emacs-frame--char-height)
      (emacs-frame-frame-pixel-height frame))))

(defun emacs-frame-builtins-layout-terminal (frame width height)
  "Realize FRAME's menu bar and text area at WIDTH by HEIGHT cells.
The initial terminal size is reconciled on the first event wait, as in
GNU batch Emacs; text and window geometry change before cached dimensions."
  (unless (emacs-frame-terminal-size-ready frame)
    (unless (emacs-frame-terminal-size-pending frame)
      (setf (emacs-frame-terminal-size-pending frame)
            (cons (emacs-frame-builtins--frame-pixel-width frame)
                  (emacs-frame-builtins--frame-pixel-height frame)))))
  (let* ((menu (max 0 (or (frame-parameter frame 'menu-bar-lines) 1)))
         (width (max 10 width))
         (height (max 4 height)))
    (setf (emacs-frame-menu-bar-lines frame) menu
          (emacs-frame-pixel-width frame) (* width emacs-frame--char-width)
          (emacs-frame-pixel-height frame)
          (* (+ height menu) emacs-frame--char-height))
    (when (emacs-frame-terminal-size-ready frame)
      (setf (emacs-frame-width frame) width
            (emacs-frame-height frame) height))
    (emacs-window-layout-frame width height menu)))

(defun emacs-frame-builtins-reconcile-terminal-sizes ()
  "Apply pending initial terminal dimensions at an event wait."
  (dolist (frame emacs-frame--registry)
    (let ((pending (emacs-frame-terminal-size-pending frame)))
      (when pending
        (setf (emacs-frame-terminal-size-pending frame) nil
              (emacs-frame-terminal-size-ready frame) t)
        (emacs-frame-builtins-layout-terminal
         frame (car pending) (- (cdr pending) (emacs-frame-menu-bar-lines frame)))))))
(defun emacs-frame-builtins--set-frame-position (frame x y)
  (let ((frame (emacs-frame-builtins--get-live-frame frame)))
    (unless (integerp x) (signal 'wrong-type-argument (list 'integerp x)))
    (unless (integerp y) (signal 'wrong-type-argument (list 'integerp y)))
    (unless (emacs-frame-builtins--terminal-p frame)
      (emacs-frame-set-frame-position frame x y))
    t))
(defun emacs-frame-builtins--get-frame (frame)
  "Return FRAME or the selected frame, checking the frame object type."
  (let ((frame (or frame (emacs-frame-selected-frame))))
    (unless (emacs-frame-p frame)
      (signal 'wrong-type-argument (list 'framep frame)))
    frame))
(defun emacs-frame-builtins--get-live-frame (frame)
  "Return FRAME or the selected frame, requiring a live frame."
  (let ((frame (or frame (emacs-frame-selected-frame))))
    (emacs-frame-builtins--check-live-frame frame)
    frame))
(defun emacs-frame-builtins--window-frame (&optional window)
  "Return the frame owning valid WINDOW, defaulting to the selected window."
  (let ((window (or window (selected-window))))
    (unless (window-valid-p window)
      (signal 'wrong-type-argument (list 'window-valid-p window)))
    (let ((root window) (frames emacs-frame--registry) owner)
      (while (and frames (not owner))
        (when (eq window
                  (cdr (assq 'minibuffer-window
                             (emacs-frame-parameters (car frames)))))
          (setq owner (car frames)))
        (setq frames (cdr frames)))
      (setq frames emacs-frame--registry)
      (while (emacs-window-parent root)
        (setq root (emacs-window-parent root)))
      (while (and frames (not owner))
        (when (eq root (emacs-frame-root-window (car frames)))
          (setq owner (car frames)))
        (setq frames (cdr frames)))
      (or owner
          ;; The window module has one implicit frame until a backend
          ;; attaches its tree to a frame's root-window slot.
          (and (or (eq root emacs-window--root)
                   (and (boundp 'emacs-minibuffer--window)
                        (eq window emacs-minibuffer--window)))
               (if (emacs-frame-p emacs-window--frame)
                   emacs-window--frame
                 (emacs-frame-selected-frame)))))))
(defun emacs-frame-builtins--delete-frame (&optional frame force)
  "Delete FRAME, preserving GNU's last-frame errors and dead-frame no-op."
  (let ((frame (emacs-frame-builtins--get-frame frame)))
    (unless (emacs-frame-dead-p frame)
      (let ((others (delq frame (copy-sequence (emacs-frame-frame-list))))
            visible)
        (dolist (other others)
          (when (emacs-frame-visible other) (setq visible t)))
        (unless (or force visible)
          (error "Attempt to delete the sole visible or iconified frame"))
        (unless others (error "Attempt to delete the only frame"))
        (emacs-frame-delete-frame frame force)))))
(defun emacs-frame-builtins--set-frame-size (frame width height &optional pixelwise)
  "Set FRAME's text size after validating WIDTH and HEIGHT as integers."
  (let ((frame (emacs-frame-builtins--get-live-frame frame)))
    (unless (integerp width)
      (signal 'wrong-type-argument (list 'integerp width)))
    (unless (integerp height)
      (signal 'wrong-type-argument (list 'integerp height)))
    (if (and noninteractive (emacs-frame-builtins--terminal-p frame))
        (emacs-frame-builtins-layout-terminal frame width height)
      (emacs-frame-set-frame-size frame (max 2 width) (max 1 height) pixelwise))
    nil))
(defun emacs-frame-builtins--frame-parameters (&optional frame)
  "Return a fresh parameter alist for FRAME, or nil for a deleted frame."
  (let ((frame (emacs-frame-builtins--get-frame frame)))
    (unless (emacs-frame-dead-p frame)
      (mapcar (lambda (entry) (cons (car entry) (cdr entry)))
              (emacs-frame-frame-parameters frame)))))
(defun emacs-frame-builtins--frame-parameter (frame parameter)
  "Return FRAME's PARAMETER, checking the frame before the symbol."
  (let ((frame (emacs-frame-builtins--get-frame frame)))
    (unless (symbolp parameter)
      (signal 'wrong-type-argument (list 'symbolp parameter)))
    (cdr (assq parameter (emacs-frame-builtins--frame-parameters frame)))))
(defvar emacs-frame-builtins--automatic-name-counter 0
  "Last automatically generated frame name number.")
(defun emacs-frame-builtins--reset-frame-name (frame)
  "Restore an automatic name after FRAME had an explicit name."
  (let ((name (assq 'name (emacs-frame-parameters frame))))
    (when (and name (cdr name))
      (setq emacs-frame-builtins--automatic-name-counter
            (1+ (max emacs-frame-builtins--automatic-name-counter
                     emacs-frame--id-counter)))
      (setf (emacs-frame-name frame)
            (format "F%d" emacs-frame-builtins--automatic-name-counter)))
    ;; An absent entry marks an automatic name in the shared model.
    (setf (emacs-frame-parameters frame)
          (assq-delete-all 'name (emacs-frame-parameters frame)))))
(defun emacs-frame-builtins--modify-frame-parameters (frame alist)
  "Apply ALIST to live FRAME, checking list structure before modification."
  (let ((frame (emacs-frame-builtins--get-live-frame frame))
        (tail alist))
    (while (consp tail)
      (unless (listp (car tail))
        (signal 'wrong-type-argument (list 'listp (car tail))))
      (setq tail (cdr tail)))
    (unless (null tail)
      (signal 'wrong-type-argument (list 'listp tail)))
    (dolist (entry (reverse alist))
      (when (and (eq (car entry) 'name) (cdr entry)
                 (not (stringp (cdr entry))))
        (signal 'wrong-type-argument (list 'stringp (cdr entry))))
      (cond
       ((and (eq (car entry) 'name) (null (cdr entry)))
        (emacs-frame-builtins--reset-frame-name frame))
       ((and noninteractive (emacs-frame-builtins--terminal-p frame)
             (memq (car entry) '(width height visibility))))
       (t (emacs-frame-modify-frame-parameters frame (list entry)))))
    nil))
(defun emacs-frame-builtins--frame-visible-p (frame)
  "Return t, icon or nil for the visibility of explicit live FRAME."
  (emacs-frame-builtins--check-live-frame frame)
  (let ((visible (emacs-frame-visible frame)))
    (if (eq visible 'iconified) 'icon visible)))
(defun emacs-frame-builtins--make-frame-visible (&optional frame)
  "Make live FRAME visible, defaulting to the selected frame."
  (emacs-frame-make-frame-visible
   (emacs-frame-builtins--get-live-frame frame)))
(defun emacs-frame-builtins--raise-frame (&optional frame)
  "Raise live FRAME and return nil."
  (let ((frame (emacs-frame-builtins--get-live-frame frame)))
    (unless (emacs-frame-builtins--terminal-p frame)
      (emacs-frame-raise-frame frame))
    nil))
(defun emacs-frame-builtins--lower-frame (&optional frame)
  "Lower live FRAME and return nil."
  (let ((frame (emacs-frame-builtins--get-live-frame frame)))
    (unless (emacs-frame-builtins--terminal-p frame)
      (emacs-frame-lower-frame frame))
    nil))
(defun emacs-frame-builtins--select-frame (frame &optional norecord)
  "Select explicit live FRAME, returning FRAME."
  (emacs-frame-builtins--check-live-frame frame)
  (emacs-frame-select-frame frame norecord))
(defun emacs-frame-builtins--frame-focus (&optional frame)
  "Return FRAME's focus redirection, or nil when focus is not redirected."
  (emacs-frame-builtins--get-live-frame frame)
  ;; The shared backend currently has no focus-redirection operation.
  nil)
(defun emacs-frame-builtins--make-frame-invisible (&optional frame force)
  (let ((frame (emacs-frame--get frame)))
    (unless (emacs-frame-builtins--terminal-p frame)
      (emacs-frame-make-frame-invisible frame force))
    nil))
(defun emacs-frame-builtins--iconify-frame (&optional frame)
  (let ((frame (emacs-frame--get frame)))
    (unless (emacs-frame-builtins--terminal-p frame)
      (setf (emacs-frame-visible frame) 'iconified)
      (emacs-frame--call-backend :frame-visible frame 'iconified))
    nil))

(defun emacs-frame-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  (or emacs-frame-builtins--adopt-blank-frame-owner
      (get symbol 'emacs-stub-bulk)
      (not (boundp 'emacs-version))
      (not (stringp emacs-version))
      (not (fboundp symbol))))

;;;; --- constructors / predicates --------------------------------------

(when (emacs-frame-builtins--install-function-p 'make-frame)
  (defalias 'make-frame #'emacs-frame-make-frame))

(when (emacs-frame-builtins--install-function-p 'framep)
  (defalias 'framep #'emacs-frame-builtins--framep))

(when (emacs-frame-builtins--install-function-p 'frame-live-p)
  (defalias 'frame-live-p #'emacs-frame-builtins--frame-live-p))

(when (emacs-frame-builtins--install-function-p 'frame-list)
  (defalias 'frame-list #'emacs-frame-frame-list))

(when (emacs-frame-builtins--install-function-p 'selected-frame)
  (defalias 'selected-frame #'emacs-frame-selected-frame))

(when (emacs-frame-builtins--install-function-p 'window-frame)
  (defalias 'window-frame #'emacs-frame-builtins--window-frame))

;;;; --- lifecycle -------------------------------------------------------

(when (emacs-frame-builtins--install-function-p 'delete-frame)
  (defalias 'delete-frame #'emacs-frame-builtins--delete-frame))

(when (emacs-frame-builtins--install-function-p 'delete-other-frames)
  (defalias 'delete-other-frames #'emacs-frame-delete-other-frames))

;;;; --- size / position -------------------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-width)
  (defalias 'frame-width #'emacs-frame-frame-width))

(when (emacs-frame-builtins--install-function-p 'frame-height)
  (defalias 'frame-height #'emacs-frame-frame-height))

(when (emacs-frame-builtins--install-function-p 'frame-char-width)
  (defalias 'frame-char-width #'emacs-frame-builtins--frame-char-width))

(when (emacs-frame-builtins--install-function-p 'frame-char-height)
  (defalias 'frame-char-height #'emacs-frame-builtins--frame-char-height))

(when (emacs-frame-builtins--install-function-p 'frame-pixel-width)
  (defalias 'frame-pixel-width #'emacs-frame-builtins--frame-pixel-width))

(when (emacs-frame-builtins--install-function-p 'frame-pixel-height)
  (defalias 'frame-pixel-height #'emacs-frame-builtins--frame-pixel-height))

(when (emacs-frame-builtins--install-function-p 'set-frame-size)
  (defalias 'set-frame-size #'emacs-frame-builtins--set-frame-size))

(when (emacs-frame-builtins--install-function-p 'set-frame-position)
  (defalias 'set-frame-position #'emacs-frame-builtins--set-frame-position))

;;;; --- parameter access ------------------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-parameter)
  (defalias 'frame-parameter #'emacs-frame-builtins--frame-parameter))

(when (emacs-frame-builtins--install-function-p 'frame-parameters)
  (defalias 'frame-parameters #'emacs-frame-builtins--frame-parameters))

(when (emacs-frame-builtins--install-function-p 'set-frame-parameter)
  (defalias 'set-frame-parameter #'emacs-frame-set-frame-parameter))

(when (emacs-frame-builtins--install-function-p 'modify-frame-parameters)
  (defalias 'modify-frame-parameters #'emacs-frame-builtins--modify-frame-parameters))

;;;; --- visibility / z-order -------------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-visible-p)
  (defalias 'frame-visible-p #'emacs-frame-builtins--frame-visible-p))

(when (emacs-frame-builtins--install-function-p 'make-frame-visible)
  (defalias 'make-frame-visible #'emacs-frame-builtins--make-frame-visible))

(when (emacs-frame-builtins--install-function-p 'make-frame-invisible)
  (defalias 'make-frame-invisible #'emacs-frame-builtins--make-frame-invisible))

(when (emacs-frame-builtins--install-function-p 'iconify-frame)
  (defalias 'iconify-frame #'emacs-frame-builtins--iconify-frame))

(when (emacs-frame-builtins--install-function-p 'raise-frame)
  (defalias 'raise-frame #'emacs-frame-builtins--raise-frame))

(when (emacs-frame-builtins--install-function-p 'lower-frame)
  (defalias 'lower-frame #'emacs-frame-builtins--lower-frame))

;;;; --- selection / focus ----------------------------------------------

(when (emacs-frame-builtins--install-function-p 'select-frame)
  (defalias 'select-frame #'emacs-frame-builtins--select-frame))

(when (emacs-frame-builtins--install-function-p 'frame-focus)
  (defalias 'frame-focus #'emacs-frame-builtins--frame-focus))

;; T87: `with-selected-frame' (GNU `subr.el') was entirely absent from
;; `src/' -- `(fboundp 'with-selected-frame)' was nil, not merely a
;; nil no-op stub -- so `(evil-mode 1)' hit `(void-function
;; with-selected-frame)' via `evil-core.el's `evil-init-esc' /
;; `evil-deinit-esc' (called by `evil-esc-mode', which `evil-mode'
;; enables/disables unconditionally).  Ported verbatim from host
;; `subr.el' (confirmed against a clean `emacs -Q --batch', Emacs
;; 31.1): saves and restores the selected frame (`select-frame' /
;; `frame-live-p', both installed above) and the current buffer
;; (`current-buffer' / `buffer-live-p' / `set-buffer'), then
;; evaluates BODY with FRAME selected.  This runtime's single-frame
;; model (one implicit frame; see `emacs-frame--ensure-initial') does
;; not change the macro shape at all -- `select-frame' and
;; `frame-live-p' already behave correctly for that one frame, so the
;; macroexpansion matches host exactly (verified: identical to
;; `(macroexpand-1 '(with-selected-frame F BODY...))' on host Emacs).
(when (emacs-frame-builtins--install-function-p 'with-selected-frame)
  (defmacro with-selected-frame (frame &rest body)
    "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected frame, and changes the
order of neither the recently selected windows nor the buffers in
the buffer list."
    (declare (indent 1) (debug t))
    (let ((old-frame (make-symbol "old-frame"))
          (old-buffer (make-symbol "old-buffer")))
      `(let ((,old-frame (selected-frame))
             (,old-buffer (current-buffer)))
         (unwind-protect
             (progn (select-frame ,frame 'norecord)
                    ,@body)
           (when (frame-live-p ,old-frame)
             (select-frame ,old-frame 'norecord))
           (when (buffer-live-p ,old-buffer)
             (set-buffer ,old-buffer)))))))

;;;; --- frame->windows + display ---------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-windows)
  (defalias 'frame-windows #'emacs-frame-frame-windows))

(when (emacs-frame-builtins--install-function-p 'display-pixel-width)
  (defalias 'display-pixel-width #'emacs-frame-display-pixel-width))

(when (emacs-frame-builtins--install-function-p 'display-pixel-height)
  (defalias 'display-pixel-height #'emacs-frame-display-pixel-height))

(provide 'emacs-frame-builtins)

;;; emacs-frame-builtins.el ends here
