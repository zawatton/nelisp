;;; emacs-window-builtins.el --- Unprefixed window.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.C'' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* window builtins (=
;; `selected-window', `windowp', `window-list', `window-buffer',
;; `set-window-buffer') to the existing `emacs-window-*' prefixed
;; implementations in `emacs-window.el', mirroring the Phase 11.B'
;; `emacs-search-builtins.el' pattern.
;;
;; Why this exists: until Phase 11.C'' the unprefixed names lived as
;; nil-stubs inside `emacs-stub.el', so callers calling
;; `(selected-window)' got a `(cons 'window nil)' sentinel even though
;; `emacs-window.el' provides a real window-tree model rooted on a
;; `nelisp-emacs-compat' buffer.  Bridging unifies the two.
;;
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub.el' no-op shims.
;;
;; Bridgeable today (= covered by `emacs-window.el'):
;;
;;   - `selected-window' / `windowp'
;;   - `window-live-p' / `window-valid-p'
;;   - `frame-selected-window'
;;   - `window-list' / `window-list-1' / `next-window' / `previous-window'
;;   - `window-buffer' / `set-window-buffer'
;;   - `select-window'
;;   - `split-window' / `split-window-below' / `split-window-right'
;;     + legacy `split-window-vertically' / `split-window-horizontally'
;;   - `delete-window' / `delete-other-windows' / `delete-windows-on'
;;   - `one-window-p' / `balance-windows'
;;   - `get-buffer-window' / `get-buffer-window-list'
;;   - `other-window' (polyfilled — `emacs-window.el' has no direct equivalent)
;;   - `window-start' / `window-end' / `window-point' / `set-window-point'
;;     / `set-window-start' / `window-height' / `window-width'
;;     / `window-body-height' / `window-body-width'
;;     / `window-max-chars-per-line' (Doc 33 §4 item 9 — line-based, see below)
;;   - `recenter' / `scroll-up' / `scroll-down' / `scroll-up-command'
;;     / `scroll-down-command' / `pos-visible-in-window-p' (Doc 33 §4
;;     item 9 — real buffer-line-based semantics via
;;     `emacs-window-recenter' / `emacs-window-scroll-up' /
;;     `emacs-window-scroll-down' / `emacs-window-pos-visible-in-window-p',
;;     replacing the nil no-op stubs that `emacs-stub-bulk.el' would
;;     otherwise install for these names)
;;
;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- windows are the nemacs window model.
;;; Code:

(require 'emacs-window)

(defun emacs-window-builtins--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (condition-case nil
           (symbol-function symbol)
         (error nil))))

(defun emacs-window-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge.

`(not (boundp \\='emacs-version))' (with or without the `stringp'
refinement) is not a reliable standalone signal: the NeLisp reader
binds `emacs-version' too, to the real string \"30.1\", so both
disjuncts evaluate to nil there and this gate silently fell through to
`--function-cell-live-p' alone.  That check only asks whether the
*current* binding looks callable, and several names this bridge owns
(`selected-window', `windowp', `window-live-p', `window-list',
`frame-selected-window', `set-window-buffer', `window-buffer' via
`emacs-stub.el''s individually-defined, untagged window.c stubs; also
`next-window', `window-height', `window-width', `window-start',
`window-end', `window-point', `window-parameter', `set-window-point',
`set-window-start', `set-window-parameter',
`current-window-configuration', `set-window-configuration',
`select-window', `display-buffer', `recenter', `scroll-up',
`scroll-down', `scroll-up-command', `scroll-down-command' via
`emacs-stub-bulk.el''s bulk dolist -- tagged with `emacs-stub-bulk' but
this gate never checked the tag) already look \"live\" by the time this
file loads, so the bridge silently declined to override them.  Net
effect verified empirically: `(selected-window)' returned a fresh,
non-`eq'-stable stub object on every call, `(window-live-p ...)' and
`(window-list)' never delegated to the real window model, and
`save-selected-window''s restore half was a no-op.  Force install
unconditionally on standalone via a NeLisp-only primitive, matching
the standalone predicate in `emacs-char-table.el' and the same
fix already applied to `emacs-font-lock-builtins.el' /
`emacs-redisplay-builtins.el' for the identical defect class."
  (or (fboundp 'nl-write-file)
      (fboundp 'nelisp--write-stdout-bytes)
      (not (boundp 'emacs-version))
      (not (stringp emacs-version))
      (not (emacs-window-builtins--function-cell-live-p symbol))))

;;;; --- argument decoding and native buffer support --------------------

(defun emacs-window-builtins--window (window &optional any-window)
  "Decode WINDOW, requiring a live leaf unless ANY-WINDOW is non-nil."
  (let ((w (or window (selected-window))))
    (unless (if any-window (emacs-window-p w) (window-live-p w))
      (signal 'wrong-type-argument
              (list (if any-window 'windowp 'window-live-p) w)))
    w))

(defun emacs-window-builtins--frame (frame &optional allow-window)
  "Validate FRAME, optionally accepting a valid window."
  (unless (or (null frame)
              (and allow-window (window-valid-p frame))
              (frame-live-p frame))
    (signal 'wrong-type-argument (list 'frame-live-p frame)))
  (or frame (selected-frame)))

(defun emacs-window-builtins--position (position)
  "Decode an integer or marker POSITION."
  (unless (or (integerp position) (markerp position))
    (signal 'wrong-type-argument (list 'integer-or-marker-p position)))
  (if (markerp position) (or (marker-position position) 0) position))

(defun emacs-window-builtins--frame-windows (frame minibuf)
  "Return FRAME's leaves, including its minibuffer as MINIBUF requests."
  (let* ((root (and (emacs-frame-p frame) (emacs-frame-root-window frame)))
         (windows (if root (emacs-window--leaves-of root)
                    (and (eq frame (selected-frame))
                         (emacs-window--all-leaves))))
         (include-mini (or (eq minibuf t)
                           (and (null minibuf) (active-minibuffer-window))))
         (mini (minibuffer-window frame)))
    ;; Allocate only when requested, just as the cyclic window bridge does.
    (when (and include-mini (null mini) (eq frame (selected-frame)))
      (setq mini (emacs-window--make
                  :id (emacs-window--next-id)
                  :buffer (get-buffer-create " *Minibuf-0*")
                  :total-cols (emacs-window-total-cols (selected-window))
                  :total-lines 1
                  :top-line (+ (emacs-window-top-line emacs-window--root)
                               (emacs-window-total-lines emacs-window--root))
                  :parameters (list (cons 'minibuffer t))))
      (setq emacs-minibuffer--window mini))
    (setq windows (delq mini windows))
    (if (and include-mini (window-live-p mini))
        (append windows (list mini))
      windows)))

(defun emacs-window-builtins--rotate (windows window)
  "Rotate WINDOWS to begin with WINDOW, if WINDOW occurs in it."
  (let ((tail windows) prefix)
    (while (and tail (not (eq (car tail) window)))
      (setq prefix (cons (car tail) prefix)
            tail (cdr tail)))
    (if tail (append tail (nreverse prefix)) windows)))

(defun emacs-window-builtins--cycle (window minibuf backwards)
  "Find WINDOW's successor or predecessor in the implicit frame."
  (let* ((w (emacs-window-builtins--window window))
         (windows (emacs-window--all-leaves))
         (include-mini (or (eq minibuf t)
                           (and (null minibuf) (active-minibuffer-window))))
         (mini (minibuffer-window)))
    ;; The frame model allocates the minibuffer lazily.  Keep its dedicated
    ;; leaf outside the ordinary window tree, as GNU's cyclic ordering does.
    (when (and include-mini (null mini))
      (setq mini (emacs-window--make
                  :id (emacs-window--next-id)
                  :buffer (get-buffer-create " *Minibuf-0*")
                  :total-cols (emacs-window-total-cols w)
                  :total-lines 1
                  :top-line (+ (emacs-window-top-line emacs-window--root)
                               (emacs-window-total-lines emacs-window--root))
                  :parameters (list (cons 'minibuffer t))))
      (setq emacs-minibuffer--window mini))
    ;; Older minibuffer allocations can be ordinary tree leaves.  Include
    ;; that leaf exactly once, and only when MINIBUF permits it.
    (setq windows (delq mini windows))
    (when (and (window-live-p mini)
               include-mini)
      (setq windows (append windows (list mini))))
    (if backwards
        (let ((previous (car (last windows))) (tail windows))
          (while (and tail (not (eq (car tail) w)))
            (setq previous (car tail) tail (cdr tail)))
          previous)
      (or (cadr (memq w windows)) (car windows)))))

(defun emacs-window-builtins--snapshot (node parent)
  "Copy NODE for a configuration, retaining its original identity."
  (let ((copy (emacs-window--copy-shallow node)))
    (setf (emacs-window-parent copy) parent
          (emacs-window-parameters copy)
          (cons (cons 'emacs-window-builtins--original node)
                (copy-alist (emacs-window-parameters node))))
    (unless (emacs-window-leaf-p node)
      (setf (emacs-window-children copy)
            (mapcar (lambda (child)
                      (emacs-window-builtins--snapshot child copy))
                    (emacs-window-children node))))
    copy))

(defun emacs-window-builtins--restore (copy parent)
  "Restore COPY into its original window with PARENT."
  (let ((node (cdr (assq 'emacs-window-builtins--original
                         (emacs-window-parameters copy)))))
    (if (not node)
        (emacs-window--copy-tree copy parent)
      (setf (emacs-window-buffer node) (emacs-window-buffer copy)
            (emacs-window-point node) (emacs-window-point copy)
            (emacs-window-start node) (emacs-window-start copy)
            (emacs-window-total-cols node) (emacs-window-total-cols copy)
            (emacs-window-total-lines node) (emacs-window-total-lines copy)
            (emacs-window-top-line node) (emacs-window-top-line copy)
            (emacs-window-leaf-p node) (emacs-window-leaf-p copy)
            (emacs-window-direction node) (emacs-window-direction copy)
            (emacs-window-deleted-p node) nil
            (emacs-window-parent node) parent
            (emacs-window-parameters node)
            (copy-alist (cdr (emacs-window-parameters copy)))
            (emacs-window-children node)
            (mapcar (lambda (child)
                      (emacs-window-builtins--restore child node))
                    (emacs-window-children copy)))
      node)))

(defun emacs-window-builtins--scroll (arg direction)
  "Scroll the selected window by ARG lines in DIRECTION."
  (let* ((w (emacs-window-builtins--window nil))
         (buffer (window-buffer w)))
    (if (nelisp-ec-buffer-p buffer)
        (emacs-window--scroll w arg direction)
      (with-current-buffer buffer
        (let* ((height (window-body-height w))
               (page (max 1 (- height
                              (if (boundp 'next-screen-context-lines)
                                  next-screen-context-lines 2))))
               (amount (* direction
                          (cond ((null arg) page)
                                ((eq arg '-) (- page))
                                (t (prefix-numeric-value arg)))))
               (start (max (point-min) (min (point-max)
                                          (emacs-window-start w))))
               (new-start (save-excursion
                            (goto-char start)
                            (forward-line amount)
                            (point))))
          (when (and (/= amount 0) (= new-start start))
            (signal (if (> amount 0) 'end-of-buffer 'beginning-of-buffer) nil))
          (setf (emacs-window-start w) new-start)
          (when (< (point) new-start) (goto-char new-start))
          (setf (emacs-window-point w) (point))
          nil)))))

;;;; --- predicates ------------------------------------------------------

(when (emacs-window-builtins--install-function-p 'windowp)
  (defalias 'windowp #'emacs-window-windowp))

(when (emacs-window-builtins--install-function-p 'window-live-p)
  (defalias 'window-live-p #'emacs-window-window-live-p))

(when (emacs-window-builtins--install-function-p 'window-valid-p)
  (defun window-valid-p (window)
    "Return non-nil if WINDOW is a live or internal model window."
    (and (emacs-window-p window)
         (not (emacs-window-deleted-p window)))))

(when (emacs-window-builtins--install-function-p 'window-parent)
  (defun window-parent (&optional window)
    "Return WINDOW's parent window, or nil for a root window."
    (let ((w (or window (selected-window))))
      (unless (window-valid-p w)
        (signal 'wrong-type-argument (list 'window-valid-p w)))
      (emacs-window-parent w))))

;;;; --- accessors -------------------------------------------------------

(when (emacs-window-builtins--install-function-p 'selected-window)
  (defalias 'selected-window #'emacs-window-selected-window))

(when (emacs-window-builtins--install-function-p 'frame-selected-window)
  (defun frame-selected-window (&optional frame-or-window)
    "Return the selected window of FRAME-OR-WINDOW's frame."
    (let* ((target (or frame-or-window (selected-frame)))
           (frame (if (window-valid-p target)
                      (window-frame target)
                    (emacs-window-builtins--frame target))))
      (or (and (emacs-frame-p frame)
               (cdr (assq 'selected-window (emacs-frame-parameters frame))))
          (if (eq frame (selected-frame))
              (emacs-window-selected-window)
            (frame-first-window frame))))))

(when (emacs-window-builtins--install-function-p 'window-list)
  (defun window-list (&optional frame minibuf window)
    "Return FRAME's live windows in cyclic order starting with WINDOW."
    (let* ((frame (or frame (selected-frame)))
           (w (emacs-window-builtins--window
               (or window (and (frame-live-p frame)
                               (frame-selected-window frame))) t)))
      (unless (eq frame (window-frame w))
        (error "Window is on a different frame"))
      (emacs-window-builtins--window w)
      (emacs-window-builtins--rotate
       (emacs-window-builtins--frame-windows frame minibuf) w))))

(when (emacs-window-builtins--install-function-p 'window-list-1)
  (defun window-list-1 (&optional window minibuf all-frames)
    "Return live windows selected by ALL-FRAMES, starting with WINDOW."
    (let* ((w (emacs-window-builtins--window window))
           (frame (window-frame w))
           (frames (cond ((or (eq all-frames t)
                              (eq all-frames 'visible)
                              (eq all-frames 0)) (frame-list))
                         ((framep all-frames) (list all-frames))
                         (t (list frame))))
           windows minibuffers)
      (dolist (f frames)
        (when (and (frame-live-p f)
                   (or (not (memq all-frames '(visible 0)))
                       (if (eq all-frames 'visible)
                           (eq (frame-visible-p f) t)
                         (frame-visible-p f))))
          (let* ((leaves (emacs-window-builtins--frame-windows f minibuf))
                 (mini (minibuffer-window f)))
            (dolist (leaf leaves)
              (unless (and (eq leaf mini) (memq leaf minibuffers))
                (when (eq leaf mini)
                  (setq minibuffers (cons leaf minibuffers)))
                (setq windows (cons leaf windows)))))))
      (emacs-window-builtins--rotate (nreverse windows) w))))

(when (emacs-window-builtins--install-function-p 'next-window)
  (defun next-window (&optional window minibuf _all-frames)
    "Return the next live window in cyclic order."
    (emacs-window-builtins--cycle window minibuf nil)))

(when (emacs-window-builtins--install-function-p 'previous-window)
  (defun previous-window (&optional window minibuf _all-frames)
    "Return the previous live window in cyclic order."
    (emacs-window-builtins--cycle window minibuf t)))

(when (emacs-window-builtins--install-function-p 'window-buffer)
  (defun window-buffer (&optional window)
    "Return WINDOW's buffer, or nil for an internal or deleted window."
    (let ((w (emacs-window-builtins--window window t)))
      (when (window-live-p w)
        (emacs-window-window-buffer w)))))

(when (emacs-window-builtins--install-function-p 'one-window-p)
  (defalias 'one-window-p #'emacs-window-one-window-p))

(when (emacs-window-builtins--install-function-p 'get-buffer-window)
  (defun get-buffer-window (&optional buffer-or-name _all-frames)
    "Return a window displaying BUFFER-OR-NAME, or nil."
    (let ((buffer (cond
                   ((null buffer-or-name) (current-buffer))
                   ((nelisp-ec-buffer-p buffer-or-name) buffer-or-name)
                   (t (or (get-buffer buffer-or-name)
                          (and (stringp buffer-or-name)
                               (cdr (assoc buffer-or-name nelisp-ec--buffers)))))))
          (windows (emacs-window--all-leaves))
          found)
      (while (and buffer windows (not found))
        (when (eq buffer (emacs-window-buffer (car windows)))
          (setq found (car windows)))
        (setq windows (cdr windows)))
      found)))

(when (emacs-window-builtins--install-function-p 'get-buffer-window-list)
  (defalias 'get-buffer-window-list #'emacs-window-get-buffer-window-list))

(when (emacs-window-builtins--install-function-p 'window-height)
  (defalias 'window-height #'emacs-window-window-height))

(when (emacs-window-builtins--install-function-p 'window-width)
  (defalias 'window-width #'emacs-window-window-width))

(when (emacs-window-builtins--install-function-p 'window-body-height)
  (defun window-body-height (&optional window pixelwise)
    "Return live WINDOW's body height in lines or pixels."
    (let* ((w (emacs-window-builtins--window window))
           (height (max 1 (- (emacs-window-total-lines w)
                             (if (eq w (minibuffer-window)) 0 1)))))
      (if (and pixelwise (not (eq pixelwise 'remap)))
          (* height (frame-char-height (window-frame w)))
        height))))

(when (emacs-window-builtins--install-function-p 'window-body-width)
  (defun window-body-width (&optional window pixelwise)
    "Return live WINDOW's body width in columns or pixels."
    (let* ((w (emacs-window-builtins--window window))
           (cols (emacs-window-window-width w)))
      (if (and pixelwise (not (eq pixelwise 'remap)))
          (* cols (frame-char-width (window-frame w)))
        cols))))

(when (emacs-window-builtins--install-function-p 'window-max-chars-per-line)
  (defun window-max-chars-per-line (&optional window _face)
    "Phase 11 polyfill: maximum display columns for WINDOW."
    (max 1 (window-body-width window))))

(when (emacs-window-builtins--install-function-p 'window-start)
  (defun window-start (&optional window)
    "Return live WINDOW's cached first displayed buffer position."
    (emacs-window-start (emacs-window-builtins--window window))))

(when (emacs-window-builtins--install-function-p 'window-end)
  (defun window-end (&optional window update)
    "Return the position at which display ends in live WINDOW.
Batch windows have an initial end distance of zero from the buffer end."
    (let* ((w (emacs-window-builtins--window window))
           (buffer (window-buffer w)))
      (if (and noninteractive
               (null (emacs-window-window-parameter w 'emacs-redisplay-window-end)))
          (1+ (if (nelisp-ec-buffer-p buffer)
                  (nelisp-ec-buffer-size buffer)
                (buffer-size buffer)))
        (emacs-window-window-end w update)))))

(when (emacs-window-builtins--install-function-p 'window-point)
  (defun window-point (&optional window)
    "Return live WINDOW's point, using buffer point for the selected window."
    (let ((w (emacs-window-builtins--window window)))
      (if (and (eq w (selected-window))
               (eq (window-buffer w) (current-buffer)))
          (point)
        (emacs-window-point w)))))

(when (emacs-window-builtins--install-function-p 'window-parameter)
  (defun window-parameter (window parameter)
    "Return WINDOW's PARAMETER, including for internal or deleted windows."
    (cdr (assq parameter
               (emacs-window-parameters
                (emacs-window-builtins--window window t))))))

(when (emacs-window-builtins--install-function-p 'window-prev-buffers)
  (defun window-prev-buffers (&optional window)
    "Return live WINDOW's previous buffer history."
    (window-parameter (emacs-window-builtins--window window) 'prev-buffers)))

(when (emacs-window-builtins--install-function-p 'window-next-buffers)
  (defun window-next-buffers (&optional window)
    "Return live WINDOW's next buffer history."
    (window-parameter (emacs-window-builtins--window window) 'next-buffers)))

;;;; --- mutation --------------------------------------------------------

(when (emacs-window-builtins--install-function-p 'set-window-buffer)
  (defun set-window-buffer (window buffer-or-name &optional keep-margins)
    "Make live WINDOW display BUFFER-OR-NAME and return nil."
    (let* ((w (emacs-window-builtins--window window))
           (buffer (if (nelisp-ec-buffer-p buffer-or-name) buffer-or-name
                     (or (get-buffer buffer-or-name)
                         (and (stringp buffer-or-name)
                              (cdr (assoc buffer-or-name nelisp-ec--buffers)))))))
      (if (nelisp-ec-buffer-p buffer)
          (emacs-window-set-window-buffer w buffer keep-margins)
        (unless (buffer-live-p buffer)
          (signal 'wrong-type-argument (list 'bufferp buffer)))
        (let ((old (emacs-window-buffer w)))
          (when (and (eq (window-dedicated-p w) t)
                     (not (eq old buffer)))
            (error "Window is dedicated to %s"
                   (emacs-window--dedicated-buffer-quote
                    (buffer-name old))))
          (unless (eq old buffer)
            (when (window-dedicated-p w)
              (set-window-dedicated-p w nil))
            (when (buffer-live-p old)
              (set-window-prev-buffers
               w (cons (list old (emacs-window-start w) (emacs-window-point w))
                       (assq-delete-all old (window-prev-buffers w)))))
            (set-window-next-buffers w nil)
            (setf (emacs-window-buffer w) buffer
                  (emacs-window-point w) (with-current-buffer buffer (point))
                  (emacs-window-start w) (with-current-buffer buffer (point-min)))))
        nil))))

(when (emacs-window-builtins--install-function-p 'set-window-point)
  (defun set-window-point (window pos)
    "Set WINDOW's point to POS, returning POS before clamping."
    (let* ((w (emacs-window-builtins--window window))
           (position (emacs-window-builtins--position pos))
           (buffer (window-buffer w)))
      (if (nelisp-ec-buffer-p buffer)
          (emacs-window-set-window-point w position)
        (with-current-buffer buffer
          (let ((clamped (max (point-min) (min (point-max) position))))
            (setf (emacs-window-point w) clamped)
            (when (eq w (selected-window)) (goto-char clamped)))))
      pos)))

(when (emacs-window-builtins--install-function-p 'set-window-start)
  (defalias 'set-window-start #'emacs-window-set-window-start))

(when (emacs-window-builtins--install-function-p 'set-window-parameter)
  (defun set-window-parameter (window parameter value)
    "Set WINDOW's PARAMETER to VALUE and return VALUE."
    (let* ((w (emacs-window-builtins--window window t))
           (entry (assq parameter (emacs-window-parameters w))))
      (if entry (setcdr entry value)
        (setf (emacs-window-parameters w)
              (cons (cons parameter value) (emacs-window-parameters w))))
      value)))

(when (emacs-window-builtins--install-function-p 'window-configuration-p)
  (defalias 'window-configuration-p #'emacs-window-configuration-p))

(when (emacs-window-builtins--install-function-p 'current-window-configuration)
  (defun current-window-configuration (&optional frame)
    "Return a snapshot of FRAME's current window configuration."
    (emacs-window-builtins--frame frame)
    (emacs-window--ensure-root)
    (emacs-window-configuration--make
     :root (emacs-window-builtins--snapshot emacs-window--root nil)
     :selected (emacs-window-id emacs-window--selected))))

(when (emacs-window-builtins--install-function-p 'set-window-configuration)
  (defun set-window-configuration (configuration &optional _dont-set-frame
                                                   _dont-set-miniwindow)
    "Restore windows from CONFIGURATION and return t for a live frame."
    (unless (window-configuration-p configuration)
      (signal 'wrong-type-argument (list 'window-configuration-p configuration)))
    (setq emacs-window--root
          (emacs-window-builtins--restore
           (emacs-window-configuration-root configuration) nil))
    (setq emacs-window--selected
          (or (emacs-window--find-by-id
               emacs-window--root
               (emacs-window-configuration-selected configuration))
              (car (emacs-window--all-leaves))))
    ;; Restoring a configuration realizes terminal decorations even when
    ;; the saved tree is unchanged.  Its text height excludes the menu bar.
    (let ((frame (selected-frame)))
      (when (and noninteractive (emacs-frame-p frame)
                 (emacs-frame-builtins--terminal-p frame))
        (emacs-frame-builtins-layout-terminal
         frame (emacs-frame-builtins--frame-pixel-width frame)
         (- (emacs-frame-builtins--frame-pixel-height frame)
            (max 0 (or (frame-parameter frame 'menu-bar-lines) 1))))))
    (emacs-window-select-window emacs-window--selected)
    t))

(when (emacs-window-builtins--install-function-p 'set-window-prev-buffers)
  (defun set-window-prev-buffers (window prev-buffers)
    "Set live WINDOW's previous buffer history to PREV-BUFFERS."
    (set-window-parameter (emacs-window-builtins--window window)
                          'prev-buffers prev-buffers)))

(when (emacs-window-builtins--install-function-p 'set-window-next-buffers)
  (defun set-window-next-buffers (window next-buffers)
    "Set live WINDOW's next buffer history to NEXT-BUFFERS."
    (set-window-parameter (emacs-window-builtins--window window)
                          'next-buffers next-buffers)))

(when (emacs-window-builtins--install-function-p 'select-window)
  (defun select-window (window &optional norecord)
    "Select live WINDOW and make its buffer current, returning WINDOW."
    (let* ((w (emacs-window-builtins--window window))
           (old (selected-window))
           (buffer (window-buffer w)))
      (when (and (bufferp (emacs-window-buffer old))
                 (eq (current-buffer) (emacs-window-buffer old)))
        (setf (emacs-window-point old) (point)))
      (emacs-window-select-window w norecord)
      (when (buffer-live-p buffer)
        (set-buffer buffer)
        (goto-char (emacs-window-point w)))
      w)))

;;;; --- split / delete (Track V, 2026-05-04) ----------------------------

(when (emacs-window-builtins--install-function-p 'split-window)
  (defun split-window (&optional window size side)
    "Split live WINDOW, respecting the Emacs minimum window dimensions."
    (let* ((w (emacs-window-builtins--window window))
           (horizontal (memq side '(left right)))
           (total (if horizontal (window-width w) (window-height w)))
           (minimum (if horizontal
                        (if (boundp 'window-min-width) window-min-width 10)
                      (if (boundp 'window-min-height) window-min-height 4))))
      (when (< total (* 2 minimum))
        (error "Window #<window %d on %s> too small for splitting"
               (emacs-window-id w) (buffer-name (window-buffer w))))
      (emacs-window-split-window w size side))))

(when (emacs-window-builtins--install-function-p 'split-window-below)
  (defun split-window-below (&optional size)
    "Phase 11 polyfill: split selected window into two stacked windows.
Bound to C-x 2 in `nemacs-main-keymap'."
    (interactive "P")
    (split-window nil size 'below)))

(when (emacs-window-builtins--install-function-p 'split-window-right)
  (defun split-window-right (&optional size)
    "Phase 11 polyfill: split selected window into two side-by-side windows.
Bound to C-x 3 in `nemacs-main-keymap'."
    (interactive "P")
    (split-window nil size 'right)))

(when (emacs-window-builtins--install-function-p 'split-window-vertically)
  (defalias 'split-window-vertically #'emacs-window-split-window-vertically))

(when (emacs-window-builtins--install-function-p 'split-window-horizontally)
  (defalias 'split-window-horizontally #'emacs-window-split-window-horizontally))

(when (emacs-window-builtins--install-function-p 'delete-window)
  (defun delete-window (&optional window)
    "Phase 11 polyfill: delete WINDOW (default = selected).
Bound to C-x 0 in `nemacs-main-keymap'."
    (interactive)
    (emacs-window-delete-window window)))

(when (emacs-window-builtins--install-function-p 'delete-other-windows)
  (defun delete-other-windows (&optional window)
    "Phase 11 polyfill: delete every window except WINDOW (default = selected).
Bound to C-x 1 in `nemacs-main-keymap'."
    (interactive)
    (emacs-window-delete-other-windows window)))

(when (emacs-window-builtins--install-function-p 'delete-windows-on)
  (defalias 'delete-windows-on #'emacs-window-delete-windows-on))

(when (emacs-window-builtins--install-function-p 'balance-windows)
  (defalias 'balance-windows #'emacs-window-balance-windows))

;;;; --- other-window (Track V) -----------------------------------------
;;
;; `emacs-window.el' has no direct `emacs-window-other-window'; we
;; build it from `next-window' + `select-window'.  COUNT is the number
;; of windows to skip (default 1, can be negative for backwards).
;; Wraps around at the ends.  ALL-FRAMES is accepted for API parity.

(defun emacs-window-other-window-impl (&optional count all-frames)
  "Bridge implementation of `other-window'.
COUNT defaults to 1; negative values walk backwards.  ALL-FRAMES is
accepted for API parity and ignored (= single-frame Phase 1)."
  (interactive "p")
  (let* ((n   (or count 1))
         (cur (emacs-window-selected-window))
         (forward-fn (lambda (w) (emacs-window-next-window w nil all-frames)))
         (back-fn    (lambda (w) (emacs-window-previous-window w nil all-frames)))
         (step (if (>= n 0) forward-fn back-fn))
         (steps (abs n))
         (target cur))
    (dotimes (_ steps)
      (setq target (funcall step target)))
    (when target
      (emacs-window-select-window target))
    target))

(when (emacs-window-builtins--install-function-p 'other-window)
  (defalias 'other-window #'emacs-window-other-window-impl))

;;;; --- display-buffer / pop-to-buffer (M3 display policy) --------------

(when (emacs-window-builtins--install-function-p 'display-buffer)
  (defalias 'display-buffer #'emacs-window-display-buffer))

(when (emacs-window-builtins--install-function-p 'pop-to-buffer)
  (defalias 'pop-to-buffer #'emacs-window-pop-to-buffer))

(when (emacs-window-builtins--install-function-p 'pop-to-buffer-same-window)
  (defalias 'pop-to-buffer-same-window #'emacs-window-pop-to-buffer))

(when (emacs-window-builtins--install-function-p 'switch-to-buffer-other-window)
  (defalias 'switch-to-buffer-other-window #'emacs-window-pop-to-buffer))

(when (emacs-window-builtins--install-function-p 'quit-window)
  (defun quit-window (&optional kill window)
    "Phase 11 polyfill: quit WINDOW, closing a popup or burying its buffer.
Bound to `q' in help/special-buffer keymaps."
    (interactive "P")
    (emacs-window-quit-window kill window)))

;;;; --- temp-buffer-window setup/show (T87) ------------------------------
;;
;; Supporting functions for `with-current-buffer-window' /
;; `with-temp-buffer-window' (macro bodies live in
;; `emacs-parity-shims.el', ported verbatim from host `window.el').
;; Ported from host `window.el' (Emacs 31.1) with two documented
;; single-frame-model simplifications:
;;
;;   1. `temp-buffer-window-setup' skips `(delete-all-overlays)' --
;;      that primitive does not exist anywhere on this runtime yet
;;      (neither `fboundp' nor `boundp'), and overlay lifecycle is
;;      outside this file's ownership; buffers this helper targets are
;;      freshly `get-buffer-create'd or reused temp buffers, so a
;;      leftover overlay is a cosmetic edge case, not a correctness
;;      blocker for the callers this closes the gap for.
;;   2. `temp-buffer-window-show' skips the `window-combination-limit'
;;      let-binding trick and the `temp-buffer-resize-mode' resize
;;      step -- both read variables that `emacs-stub-bulk.el' installs
;;      as nil-returning *functions*, not special variables (so
;;      referencing them as variables would signal `void-variable');
;;      the trick and the resize step are opt-in discretionary
;;      behavior that defaults off in real Emacs too, so omitting them
;;      changes nothing for the default configuration this runtime
;;      targets.
;;
;; Both hooks below are real (`run-hooks' is called with them), just
;; empty by default like host Emacs.

(unless (boundp 'temp-buffer-window-setup-hook)
  (defvar temp-buffer-window-setup-hook nil
    "Normal hook run by `with-temp-buffer-window' before buffer display.
This hook is run by `with-temp-buffer-window' with the buffer to be
displayed current."))

(unless (boundp 'temp-buffer-window-show-hook)
  (defvar temp-buffer-window-show-hook nil
    "Normal hook run by `with-temp-buffer-window' after buffer display.
This hook is run by `with-temp-buffer-window' with the buffer
displayed and current and its window selected."))

(when (emacs-window-builtins--install-function-p 'temp-buffer-window-setup)
  (defun temp-buffer-window-setup (buffer-or-name)
    "Set up temporary buffer specified by BUFFER-OR-NAME.
Return the buffer.  (Single-frame-model port -- see file commentary
for the `delete-all-overlays' simplification.)"
    (let ((old-dir default-directory)
          (buffer (get-buffer-create buffer-or-name)))
      (with-current-buffer buffer
        (kill-all-local-variables)
        (setq default-directory old-dir)
        (setq buffer-read-only nil)
        (setq buffer-file-name nil)
        (setq buffer-undo-list t)
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (erase-buffer)
          (run-hooks 'temp-buffer-window-setup-hook))
        buffer))))

(when (emacs-window-builtins--install-function-p 'temp-buffer-window-show)
  (defun temp-buffer-window-show (buffer &optional action)
    "Show temporary buffer BUFFER in a window.
Return the window showing BUFFER.  Pass ACTION as action argument to
`display-buffer'.  (Single-frame-model port -- see file commentary
for the `window-combination-limit' / `temp-buffer-resize-mode'
simplification.)"
    (let (window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (setq window (display-buffer buffer action))
        (when window
          (setq minibuffer-scroll-window window)
          (set-window-hscroll window 0)
          (with-selected-window window
            (run-hooks 'temp-buffer-window-show-hook))))
      window)))

;;;; --- scroll / recenter / visibility (Doc 33 §4 item 9) ----------------
;;
;; Real buffer-line-based implementations (see `emacs-window.el').
;; These names are in `emacs-stub-bulk.el's nil-no-op list (or, for
;; `pos-visible-in-window-p', void entirely); this file loads first in
;; the standalone bootstrap, so the `(unless (fboundp ...))' guards
;; there defer to the real definitions installed here.

(when (emacs-window-builtins--install-function-p 'recenter)
  (defun recenter (&optional arg _redisplay)
    "Put point on screen line ARG of the selected window."
    (interactive "P")
    (let* ((w (emacs-window-builtins--window nil))
           (buffer (window-buffer w)))
      (if (nelisp-ec-buffer-p buffer)
          (emacs-window-recenter w arg)
        (with-current-buffer buffer
          (let* ((height (window-body-height w))
                 (line (if (integerp arg) arg (/ height 2)))
                 (row (max 0 (min (1- height)
                                  (if (< line 0) (+ height line) line)))))
            (setf (emacs-window-point w) (point)
                  (emacs-window-start w)
                  (save-excursion (forward-line (- row)) (point)))))))
    nil))

(when (emacs-window-builtins--install-function-p 'scroll-up)
  (defun scroll-up (&optional arg)
    "Scroll the selected window upward ARG lines."
    (interactive "P")
    (emacs-window-builtins--scroll arg 1)))

(when (emacs-window-builtins--install-function-p 'scroll-down)
  (defun scroll-down (&optional arg)
    "Scroll the selected window downward ARG lines."
    (interactive "P")
    (emacs-window-builtins--scroll arg -1)))

(when (emacs-window-builtins--install-function-p 'scroll-up-command)
  (defalias 'scroll-up-command #'scroll-up))

(when (emacs-window-builtins--install-function-p 'scroll-down-command)
  (defalias 'scroll-down-command #'scroll-down))

(when (emacs-window-builtins--install-function-p 'pos-visible-in-window-p)
  (defun pos-visible-in-window-p (&optional pos window partially)
    "Return whether POS is displayed in live WINDOW."
    (let ((w (emacs-window-builtins--window window)))
      (unless (or (null pos) (eq pos t))
        (setq pos (emacs-window-builtins--position pos)))
      ;; A batch frame has no displayed glyph rows.
      (unless noninteractive
        (emacs-window-pos-visible-in-window-p pos w partially)))))

(when (emacs-window-builtins--install-function-p 'frame-root-window)
  (defun frame-root-window (&optional frame-or-window)
    "Return the internal root of FRAME-OR-WINDOW's live window tree."
    (let ((root (if (window-valid-p frame-or-window) frame-or-window
                  (let ((frame (emacs-window-builtins--frame frame-or-window)))
                    (or (emacs-frame-root-window frame)
                        (and (eq frame (selected-frame)) (emacs-window-selected-window)))))))
      (while (emacs-window-parent root) (setq root (emacs-window-parent root)))
      root)))

(when (emacs-window-builtins--install-function-p 'fit-window-to-buffer)
  (defun fit-window-to-buffer (&optional window max-height min-height max-width min-width preserve-size)
    (interactive)
    (emacs-window-fit-window-to-buffer (emacs-window-builtins--window window)
                                      max-height min-height max-width min-width preserve-size)))
(when (emacs-window-builtins--install-function-p 'window-resize)
  (defun window-resize (window delta &optional horizontal ignore pixelwise)
    (setq window (or window (selected-window)))
    (unless (window-valid-p window)
      (signal 'wrong-type-argument (list 'window-valid-p window)))
    (emacs-window-window-resize window delta horizontal ignore pixelwise)))
(when (emacs-window-builtins--install-function-p 'shrink-window-if-larger-than-buffer)
  (defun shrink-window-if-larger-than-buffer (&optional window)
    (interactive)
    (emacs-window-shrink-window-if-larger-than-buffer (emacs-window-builtins--window window))))
(when (emacs-window-builtins--install-function-p 'window-text-pixel-size)
  (defun window-text-pixel-size (&optional window from to x-limit y-limit mode-lines ignore-line-at-end)
    (emacs-window-text-pixel-size (emacs-window-builtins--window window)
                                 from to x-limit y-limit mode-lines ignore-line-at-end)))

(defvar emacs-window-builtins--legacy-pixel-measurement nil)

(defun emacs-window-builtins--pixel-measurement
    (&optional window from to x-limit y-limit mode-lines ignore-line-at-end)
  "Measure native buffers with shared window semantics.
The transitional ec-buffer renderer includes its final empty glyph row;
retain its existing shaping provider for that buffer family."
  (let ((window (emacs-window-builtins--window window)))
    (if (and emacs-window-builtins--legacy-pixel-measurement
             (nelisp-ec-buffer-p (emacs-window-window-buffer window)))
        (funcall emacs-window-builtins--legacy-pixel-measurement
                 window from to x-limit y-limit mode-lines ignore-line-at-end)
      (emacs-window-text-pixel-size
       window from to x-limit y-limit mode-lines ignore-line-at-end))))

;; The pixel adapter retains its installation entry point. Preserve host
;; Emacs primitives and the existing legacy renderer's measurement contract.
(defun emacs-window-builtins--install-pixel-measurement ()
  "Route the realized pixel adapter through the owning buffer family."
  (when (fboundp 'nelisp--write-stdout-bytes)
    (unless (eq (symbol-function 'emacs-frame-pixels-window-text-size)
                (symbol-function 'emacs-window-builtins--pixel-measurement))
      (setq emacs-window-builtins--legacy-pixel-measurement
            (symbol-function 'emacs-frame-pixels-window-text-size)))
    (fset 'emacs-frame-pixels-window-text-size
          (symbol-function 'emacs-window-builtins--pixel-measurement))))
(if (featurep 'emacs-frame-pixels)
    (emacs-window-builtins--install-pixel-measurement)
  (eval-after-load 'emacs-frame-pixels #'emacs-window-builtins--install-pixel-measurement))

(defun emacs-window-builtins--pos-property-window
    (original position property &optional object)
  "Decode a shared window OBJECT at the position-property boundary.
The buffer property owner handles stickiness; the window shim supplies the
displayed buffer. GNU position properties use buffer-wide overlays even
when OBJECT is a window. Host Emacs already implements this variant."
  (if (emacs-window-p object)
      (let* ((window (progn (emacs-window--check-leaf object) object))
             (position (emacs-window-builtins--position position))
             (buffer (emacs-window-window-buffer window))
             (overlay (and (fboundp 'nelisp--write-stdout-bytes)
                           (get-char-property-and-overlay position property buffer))))
        (if (cdr overlay) (car overlay)
          (funcall original position property buffer)))
    (funcall original position property object)))

(defun emacs-window-builtins--install-pos-property-window ()
  "Install the native window-object bridge after its property provider."
  (when (and (fboundp 'nelisp--write-stdout-bytes) (fboundp 'get-pos-property))
    (advice-remove 'get-pos-property #'emacs-window-builtins--pos-property-window)
    (advice-add 'get-pos-property :around #'emacs-window-builtins--pos-property-window)))
(if (featurep 'emacs-cc-editfns-1)
    (emacs-window-builtins--install-pos-property-window)
  (eval-after-load 'emacs-cc-editfns-1 #'emacs-window-builtins--install-pos-property-window))

(provide 'emacs-window-builtins)

;;; emacs-window-builtins.el ends here
