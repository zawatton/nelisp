;;; emacs-frame.el --- Emacs C frame.c port (stub-mode invariant)  -*- lexical-binding: t; -*-

;; Phase 1 module 3/6 per nelisp-emacs Doc 01 (LOCKED v2).
;; Layer: nelisp-emacs (Layer 2 extension on top of NeLisp).
;; Namespace: `emacs-frame-' so loading inside a host Emacs does NOT
;; shadow `make-frame', `selected-frame', `frame-list', etc.
;;
;; Foundation contracts:
;;   - NeLisp Doc 34 v2 §2.11 LOCKED `frame stub mode invariant':
;;       * `make-frame' returns a unique frame-id (a `cl-defstruct'
;;         object whose `emacs-frame-id' slot is monotonically
;;         allocated, so identity is opaque + unique).
;;       * `frame-width' = 80 / `frame-height' = 24 (stub default).
;;       * `delete-frame' updates the registry only (= no backend
;;         destroy call in stub mode).
;;       * placeholder backend object shape (`emacs-frame--backend'
;;         slot) is compatible with Doc 33 §6.1 / Doc 43 §2.1
;;         `display-spec-frame-handle' substitution.
;;
;;   - NeLisp Doc 43 v2 §2.1 LOCKED `frame swap-in protocol' (Phase
;;     11.A consumer):
;;       * Module exposes `emacs-frame-set-backend-dispatch' so a
;;         later TUI / GUI backend can register a dispatch table
;;         (= `:frame-create' / `:frame-destroy' / `:frame-resize'
;;         / `:frame-visible' / `:capability-query' callbacks).
;;       * Stub mode = built-in dispatch table that records state
;;         in the frame struct only (no I/O).
;;       * `emacs-frame-current-backend' / `emacs-frame-capability-p'
;;         provide capability flag queries to consumers (e.g.
;;         `emacs-window' redisplay later).
;;
;;   - NOT a producer of `display-spec' itself; consumes the
;;     `:capability-query' hook to expose backend capabilities.
;;
;; API surface (~25 public APIs across 8 categories):
;;
;;   A. frame query              (5 APIs)
;;      framep / frame-live-p
;;      selected-frame / frame-list
;;      window-frame
;;
;;   B. frame creation/deletion  (3 APIs)
;;      make-frame &optional PARAMS / delete-frame &optional FRAME
;;      delete-other-frames &optional FRAME
;;
;;   C. frame size               (6 APIs)
;;      frame-width / frame-height
;;      frame-char-width / frame-char-height
;;      frame-pixel-width / frame-pixel-height
;;      set-frame-size FRAME COLS LINES
;;
;;   D. frame position           (1 API)
;;      set-frame-position FRAME X Y
;;
;;   E. frame parameters         (4 APIs)
;;      frame-parameters &optional FRAME
;;      frame-parameter FRAME PARAMETER
;;      set-frame-parameter FRAME PARAMETER VALUE
;;      modify-frame-parameters FRAME ALIST
;;
;;   F. frame visibility / Z     (4 APIs)
;;      frame-visible-p / make-frame-visible / make-frame-invisible
;;      raise-frame / lower-frame
;;
;;   G. frame selection / focus  (2 APIs)
;;      select-frame &optional NORECORD
;;      frame-focus
;;
;;   H. frame->windows + display (3 APIs)
;;      frame-windows &optional FRAME
;;      display-pixel-width / display-pixel-height
;;
;; Non-goals (deferred per task spec):
;;   - real display backend rendering            (= Phase 11)
;;   - font / glyph metrics                       (= Phase 11)
;;   - IME / input methods                        (= Phase 11)
;;   - frame configurations save/restore          (Phase 1.5+)
;;   - tab-bar / tool-bar / scroll-bar / fringe   (Phase 11.B)

;;; Code:

(require 'cl-lib)

;;; Errors

(define-error 'emacs-frame-error    "emacs-frame error")
(define-error 'emacs-frame-dead     "Frame is dead" 'emacs-frame-error)
(define-error 'emacs-frame-only     "Cannot delete sole frame"
  'emacs-frame-error)
(define-error 'emacs-frame-bad-size "Bad frame size" 'emacs-frame-error)

;;; Tunables (Doc 34 §2.11 stub invariant constants)

(defconst emacs-frame--default-cols  80
  "Doc 34 §2.11 LOCKED stub `frame-width'.")
(defconst emacs-frame--default-lines 24
  "Doc 34 §2.11 LOCKED stub `frame-height'.")
(defconst emacs-frame--char-width    8
  "Pseudo `frame-char-width' (px) used to derive `frame-pixel-width'.")
(defconst emacs-frame--char-height   16
  "Pseudo `frame-char-height' (px) used to derive `frame-pixel-height'.")
(defconst emacs-frame--display-cols  1024
  "Pseudo `display-pixel-width' for the implicit single display.")
(defconst emacs-frame--display-lines 768
  "Pseudo `display-pixel-height' for the implicit single display.")
(defconst emacs-frame--min-cols      2
  "Minimum allowed `frame-width' (= sane lower bound for split).")
(defconst emacs-frame--min-lines     1
  "Minimum allowed `frame-height'.")

;;; Frame struct

(cl-defstruct (emacs-frame
               (:constructor emacs-frame--make)
               (:copier      emacs-frame--copy-shallow)
               (:predicate   emacs-frame-p))
  "An Emacs frame, abstract handle (Doc 34 §2.2 LOCKED A).

Slots:
- ID            : monotonically increasing integer; identity is
                  opaque (Doc 34 §2.11 invariant: unique frame-id).
- NAME          : human-readable name (default \"F<id>\").
- WIDTH         : total width in columns.
- HEIGHT        : total height in lines.
- PIXEL-WIDTH   : derived width in pseudo pixels (= WIDTH * char-w).
- PIXEL-HEIGHT  : derived height in pseudo pixels.
- LEFT / TOP    : on-display position in pseudo pixels.
- VISIBLE       : nil / t / `iconified'.
- PARAMETERS    : alist of (KEY . VALUE) for `frame-parameter'.
- ROOT-WINDOW   : opaque window object owned by `emacs-window' (or
                  nil if window module is not loaded).
- BACKEND       : symbol identifying the realising backend, or
                  the constant `stub' (Doc 43 §2.1 invariant: stub
                  vs real backend).  Phase 11.A swap mutates this
                  via `emacs-frame-set-backend-dispatch'.
- BACKEND-OBJ   : Phase 11+ `display-spec-frame-handle' object, or
                  nil in stub mode.  Reserved slot so the Phase 11
                  swap-in does not need to re-shape this struct.
- DEAD-P        : t once the frame has been deleted from the
                  registry."
  (id           0)
  (name         nil)
  (width        emacs-frame--default-cols)
  (height       emacs-frame--default-lines)
  (pixel-width  (* emacs-frame--default-cols  emacs-frame--char-width))
  (pixel-height (* emacs-frame--default-lines emacs-frame--char-height))
  (left         0)
  (top          0)
  (visible      t)
  (parameters   nil)
  (root-window  nil)
  (backend      'stub)
  (backend-obj  nil)
  (dead-p       nil))

;;; Module state

(defvar emacs-frame--id-counter 0
  "Monotonically increasing frame-id counter (Doc 34 §2.11).")

(defvar emacs-frame--registry nil
  "List of all live frames in creation order (most recent last).")

(defvar emacs-frame--selected nil
  "Currently selected frame.")

(defvar emacs-frame--focus nil
  "Frame currently holding input focus (= or `emacs-frame--selected').")

(defvar emacs-frame--backend-dispatch nil
  "Current backend dispatch plist or nil.

Phase 11.A swap-in (Doc 43 §2.1 protocol step 2) registers a plist
with the following keys:

  :name              symbol identifying the backend (= `tui', `gtk', ...).
  :frame-create      (FRAME PARAMS) -> backend-obj.
  :frame-destroy     (FRAME) -> any.
  :frame-resize      (FRAME COLS LINES) -> any.
  :frame-position    (FRAME X Y) -> any.
  :frame-visible     (FRAME VISIBLE-P) -> any.
  :frame-raise       (FRAME) -> any.
  :frame-lower       (FRAME) -> any.
  :capability-query  (CAPABILITY) -> non-nil if supported.

In stub mode this var is nil and all hooks no-op.")

;;; Init / fresh-world helpers

(defun emacs-frame--next-id ()
  (cl-incf emacs-frame--id-counter))

(defun emacs-frame--call-backend (key &rest args)
  "Call backend dispatch KEY with ARGS, no-op if backend not installed."
  (let ((fn (and emacs-frame--backend-dispatch
                 (plist-get emacs-frame--backend-dispatch key))))
    (when (functionp fn)
      (apply fn args))))

(defun emacs-frame--ensure-initial ()
  "Ensure at least one live frame exists and is selected.

Returns the (single) frame of a freshly created registry, or the
existing selected frame if one is already in place."
  (unless (and emacs-frame--selected
               (emacs-frame-p emacs-frame--selected)
               (not (emacs-frame-dead-p emacs-frame--selected)))
    (let* ((id (emacs-frame--next-id))
           (f  (emacs-frame--make
                :id   id
                :name (format "F%d" id))))
      (push f emacs-frame--registry)
      (setq emacs-frame--registry (nreverse emacs-frame--registry))
      (setq emacs-frame--selected f
            emacs-frame--focus    f)
      ;; Inform backend (no-op in stub mode).
      (let ((obj (emacs-frame--call-backend :frame-create f nil)))
        (when obj
          (setf (emacs-frame-backend-obj f) obj)))))
  emacs-frame--selected)

(defun emacs-frame-reset ()
  "Tear down the registry so the next API call starts fresh.

Test-only convenience; not part of the public Emacs API surface."
  (setq emacs-frame--id-counter 0
        emacs-frame--registry   nil
        emacs-frame--selected   nil
        emacs-frame--focus      nil)
  nil)

;;; Internal sanity helpers

(defsubst emacs-frame--check-live (frame)
  (unless (emacs-frame-p frame)
    (signal 'wrong-type-argument (list 'emacs-frame-p frame)))
  (when (emacs-frame-dead-p frame)
    (signal 'emacs-frame-dead (list frame)))
  frame)

(defun emacs-frame--get (frame)
  "Return FRAME or the selected frame if nil; signal if non-frame."
  (let ((f (or frame (emacs-frame-selected-frame))))
    (emacs-frame--check-live f)
    f))

;;; Backend swap-in (Doc 43 §2.1 protocol)

(defun emacs-frame-set-backend-dispatch (dispatch)
  "Install DISPATCH as the active frame backend (Doc 43 §2.1).

DISPATCH is a plist (see `emacs-frame--backend-dispatch' for keys)
or nil to revert to stub mode.

After install the `:name' field (or `stub' if nil) is recorded on
every existing live frame's BACKEND slot, but no immediate
`:frame-create' is called for already-live stub frames (= Phase
11.A re-creation policy is `swap restart recommended' per Doc 43
§2.1 step 3)."
  (setq emacs-frame--backend-dispatch dispatch)
  (let ((name (or (plist-get dispatch :name) 'stub)))
    (dolist (f emacs-frame--registry)
      (unless (emacs-frame-dead-p f)
        (setf (emacs-frame-backend f) name)))
    name))

(defun emacs-frame-current-backend ()
  "Return the symbol identifying the active backend (`stub' if none)."
  (or (and emacs-frame--backend-dispatch
           (plist-get emacs-frame--backend-dispatch :name))
      'stub))

(defun emacs-frame-capability-p (capability)
  "Return non-nil if the active backend supports CAPABILITY.

In stub mode every capability returns nil except the stub-only
core ones: `frame-create', `frame-destroy', `frame-resize'."
  (let ((fn (and emacs-frame--backend-dispatch
                 (plist-get emacs-frame--backend-dispatch
                            :capability-query))))
    (cond
     ((functionp fn) (funcall fn capability))
     (t (memq capability '(frame-create frame-destroy frame-resize))))))

;;; A. frame query

(defun emacs-frame-framep (object)
  "Return non-nil if OBJECT is a (live or dead) frame.
Mirrors Emacs `framep' return-value semantics: returns the symbol
identifying the backend (`stub', `tui', ...) for a frame, nil for
a non-frame, just like Emacs returns `t', `x', `w32', ..."
  (and (emacs-frame-p object)
       (emacs-frame-backend object)))

(defun emacs-frame-frame-live-p (object)
  "Return non-nil if OBJECT is a live (= not deleted) frame.
The non-nil return is the backend symbol, matching Emacs."
  (and (emacs-frame-p object)
       (not (emacs-frame-dead-p object))
       (emacs-frame-backend object)))

(defun emacs-frame-selected-frame ()
  "Return the currently selected frame.
Auto-creates the implicit first frame if none exist yet."
  (emacs-frame--ensure-initial)
  emacs-frame--selected)

(defun emacs-frame-frame-list ()
  "Return a list of all live frames in creation order."
  (emacs-frame--ensure-initial)
  (cl-remove-if #'emacs-frame-dead-p emacs-frame--registry))

(defun emacs-frame-window-frame (&optional window)
  "Return the frame containing WINDOW.

WINDOW is treated opaquely: callers pass an `emacs-window' object
or any object that has been recorded as a frame's `root-window'.
If WINDOW is nil, the selected frame is returned (matches the
common case `(window-frame)').

Resolution order:
1. If WINDOW is nil       -> selected frame.
2. If WINDOW = some FRAME -> FRAME (lets callers pass a frame).
3. If WINDOW is the symbol `emacs-window--default-frame', the
   stub sentinel emitted by `emacs-window-window-frame' before
   `emacs-frame' was loaded -> selected frame.
4. Otherwise scan the registry for a frame whose `root-window' is
   `eq' to WINDOW; return it or nil."
  (cond
   ((null window)            (emacs-frame-selected-frame))
   ((emacs-frame-p window)   (emacs-frame--check-live window))
   ((eq window 'emacs-window--default-frame)
    (emacs-frame-selected-frame))
   (t
    (cl-some (lambda (f)
               (and (not (emacs-frame-dead-p f))
                    (eq (emacs-frame-root-window f) window)
                    f))
             emacs-frame--registry))))

;;; B. frame creation / deletion

(defun emacs-frame--apply-params (frame params)
  "Apply PARAMS alist to FRAME, mutating struct slots when possible."
  (dolist (kv params)
    (let ((k (car kv)) (v (cdr kv)))
      (pcase k
        ('width        (when (integerp v) (setf (emacs-frame-width  frame) v)))
        ('height       (when (integerp v) (setf (emacs-frame-height frame) v)))
        ('left         (when (integerp v) (setf (emacs-frame-left   frame) v)))
        ('top          (when (integerp v) (setf (emacs-frame-top    frame) v)))
        ('name         (when (stringp v)  (setf (emacs-frame-name   frame) v)))
        ('visibility   (setf (emacs-frame-visible frame) v))
        (_             nil))
      ;; Always remember the param verbatim for `frame-parameter'.
      (setf (emacs-frame-parameters frame)
            (cons (cons k v)
                  (assq-delete-all k (emacs-frame-parameters frame))))))
  ;; Recompute pixel size after width/height application.
  (setf (emacs-frame-pixel-width  frame)
        (* (emacs-frame-width  frame) emacs-frame--char-width))
  (setf (emacs-frame-pixel-height frame)
        (* (emacs-frame-height frame) emacs-frame--char-height))
  frame)

(defun emacs-frame-make-frame (&optional params)
  "Create and return a new frame.

PARAMS is an alist of (PARAMETER . VALUE) pairs.  Recognized keys:
  width / height       : initial size in cols/lines (default 80x24).
  left / top           : on-display position in pseudo pixels.
  name                 : human-readable name.
  visibility           : t (default), nil, or `iconified'.

Other keys are stored verbatim in `frame-parameters' but otherwise
ignored at stub-mode level.

Doc 34 §2.11 invariant: returns a unique frame-id object that is
`eq'-distinct from every prior return value, even across frame
deletes."
  (emacs-frame--ensure-initial)
  (let* ((id (emacs-frame--next-id))
         (f  (emacs-frame--make
              :id      id
              :name    (or (cdr (assq 'name params)) (format "F%d" id))
              :backend (emacs-frame-current-backend))))
    (emacs-frame--apply-params f params)
    (setq emacs-frame--registry
          (append emacs-frame--registry (list f)))
    (let ((obj (emacs-frame--call-backend :frame-create f params)))
      (when obj (setf (emacs-frame-backend-obj f) obj)))
    f))

(defun emacs-frame-delete-frame (&optional frame _force)
  "Delete FRAME (default: the selected frame).

In stub mode this updates the registry only (= no backend call).
With a real backend it also dispatches to `:frame-destroy'.

Signals `emacs-frame-only' if FRAME is the sole live frame, to
preserve the Emacs invariant that at least one frame remains.

Doc 34 §2.11 invariant: the `frame-id' of the deleted frame is
NOT recycled (= identity remains opaquely unique forever)."
  (let ((f (emacs-frame--get frame)))
    (when (= 1 (length (emacs-frame-frame-list)))
      (signal 'emacs-frame-only (list f)))
    (emacs-frame--call-backend :frame-destroy f)
    (setf (emacs-frame-dead-p f) t)
    (when (eq f emacs-frame--selected)
      (setq emacs-frame--selected
            (cl-find-if-not #'emacs-frame-dead-p emacs-frame--registry)))
    (when (eq f emacs-frame--focus)
      (setq emacs-frame--focus emacs-frame--selected))
    nil))

(defun emacs-frame-delete-other-frames (&optional frame)
  "Delete every live frame other than FRAME (default: selected frame)."
  (let ((keep (emacs-frame--get frame)))
    (dolist (f (emacs-frame-frame-list))
      (unless (eq f keep)
        (emacs-frame-delete-frame f)))
    nil))

;;; C. frame size

(defun emacs-frame-frame-width (&optional frame)
  "Return the width in columns of FRAME (default: selected frame).

Doc 34 §2.11 LOCKED invariant: stub-mode default = 80."
  (emacs-frame-width (emacs-frame--get frame)))

(defun emacs-frame-frame-height (&optional frame)
  "Return the height in lines of FRAME (default: selected frame).

Doc 34 §2.11 LOCKED invariant: stub-mode default = 24."
  (emacs-frame-height (emacs-frame--get frame)))

(defun emacs-frame-frame-char-width (&optional frame)
  "Return the width in pseudo pixels of one character cell on FRAME."
  (ignore (emacs-frame--get frame))
  emacs-frame--char-width)

(defun emacs-frame-frame-char-height (&optional frame)
  "Return the height in pseudo pixels of one character cell on FRAME."
  (ignore (emacs-frame--get frame))
  emacs-frame--char-height)

(defun emacs-frame-frame-pixel-width (&optional frame)
  "Return the width in pseudo pixels of FRAME."
  (emacs-frame-pixel-width (emacs-frame--get frame)))

(defun emacs-frame-frame-pixel-height (&optional frame)
  "Return the height in pseudo pixels of FRAME."
  (emacs-frame-pixel-height (emacs-frame--get frame)))

(defun emacs-frame-set-frame-size (frame cols lines &optional _pixelwise)
  "Resize FRAME to COLS x LINES.

Signals `emacs-frame-bad-size' if either dimension is below the
configured minimum."
  (let ((f (emacs-frame--get frame)))
    (unless (and (integerp cols)  (>= cols  emacs-frame--min-cols))
      (signal 'emacs-frame-bad-size (list 'cols cols)))
    (unless (and (integerp lines) (>= lines emacs-frame--min-lines))
      (signal 'emacs-frame-bad-size (list 'lines lines)))
    (setf (emacs-frame-width        f) cols
          (emacs-frame-height       f) lines
          (emacs-frame-pixel-width  f) (* cols  emacs-frame--char-width)
          (emacs-frame-pixel-height f) (* lines emacs-frame--char-height))
    (emacs-frame--call-backend :frame-resize f cols lines)
    nil))

;;; D. frame position

(defun emacs-frame-set-frame-position (frame x y)
  "Set the on-display position of FRAME to (X, Y) in pseudo pixels."
  (let ((f (emacs-frame--get frame)))
    (unless (integerp x) (signal 'wrong-type-argument (list 'integerp x)))
    (unless (integerp y) (signal 'wrong-type-argument (list 'integerp y)))
    (setf (emacs-frame-left f) x
          (emacs-frame-top  f) y)
    (emacs-frame--call-backend :frame-position f x y)
    nil))

;;; E. frame parameters

(defun emacs-frame-frame-parameters (&optional frame)
  "Return an alist of all parameters of FRAME (default: selected frame).

In addition to user-supplied parameters this always includes the
core derived values: `width', `height', `pixel-width',
`pixel-height', `left', `top', `name', `visibility'."
  (let* ((f (emacs-frame--get frame))
         (user (copy-sequence (emacs-frame-parameters f))))
    (dolist (kv `((width      . ,(emacs-frame-width        f))
                  (height     . ,(emacs-frame-height       f))
                  (pixel-width  . ,(emacs-frame-pixel-width  f))
                  (pixel-height . ,(emacs-frame-pixel-height f))
                  (left       . ,(emacs-frame-left         f))
                  (top        . ,(emacs-frame-top          f))
                  (name       . ,(emacs-frame-name         f))
                  (visibility . ,(emacs-frame-visible      f))))
      (unless (assq (car kv) user) (push kv user)))
    user))

(defun emacs-frame-frame-parameter (frame parameter)
  "Return the value of PARAMETER on FRAME, or nil if unset."
  (cdr (assq parameter (emacs-frame-frame-parameters frame))))

(defun emacs-frame-set-frame-parameter (frame parameter value)
  "Set PARAMETER on FRAME to VALUE.  Returns VALUE."
  (emacs-frame--apply-params (emacs-frame--get frame)
                             (list (cons parameter value)))
  value)

(defun emacs-frame-modify-frame-parameters (frame alist)
  "Apply ALIST of (PARAMETER . VALUE) pairs to FRAME, returning nil."
  (emacs-frame--apply-params (emacs-frame--get frame) alist)
  nil)

;;; F. frame visibility / Z-order

(defun emacs-frame-frame-visible-p (&optional frame)
  "Return non-nil if FRAME is visible (`t' or `iconified')."
  (emacs-frame-visible (emacs-frame--get frame)))

(defun emacs-frame-make-frame-visible (&optional frame)
  "Mark FRAME visible (= visibility `t').  Returns FRAME."
  (let ((f (emacs-frame--get frame)))
    (setf (emacs-frame-visible f) t)
    (emacs-frame--call-backend :frame-visible f t)
    f))

(defun emacs-frame-make-frame-invisible (&optional frame _force)
  "Mark FRAME invisible (= visibility nil).  Returns FRAME."
  (let ((f (emacs-frame--get frame)))
    (setf (emacs-frame-visible f) nil)
    (emacs-frame--call-backend :frame-visible f nil)
    f))

(defun emacs-frame-raise-frame (&optional frame)
  "Raise FRAME (= move to end of registry; selection unchanged)."
  (let ((f (emacs-frame--get frame)))
    (setq emacs-frame--registry
          (append (delq f emacs-frame--registry) (list f)))
    (emacs-frame--call-backend :frame-raise f)
    f))

(defun emacs-frame-lower-frame (&optional frame)
  "Lower FRAME (= move to front of registry; selection unchanged)."
  (let ((f (emacs-frame--get frame)))
    (setq emacs-frame--registry
          (cons f (delq f emacs-frame--registry)))
    (emacs-frame--call-backend :frame-lower f)
    f))

;;; G. frame selection / focus

(defun emacs-frame-select-frame (frame &optional _norecord)
  "Make FRAME the selected frame.  Returns FRAME."
  (let ((f (emacs-frame--check-live frame)))
    (setq emacs-frame--selected f
          emacs-frame--focus    f)
    f))

(defun emacs-frame-frame-focus (&optional frame)
  "Return the frame currently holding input focus.

With FRAME non-nil, return the focus relationship of FRAME (in
stub mode this is FRAME itself if it is the focused frame, else
nil); without FRAME, return the focused frame."
  (cond
   ((null frame) emacs-frame--focus)
   (t (let ((f (emacs-frame--check-live frame)))
        (and (eq f emacs-frame--focus) f)))))

;;; H. frame->windows + display

(defun emacs-frame-frame-windows (&optional frame)
  "Return a list of windows on FRAME.

In stub-mode this delegates to `emacs-window-window-list' if that
function is bound; otherwise returns the frame's `root-window' as
a one-element list (or nil if no root-window has been wired in)."
  (let* ((f (emacs-frame--get frame))
         (root (emacs-frame-root-window f)))
    (cond
     ((fboundp 'emacs-window-window-list)
      ;; emacs-window's Phase 1 single-frame stub ignores its FRAME
      ;; arg, so this still returns the right list.
      (funcall (symbol-function 'emacs-window-window-list) f))
     (root (list root))
     (t    nil))))

(defun emacs-frame-display-pixel-width (&optional _display)
  "Return the pseudo pixel-width of the implicit single display."
  emacs-frame--display-cols)

(defun emacs-frame-display-pixel-height (&optional _display)
  "Return the pseudo pixel-height of the implicit single display."
  emacs-frame--display-lines)

(provide 'emacs-frame)

;;; emacs-frame.el ends here
