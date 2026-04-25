;;; emacs-tui-stub.el --- Phase 1 close gate TUI stub backend  -*- lexical-binding: t; -*-

;; Phase 1 module 6/6 per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2).
;; Layer: nelisp-emacs (Layer 2 extension on top of NeLisp).
;; Namespace: `emacs-tui-stub-' so loading inside a host Emacs does NOT
;; shadow any `tui-' or `display-' symbol.
;;
;; Foundation contracts:
;;   - Doc 34 v2 §2.11 frame stub mode invariant (LOCKED, 80x24 fixed +
;;     unique frame-id + delete-frame registry-only).
;;   - Doc 43 v2 §2.1 frame swap-in protocol (= step 1 = `stub' backend
;;     receipt path; this module IS that path's reference impl).
;;   - Doc 43 v2 §2.5a degrade contract (= unsupported capability API
;;     calls signal `display-spec-unsupported' with :capability / :api /
;;     :backend plist data; *no silent no-op / nil return / crash*).
;;   - Doc 43 v2 §2.5 capability matrix (= MVP minimum: `text' /
;;     `basic-color' / `keyboard' / `resize' / `layout-box' /
;;     `layout-grid'; everything else returns nil from `capability-p').
;;
;; Role in the architecture:
;;   - Phase 1 close gate `minimum impl': everything is no-op + log;
;;     `flush' does NOT actually paint anything to a real terminal.  The
;;     module's job is to provide a backend handle that satisfies the
;;     `frame stub mode invariant' so that `emacs-frame.el' /
;;     `emacs-window.el' integration ERT can run end-to-end without
;;     touching ncurses / terminfo.
;;   - Phase 2 (Doc 43 §3.1 Phase 11.A) replaces the canvas / event /
;;     capability primitives with `emacs-tui-backend.el' +
;;     `emacs-tui-terminfo.el' + `emacs-tui-event.el'.  This stub is
;;     retained as the `:backend stub' selectable path for ERT and for
;;     headless smoke runs.
;;
;; API surface (~13 public APIs):
;;
;;   A. backend lifecycle  (3 APIs)
;;      emacs-tui-stub-init           — return a fresh backend handle
;;      emacs-tui-stub-shutdown       — tear down a handle
;;      emacs-tui-stub-handlep        — predicate
;;
;;   B. capability query  (2 APIs + 1 var + 1 condition reuse)
;;      emacs-tui-stub-capabilities   — backend declared capability list
;;      emacs-tui-stub-get-capability — bool, O(1) membership test
;;
;;   C. frame management  (3 APIs)
;;      emacs-tui-stub-frame-create   — register and return a frame
;;      emacs-tui-stub-frame-destroy  — registry update only
;;      emacs-tui-stub-frame-resize   — adjust width/height (stub: no-op
;;                                      vs. invariant 80x24, see §2.11)
;;
;;   D. canvas drawing  (3 APIs)
;;      emacs-tui-stub-canvas-clear     — clear a frame's canvas
;;      emacs-tui-stub-canvas-draw-text — write TEXT at (ROW, COL) w/ FACE
;;      emacs-tui-stub-canvas-flush     — flush pending writes (no-op log)
;;
;;   E. event polling  (2 APIs)
;;      emacs-tui-stub-event-poll       — return next event or nil
;;      emacs-tui-stub-event-inject     — test helper, push synthetic event
;;
;; Non-goals (deferred per task spec):
;;   - actual TUI rendering / ncurses / terminfo (= Phase 2,
;;     `emacs-tui-backend.el', Doc 43 §3.1 Phase 11.A).
;;   - input event interpretation (= Phase 2 `emacs-tui-event.el',
;;     this stub only buffers and replays).
;;   - font / glyph metrics / shaping (= Phase 11.B, Doc 43 §2.3).
;;   - mouse / 256-color / truecolor / image-* / IME / bidi (= Doc 43
;;     §2.5 capability matrix flags = `-' for TUI MVP).

;;; Code:

(require 'cl-lib)

;;; Errors (Doc 43 §2.5a degrade contract)

(define-error 'emacs-tui-stub-error
  "emacs-tui-stub error")

(define-error 'emacs-tui-stub-bad-handle
  "Not an emacs-tui-stub backend handle"
  'emacs-tui-stub-error)

(define-error 'emacs-tui-stub-bad-frame
  "Frame not registered with this backend"
  'emacs-tui-stub-error)

;; Doc 43 §2.5a `display-spec-unsupported' is the *cross-backend*
;; portable degrade signal.  In Phase 1 the upstream NeLisp side does
;; not yet define it, so we define it locally guarded by `unless' so
;; that Phase 2's import remains byte-identical.
(unless (get 'display-spec-unsupported 'error-conditions)
  (define-error 'display-spec-unsupported
    "Display capability not supported by current backend"))

;;; Contract version constants (per Doc 34 §2.11 / Doc 43 §2.1 / §2.5a)

(defconst emacs-tui-stub-frame-stub-invariant-version 1
  "FRAME_STUB_INVARIANT_VERSION per Doc 34 v2 §2.11.
Bumped on incompatible change to the 80x24-fixed + unique frame-id +
delete-frame-registry-only invariants.")

(defconst emacs-tui-stub-degrade-contract-version 1
  "DEGRADE_CONTRACT_VERSION per Doc 43 v2 §2.5a.
Bumped on incompatible change to the `display-spec-unsupported'
condition data plist (= :capability / :api / :backend keys).")

(defconst emacs-tui-stub-frame-default-width 80
  "Frame default width per Doc 34 v2 §2.11 LOCKED invariant.")

(defconst emacs-tui-stub-frame-default-height 24
  "Frame default height per Doc 34 v2 §2.11 LOCKED invariant.")

;;; Customization

(defcustom emacs-tui-stub-log-enabled nil
  "When non-nil, write a one-line log entry per stub no-op operation.
The log buffer is named `*emacs-tui-stub-log*' and is created lazily.
Default nil keeps ERT runs silent."
  :type 'boolean
  :group 'emacs-tui-stub)

(defcustom emacs-tui-stub-resize-allowed nil
  "When non-nil, `emacs-tui-stub-frame-resize' updates width/height.
When nil (default), frames stay at the Doc 34 v2 §2.11 LOCKED 80x24.
Set to t in Phase 11.A integration tests that exercise resize plumbing
without a real backend."
  :type 'boolean
  :group 'emacs-tui-stub)

(defgroup emacs-tui-stub nil
  "Phase 1 close gate TUI stub backend (no-op + log)."
  :group 'emacs)

;;; Backend handle struct

(cl-defstruct (emacs-tui-stub-handle
               (:constructor emacs-tui-stub--make-handle)
               (:copier nil)
               (:predicate emacs-tui-stub-handlep))
  "Opaque backend handle returned by `emacs-tui-stub-init'."
  (id           nil :read-only t)        ;; gensym for human-readable id
  (alive-p      t)                       ;; nil after shutdown
  (capabilities nil :read-only t)        ;; list of capability symbols
  (frames       nil)                     ;; alist (frame-id . frame-rec)
  (next-frame-id 1)                      ;; monotonic frame-id counter
  (event-queue  nil))                    ;; FIFO list of pending events

;;; Frame record (per-frame state inside a backend handle)

(cl-defstruct (emacs-tui-stub-frame
               (:constructor emacs-tui-stub--make-frame)
               (:copier nil)
               (:predicate emacs-tui-stub-framep))
  "A registered frame record inside a stub backend."
  (id      nil :read-only t)             ;; integer, unique within handle
  (name    nil :read-only t)             ;; user-visible label
  (width   nil)                          ;; current width (Doc 34 §2.11)
  (height  nil)                          ;; current height
  (params  nil)                          ;; alist of frame parameters
  (canvas  nil)                          ;; vector of vectors (row * col)
  (dirty   t))                           ;; t = needs flush

;;; Module-private id generator (independent of any frame)

(defvar emacs-tui-stub--handle-counter 0
  "Monotonic counter for handle ids (printable as `stub-N').")

;;; Logging

(defun emacs-tui-stub--log (fmt &rest args)
  "Append an entry to `*emacs-tui-stub-log*' if logging is enabled.
FMT and ARGS are passed straight to `format'."
  (when emacs-tui-stub-log-enabled
    (let ((buf (get-buffer-create "*emacs-tui-stub-log*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (apply #'format fmt args) "\n")))))

;;; Capability list (Doc 43 §2.5 TUI MVP minimum + stub-specific subset)

(defconst emacs-tui-stub-default-capabilities
  '(text basic-color keyboard resize layout-box layout-grid)
  "Doc 43 §2.5 TUI MVP capability subset declared by the stub backend.
This is intentionally a *strict subset* of `emacs-tui-backend.el'
Phase 11.A capabilities so that application code which is
capability-aware (= Doc 43 §2.5a `display-spec-capability-p' guard)
exercises both the supported and unsupported paths during Phase 1
ERT.")

;;; A. backend lifecycle

;;;###autoload
(defun emacs-tui-stub-init (&optional capabilities)
  "Initialize a fresh stub backend and return its handle.
CAPABILITIES, if non-nil, overrides
`emacs-tui-stub-default-capabilities' (= Doc 43 §2.5 MVP subset).

Returns an `emacs-tui-stub-handle' satisfying
`emacs-tui-stub-handlep'."
  (let* ((counter (cl-incf emacs-tui-stub--handle-counter))
         (id (intern (format "stub-%d" counter)))
         (caps (or capabilities emacs-tui-stub-default-capabilities))
         (handle (emacs-tui-stub--make-handle
                  :id id
                  :alive-p t
                  :capabilities (copy-sequence caps)
                  :frames nil
                  :next-frame-id 1
                  :event-queue nil)))
    (emacs-tui-stub--log "init handle=%S caps=%S" id caps)
    handle))

;;;###autoload
(defun emacs-tui-stub-shutdown (handle)
  "Tear down HANDLE (mark it not alive and drop frames + queue).
After shutdown, calling any operation other than
`emacs-tui-stub-handlep' on HANDLE signals
`emacs-tui-stub-bad-handle'.  Returns t."
  (emacs-tui-stub--check-handle handle)
  (emacs-tui-stub--log "shutdown handle=%S frames=%d events=%d"
                       (emacs-tui-stub-handle-id handle)
                       (length (emacs-tui-stub-handle-frames handle))
                       (length (emacs-tui-stub-handle-event-queue handle)))
  (setf (emacs-tui-stub-handle-alive-p handle) nil)
  (setf (emacs-tui-stub-handle-frames handle) nil)
  (setf (emacs-tui-stub-handle-event-queue handle) nil)
  t)

(defun emacs-tui-stub--check-handle (handle)
  "Signal `emacs-tui-stub-bad-handle' unless HANDLE is alive."
  (unless (emacs-tui-stub-handlep handle)
    (signal 'emacs-tui-stub-bad-handle (list handle)))
  (unless (emacs-tui-stub-handle-alive-p handle)
    (signal 'emacs-tui-stub-bad-handle
            (list 'shutdown (emacs-tui-stub-handle-id handle)))))

;;; B. capability query (Doc 43 §2.5 / §2.5a)

(defun emacs-tui-stub-capabilities (handle)
  "Return HANDLE's declared capability list (a fresh copy)."
  (emacs-tui-stub--check-handle handle)
  (copy-sequence (emacs-tui-stub-handle-capabilities handle)))

(defun emacs-tui-stub-get-capability (handle cap-name)
  "Return non-nil iff CAP-NAME is declared by HANDLE.
Equivalent to `display-spec-capability-p' for the stub backend.
Returns t / nil only (never raises for unknown capability — that is
the Doc 43 §2.5a `pre-check guard' contract)."
  (emacs-tui-stub--check-handle handle)
  (and (memq cap-name (emacs-tui-stub-handle-capabilities handle)) t))

(defun emacs-tui-stub--require-capability (handle cap-name api-name)
  "Signal `display-spec-unsupported' unless CAP-NAME is declared.
HANDLE = backend, API-NAME = the symbol naming the caller for the
condition data plist (Doc 43 §2.5a invariant 2)."
  (unless (emacs-tui-stub-get-capability handle cap-name)
    (signal 'display-spec-unsupported
            (list :capability cap-name
                  :api api-name
                  :backend 'stub))))

;;; C. frame management (Doc 34 v2 §2.11)

;;;###autoload
(defun emacs-tui-stub-frame-create (handle name &optional params)
  "Register a fresh frame named NAME in HANDLE and return it.
NAME is a string label; PARAMS is an optional alist of frame
parameters (subset only — Doc 43 §2.1 invariant `stub では subset').

Returns an `emacs-tui-stub-frame' with id unique within HANDLE,
width = `emacs-tui-stub-frame-default-width' (80),
height = `emacs-tui-stub-frame-default-height' (24)."
  (emacs-tui-stub--check-handle handle)
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((fid (emacs-tui-stub-handle-next-frame-id handle))
         (frame (emacs-tui-stub--make-frame
                 :id fid
                 :name name
                 :width emacs-tui-stub-frame-default-width
                 :height emacs-tui-stub-frame-default-height
                 :params (copy-sequence params)
                 :canvas (emacs-tui-stub--make-canvas
                          emacs-tui-stub-frame-default-width
                          emacs-tui-stub-frame-default-height)
                 :dirty t)))
    (setf (emacs-tui-stub-handle-next-frame-id handle) (1+ fid))
    (push (cons fid frame) (emacs-tui-stub-handle-frames handle))
    (emacs-tui-stub--log "frame-create handle=%S id=%d name=%S"
                         (emacs-tui-stub-handle-id handle) fid name)
    frame))

;;;###autoload
(defun emacs-tui-stub-frame-destroy (handle frame)
  "Remove FRAME from HANDLE's registry.
Stub-only path per Doc 34 v2 §2.11 (`registry update のみ').  Returns
t on success; raises `emacs-tui-stub-bad-frame' if FRAME is not
registered with HANDLE."
  (emacs-tui-stub--check-handle handle)
  (emacs-tui-stub--check-frame handle frame)
  (let ((fid (emacs-tui-stub-frame-id frame)))
    (setf (emacs-tui-stub-handle-frames handle)
          (assq-delete-all fid (emacs-tui-stub-handle-frames handle)))
    (emacs-tui-stub--log "frame-destroy handle=%S id=%d"
                         (emacs-tui-stub-handle-id handle) fid))
  t)

;;;###autoload
(defun emacs-tui-stub-frame-resize (handle frame width height)
  "Resize FRAME inside HANDLE to WIDTH x HEIGHT.
Per Doc 34 v2 §2.11 LOCKED invariant the stub mode is fixed at 80x24,
so by default this is a no-op (logged) and the frame's width/height
remain at their LOCKED values.  Setting `emacs-tui-stub-resize-allowed'
to non-nil reallocates the canvas to (WIDTH HEIGHT) — used by Phase
11.A integration tests that exercise resize plumbing.

Returns the frame."
  (emacs-tui-stub--check-handle handle)
  (emacs-tui-stub--check-frame handle frame)
  (unless (and (integerp width) (> width 0))
    (signal 'wrong-type-argument (list 'positive-integer width)))
  (unless (and (integerp height) (> height 0))
    (signal 'wrong-type-argument (list 'positive-integer height)))
  (cond
   (emacs-tui-stub-resize-allowed
    (setf (emacs-tui-stub-frame-width frame) width
          (emacs-tui-stub-frame-height frame) height
          (emacs-tui-stub-frame-canvas frame)
          (emacs-tui-stub--make-canvas width height)
          (emacs-tui-stub-frame-dirty frame) t)
    (emacs-tui-stub--log "frame-resize handle=%S id=%d %dx%d (applied)"
                         (emacs-tui-stub-handle-id handle)
                         (emacs-tui-stub-frame-id frame)
                         width height))
   (t
    (emacs-tui-stub--log
     "frame-resize handle=%S id=%d %dx%d (no-op; invariant=%dx%d)"
     (emacs-tui-stub-handle-id handle)
     (emacs-tui-stub-frame-id frame)
     width height
     emacs-tui-stub-frame-default-width
     emacs-tui-stub-frame-default-height)))
  frame)

(defun emacs-tui-stub--check-frame (handle frame)
  "Signal `emacs-tui-stub-bad-frame' unless FRAME is registered."
  (unless (emacs-tui-stub-framep frame)
    (signal 'emacs-tui-stub-bad-frame (list 'not-frame frame)))
  (unless (assq (emacs-tui-stub-frame-id frame)
                (emacs-tui-stub-handle-frames handle))
    (signal 'emacs-tui-stub-bad-frame
            (list 'unknown-frame
                  (emacs-tui-stub-frame-id frame)
                  (emacs-tui-stub-handle-id handle)))))

;;; D. canvas drawing

(defun emacs-tui-stub--make-canvas (width height)
  "Allocate a fresh HEIGHT x WIDTH canvas filled with space + nil face.
Each cell is a (CHAR . FACE) cons; the canvas is a vector of vectors."
  (let ((rows (make-vector height nil)))
    (dotimes (r height)
      (let ((row (make-vector width nil)))
        (dotimes (c width)
          (aset row c (cons ?\s nil)))
        (aset rows r row)))
    rows))

;;;###autoload
(defun emacs-tui-stub-canvas-clear (handle frame)
  "Clear FRAME's canvas to spaces (= Doc 34 v2 §2.11 stub fill).
Marks the frame dirty so the next `flush' call logs the redraw.
Returns the frame."
  (emacs-tui-stub--check-handle handle)
  (emacs-tui-stub--check-frame handle frame)
  (setf (emacs-tui-stub-frame-canvas frame)
        (emacs-tui-stub--make-canvas
         (emacs-tui-stub-frame-width frame)
         (emacs-tui-stub-frame-height frame))
        (emacs-tui-stub-frame-dirty frame) t)
  (emacs-tui-stub--log "canvas-clear handle=%S id=%d"
                       (emacs-tui-stub-handle-id handle)
                       (emacs-tui-stub-frame-id frame))
  frame)

;;;###autoload
(defun emacs-tui-stub-canvas-draw-text (handle frame row col text &optional face)
  "Paint TEXT at (ROW, COL) on FRAME's canvas with optional FACE.
Per Doc 43 §2.5 the stub backend declares `text' + `basic-color', so
TEXT is *always* accepted and FACE is opaque (stored as-is).  Out-
of-bounds writes are clipped silently (= stub policy, real backend
will signal at Phase 11.A).

Returns the number of cells actually written (0 if fully clipped)."
  (emacs-tui-stub--check-handle handle)
  (emacs-tui-stub--check-frame handle frame)
  (emacs-tui-stub--require-capability handle 'text 'canvas-draw-text)
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (unless (and (integerp row) (>= row 0))
    (signal 'wrong-type-argument (list 'natnum row)))
  (unless (and (integerp col) (>= col 0))
    (signal 'wrong-type-argument (list 'natnum col)))
  (let* ((width  (emacs-tui-stub-frame-width  frame))
         (height (emacs-tui-stub-frame-height frame))
         (canvas (emacs-tui-stub-frame-canvas frame))
         (written 0))
    (when (and (< row height) (< col width))
      (let* ((row-vec (aref canvas row))
             (n (length text))
             (limit (min n (- width col))))
        (dotimes (i limit)
          (aset row-vec (+ col i) (cons (aref text i) face)))
        (setq written limit)
        (setf (emacs-tui-stub-frame-dirty frame) t)))
    (emacs-tui-stub--log "canvas-draw-text handle=%S id=%d (%d,%d) %S face=%S written=%d"
                         (emacs-tui-stub-handle-id handle)
                         (emacs-tui-stub-frame-id frame)
                         row col text face written)
    written))

;;;###autoload
(defun emacs-tui-stub-canvas-flush (handle frame)
  "Flush FRAME's pending canvas writes (stub: log + clear dirty bit).
A real backend (= Phase 11.A `emacs-tui-backend.el') will issue
terminal escape sequences here.  Returns t if the frame was dirty
(= a flush would have been issued), nil otherwise."
  (emacs-tui-stub--check-handle handle)
  (emacs-tui-stub--check-frame handle frame)
  (let ((dirty (emacs-tui-stub-frame-dirty frame)))
    (when dirty
      (setf (emacs-tui-stub-frame-dirty frame) nil)
      (emacs-tui-stub--log "canvas-flush handle=%S id=%d"
                           (emacs-tui-stub-handle-id handle)
                           (emacs-tui-stub-frame-id frame)))
    dirty))

;;; E. event polling

;;;###autoload
(defun emacs-tui-stub-event-poll (handle)
  "Pop and return the next pending event from HANDLE, or nil if empty.
Events are arbitrary Lisp objects (Phase 1 stub does not interpret
them — Phase 11.A `emacs-tui-event.el' will parse stdin / SIGWINCH
into structured events).  Use `emacs-tui-stub-event-inject' from
ERT to push synthetic events."
  (emacs-tui-stub--check-handle handle)
  (let ((q (emacs-tui-stub-handle-event-queue handle)))
    (when q
      (let ((ev (car q)))
        (setf (emacs-tui-stub-handle-event-queue handle) (cdr q))
        (emacs-tui-stub--log "event-poll handle=%S ev=%S"
                             (emacs-tui-stub-handle-id handle) ev)
        ev))))

(defun emacs-tui-stub-event-inject (handle event)
  "Append EVENT to HANDLE's event queue (test helper).
Returns the new queue length."
  (emacs-tui-stub--check-handle handle)
  (setf (emacs-tui-stub-handle-event-queue handle)
        (append (emacs-tui-stub-handle-event-queue handle)
                (list event)))
  (length (emacs-tui-stub-handle-event-queue handle)))

(provide 'emacs-tui-stub)

;;; emacs-tui-stub.el ends here
