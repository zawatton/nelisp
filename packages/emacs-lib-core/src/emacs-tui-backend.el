;;; emacs-tui-backend.el --- Phase 2 TUI MVP backend (ANSI escape based)  -*- lexical-binding: t; -*-

;; Phase 2 module per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2 §3.2),
;; mirroring NeLisp Doc 43 v2 §3.1 Phase 11.A TUI MVP reference impl.
;; Layer: nelisp-emacs (Layer 2/3 extension on top of NeLisp).
;; Namespace: `emacs-tui-backend-' so loading inside a host Emacs does
;; NOT shadow any `tui-' or `display-' symbol.
;;
;; Foundation contracts (LOCKED):
;;   - Doc 34 v2 §2.11 frame stub mode invariant — real backend takes
;;     over from `emacs-tui-stub.el', honoring the swap-in protocol.
;;   - Doc 43 v2 §2.1 frame swap-in protocol = step 2 = real backend.
;;   - Doc 43 v2 §2.5 capability matrix — TUI MVP minimum subset =
;;     `text' / `basic-color' / `keyboard' / `resize' / `layout-box' /
;;     `layout-grid'.  256-color / truecolor / mouse / image-* / IME /
;;     bidi / composition are *NOT* declared (= signal
;;     `display-spec-unsupported').
;;   - Doc 43 v2 §2.5a degrade contract — every API requiring an
;;     undeclared capability signals `display-spec-unsupported' with
;;     the LOCKED data plist (= :capability / :api / :backend).
;;   - Doc 43 v2 §2.6 event-source pull-on-demand contract — the
;;     `event-poll' API is a strict pull (= `nil' on empty queue,
;;     never blocks beyond the optional TIMEOUT-MS argument).
;;
;; Role in the architecture:
;;   - `emacs-tui-stub.el' (Phase 1) was the no-op + log reference for
;;     ERT during the Doc 34 §2.11 swap-in dry-run.  This Phase 2
;;     module is the *real* backend that emits ANSI escape sequences
;;     to a stream sink (default: `princ' to standard-output) so that
;;     a host Emacs run with `--batch` + tty stdout can display
;;     content end-to-end.
;;   - The output sink is parameterised via `emacs-tui-backend-output-fn'
;;     so that ERT can capture the escape stream into a buffer and
;;     assert on it without needing a real terminal.
;;   - `emacs-frame.el' (Doc 01 Phase 1, T140) provides the backend
;;     dispatch table; Phase 11.A wiring sets the dispatch alist to
;;     route to `emacs-tui-backend-*'.  The wiring itself is *not*
;;     part of this file (= performed by integration glue), but the
;;     API surface here matches the dispatch contract.
;;
;; API surface (~17 public APIs):
;;
;;   A. backend lifecycle (3 APIs)
;;      emacs-tui-backend-init           — return a fresh backend handle
;;      emacs-tui-backend-shutdown       — tear down a handle
;;      emacs-tui-backend-handlep        — predicate
;;
;;   B. capability query (2 APIs)
;;      emacs-tui-backend-capabilities   — declared capability list
;;      emacs-tui-backend-get-capability — bool, O(1) membership test
;;
;;   C. frame management (3 APIs)
;;      emacs-tui-backend-frame-create   — register & return a frame
;;      emacs-tui-backend-frame-destroy  — registry update
;;      emacs-tui-backend-frame-resize   — adjust width/height
;;
;;   D. canvas drawing (3 APIs)
;;      emacs-tui-backend-canvas-clear     — clear a frame's canvas
;;      emacs-tui-backend-canvas-draw-text — paint TEXT at (ROW, COL)
;;      emacs-tui-backend-canvas-flush     — emit pending writes as ANSI
;;
;;   E. event polling (1 API + 1 test helper)
;;      emacs-tui-backend-event-poll       — pull next event (non-blocking)
;;      emacs-tui-backend-event-inject     — test helper, push synthetic
;;
;;   F. cursor (2 APIs)
;;      emacs-tui-backend-cursor-show      — set + show cursor at (ROW, COL)
;;      emacs-tui-backend-cursor-hide      — hide cursor
;;
;;   G. resize listener (1 API)
;;      emacs-tui-backend-resize-listen    — register SIGWINCH callback
;;
;; Non-goals (deferred per Doc 43 §3.1 Phase 11.A scope):
;;   - redisplay engine glyph matrix (= Phase 11.B / Doc 01 Phase 3).
;;   - font shaping (= Phase 11.B v2.x, declared `font-shaping' = -).
;;   - mouse / image / IME (= TUI v2.x or GUI).
;;   - terminfo capability detection (= `emacs-tui-terminfo.el',
;;     separate Phase 2 module).
;;   - stdin parser + SIGWINCH installer (= `emacs-tui-event.el',
;;     separate Phase 2 module — this backend exposes the API surface
;;     and consumes events through `event-inject').
;;
;; Phase 3.B.3 (= the SGR emit half of Doc 43 §2.4 v2 color extension):
;; this file's `--sgr-from-face' now accepts 256-color descriptor
;; `(palette N)' and truecolor descriptor `(rgb R G B)' as alist
;; values, producing `\\e[38;5;Nm' / `\\e[48;5;Nm' (256-color) and
;; `\\e[38;2;R;G;Bm' / `\\e[48;2;R;G;Bm' (truecolor) escapes per
;; ECMA-48 / xterm conventions.  The parsing of user-facing color
;; spec forms (`\"#rrggbb\"', `(:r N :g N :b N)', `:palette-N', etc.)
;; lives in `emacs-redisplay--parse-color-spec'.

;;; Code:

(require 'cl-lib)

;;; Errors (Doc 43 §2.5a degrade contract)

(define-error 'emacs-tui-backend-error
  "emacs-tui-backend error")

(define-error 'emacs-tui-backend-bad-handle
  "Not an emacs-tui-backend handle"
  'emacs-tui-backend-error)

(define-error 'emacs-tui-backend-bad-frame
  "Frame not registered with this backend"
  'emacs-tui-backend-error)

;; Doc 43 §2.5a `display-spec-unsupported' is the *cross-backend*
;; portable degrade signal.  Define it locally guarded by `unless' so
;; that loading after `emacs-tui-stub.el' (which defines the same
;; condition) is a no-op, and so the upstream NeLisp side can later
;; provide the same definition without conflict.
(unless (get 'display-spec-unsupported 'error-conditions)
  (define-error 'display-spec-unsupported
    "Display capability not supported by current backend"))

;;; Contract version constants

(defconst emacs-tui-backend-frame-stub-invariant-version 1
  "FRAME_STUB_INVARIANT_VERSION per Doc 34 v2 §2.11.
Bumped on incompatible change to the swap-in protocol invariants
shared with `emacs-tui-stub.el'.")

(defconst emacs-tui-backend-degrade-contract-version 1
  "DEGRADE_CONTRACT_VERSION per Doc 43 v2 §2.5a.
Bumped on incompatible change to the `display-spec-unsupported'
condition data plist.")

(defconst emacs-tui-backend-event-source-contract-version 1
  "EVENT_SOURCE_CONTRACT_VERSION per Doc 34 v2 §2.4 + Doc 43 v2 §2.6.")

(defconst emacs-tui-backend-frame-default-width 80
  "Frame default width per Doc 34 v2 §2.11 LOCKED invariant.
The real backend MAY resize beyond this default; the stub remains
LOCKED at 80x24.")

(defconst emacs-tui-backend-frame-default-height 24
  "Frame default height per Doc 34 v2 §2.11 LOCKED invariant.")

;;; Customization

(defgroup emacs-tui-backend nil
  "Phase 2 TUI MVP backend (ANSI escape based)."
  :group 'emacs)

(defcustom emacs-tui-backend-output-fn nil
  "Function called with one string argument to emit ANSI escapes.
When nil (the default), `princ' to `standard-output' is used so that
a host Emacs run as `emacs --batch -Q' with attached tty actually
paints to the terminal.  ERT sets this to a buffer-appender function
to capture the escape stream without touching a real terminal."
  :type '(choice (const :tag "Default (princ)" nil)
                 function)
  :group 'emacs-tui-backend)

(defcustom emacs-tui-backend-color-mode '16-color
  "Color mode declared by the backend at init time.
Per Doc 43 §2.5 TUI MVP, only 16-color (= `basic-color' capability)
is in scope.  Setting this to `256-color' adds the `256-color' cap.
Setting to `truecolor' adds 256-color + truecolor caps (intended for
terminals that pass terminfo detection — Phase 11.A v2.x extension).
Defaults are conservative; ERT for the MVP path uses `16-color'."
  :type '(choice (const :tag "16 colors (TUI MVP)" 16-color)
                 (const :tag "256 colors (v2.x)"   256-color)
                 (const :tag "Truecolor (v2.x)"    truecolor))
  :group 'emacs-tui-backend)

(defcustom emacs-tui-backend-log-enabled nil
  "When non-nil, append a one-line log entry per backend op.
The log buffer is `*emacs-tui-backend-log*' and is created lazily.
Default nil keeps ERT runs silent."
  :type 'boolean
  :group 'emacs-tui-backend)

;;; ANSI escape primitives (low-level, internal)

(defconst emacs-tui-backend--csi "\e["
  "Control Sequence Introducer (= ESC + `[`).")

(defconst emacs-tui-backend--reset (concat "\e[" "0m")
  "SGR 0 = reset all attributes.")

(defconst emacs-tui-backend--cursor-hide (concat "\e[" "?25l")
  "DECTCEM hide cursor.")

(defconst emacs-tui-backend--cursor-show (concat "\e[" "?25h")
  "DECTCEM show cursor.")

(defconst emacs-tui-backend--cursor-save (concat "\e[" "s")
  "ANSI SCO save cursor position.")

(defconst emacs-tui-backend--cursor-restore (concat "\e[" "u")
  "ANSI SCO restore cursor position.")

(defconst emacs-tui-backend--clear-screen (concat "\e[" "2J")
  "Clear entire screen.")

(defconst emacs-tui-backend--clear-line (concat "\e[" "2K")
  "Clear current line.")

(defconst emacs-tui-backend--alt-screen-on (concat "\e[" "?1049h")
  "Switch to alternate screen buffer (xterm extension).")

(defconst emacs-tui-backend--alt-screen-off (concat "\e[" "?1049l")
  "Switch back from alternate screen buffer.")

(defun emacs-tui-backend--cup (row col)
  "Return the CUP (Cursor Position) escape moving to (ROW, COL).
ROW and COL are 0-based on the public API; the wire protocol is
1-based, so we add 1 internally."
  (concat "\e["
          (number-to-string (1+ row))
          ";"
          (number-to-string (1+ col))
          "H"))

(defconst emacs-tui-backend--ansi-fg-base 30
  "ANSI SGR foreground base offset (= `30 + color').")

(defconst emacs-tui-backend--ansi-bg-base 40
  "ANSI SGR background base offset (= `40 + color').")

(defconst emacs-tui-backend--color-name-table
  '((black   . 0) (red     . 1) (green   . 2) (yellow . 3)
    (blue    . 4) (magenta . 5) (cyan    . 6) (white  . 7))
  "8-color basic palette; index = ANSI SGR offset.
Bright variants (8..15) are obtained by adding the SGR `90+'/`100+'
escape, which we represent by accepting `bright-NAME' symbols.")

(defun emacs-tui-backend--color-code (color)
  "Resolve COLOR (a symbol) to an (OFFSET . BRIGHT-P) pair, or nil.
COLOR may be `red', `bright-red', `default', or nil.  Returns nil
for nil / `default' (= caller emits no SGR for that channel).

This is the 16-color (= legacy MVP) path.  256-color and truecolor
descriptors (cons-list shapes) are handled by
`emacs-tui-backend--color->sgr-tokens' (Phase 3.B.3)."
  (cond
   ((null color) nil)
   ((eq color 'default) nil)
   ((symbolp color)
    (let* ((name (symbol-name color))
           (bright-p (string-prefix-p "bright-" name))
           (base (if bright-p (intern (substring name 7)) color))
           (cell (assq base emacs-tui-backend--color-name-table)))
      (and cell (cons (cdr cell) bright-p))))
   (t nil)))

(defun emacs-tui-backend--color->sgr-tokens (color is-bg-p)
  "Return the SGR token list for COLOR rendered as fg or bg.

COLOR may be:
  nil / `default'         → nil (= no SGR for this channel)
  symbol (`red' / ...)    → 16-color, e.g. (\"31\") fg / (\"41\") bg
  (palette N)             → 256-color (\"38;5;N\" fg / \"48;5;N\" bg)
  (rgb R G B)             → truecolor (\"38;2;R;G;B\" fg / \"48;2;R;G;B\")

IS-BG-P is non-nil to render background, nil for foreground.
Returns a (possibly-empty) list of *string* tokens to be joined by
`;' inside a single `\\e[...m' SGR escape.

Phase 3.B.3 (Doc 43 §2.4 v2): 256-color + truecolor descriptors are
emitted per ECMA-48 / xterm.  Out-of-range integers are clamped to
[0,255] for robustness against bad face data (= MVP graceful
degrade)."
  (cl-flet ((clamp (n) (cond ((not (integerp n)) 0)
                             ((< n 0) 0) ((> n 255) 255) (t n))))
    (cond
     ;; nil / default — no token.
     ((null color) nil)
     ((eq color 'default) nil)
     ;; (palette N) — 256-color descriptor.
     ((and (consp color) (eq (car color) 'palette))
      (let* ((raw (if (consp (cdr color)) (cadr color) (cdr color)))
             (n (clamp raw)))
        (list (if is-bg-p "48;5;" "38;5;")
              (number-to-string n))))
     ;; (rgb R G B) — truecolor descriptor.
     ((and (consp color) (eq (car color) 'rgb))
      (let* ((rgb (cdr color))
             (r (clamp (nth 0 rgb)))
             (g (clamp (nth 1 rgb)))
             (b (clamp (nth 2 rgb))))
        (list (if is-bg-p "48;2;" "38;2;")
              (format "%d;%d;%d" r g b))))
     ;; Symbol — 16-color path via existing helper.
     ((symbolp color)
      (let ((cell (emacs-tui-backend--color-code color)))
        (when cell
          (list (number-to-string
                 (+ (car cell)
                    (cond ((and is-bg-p (cdr cell)) 100)
                          (is-bg-p emacs-tui-backend--ansi-bg-base)
                          ((cdr cell) 90)
                          (t emacs-tui-backend--ansi-fg-base))))))))
     ;; Anything else — silent ignore (= robust against bad data).
     (t nil))))

(defun emacs-tui-backend--sgr-from-face (face)
  "Build the SGR escape string corresponding to FACE.
FACE is an alist with keys `:foreground' / `:background' / `:bold' /
`:underline' / `:reverse' (subset; unknown keys are ignored).

Color values may be (Phase 3.B.3 extension):
  16-color  : a symbol (`red', `bright-blue', `default', ...)
  256-color : a list `(palette N)' with N = 0..255
  truecolor : a list `(rgb R G B)' with R/G/B = 0..255

Returns a possibly-empty string (= no escape if FACE is nil or empty)."
  (if (null face)
      ""
    (let ((parts nil))
      ;; Foreground
      (let ((toks (emacs-tui-backend--color->sgr-tokens
                   (cdr (assq :foreground face)) nil)))
        (when toks
          ;; Splice the (one or two) tokens contiguously — we then
          ;; rely on `;' joining via mapconcat to produce e.g.
          ;; "38;5;200" within a single SGR.  The two-element form
          ;; "38;5;" + "N" already encodes its own internal ';' so we
          ;; concat them with no separator before pushing as a single
          ;; logical part.
          (push (mapconcat #'identity toks "") parts)))
      ;; Background
      (let ((toks (emacs-tui-backend--color->sgr-tokens
                   (cdr (assq :background face)) t)))
        (when toks
          (push (mapconcat #'identity toks "") parts)))
      ;; Attributes
      (when (cdr (assq :bold face))      (push "1" parts))
      (when (cdr (assq :underline face)) (push "4" parts))
      (when (cdr (assq :reverse face))   (push "7" parts))
      (if (null parts)
          ""
        (format "\e[%sm" (mapconcat #'identity (nreverse parts) ";"))))))

;;; Output emission

(defun emacs-tui-backend--emit (string)
  "Write STRING to the configured output sink.
Honors `emacs-tui-backend-output-fn' (function of one string) or
falls back to `princ' on `standard-output' for terminal use."
  (if emacs-tui-backend-output-fn
      (funcall emacs-tui-backend-output-fn string)
    (princ string)))

;;; Logging

(defun emacs-tui-backend--log (fmt &rest args)
  "Append a log entry to `*emacs-tui-backend-log*' if logging enabled.
FMT and ARGS are passed straight to `format'."
  (when emacs-tui-backend-log-enabled
    (let ((buf (get-buffer-create "*emacs-tui-backend-log*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (apply #'format fmt args) "\n")))))

;;; Backend handle struct

(cl-defstruct (emacs-tui-backend-handle
               (:constructor emacs-tui-backend--make-handle)
               (:copier nil)
               (:predicate emacs-tui-backend-handlep))
  "Opaque backend handle returned by `emacs-tui-backend-init'."
  (id            nil :read-only t)        ;; gensym id, e.g. `tui-1'
  (alive-p       t)                       ;; nil after shutdown
  (capabilities  nil :read-only t)        ;; capability symbol list
  (frames        nil)                     ;; alist (frame-id . frame-rec)
  (next-frame-id 1)                       ;; monotonic frame-id counter
  (event-queue   nil)                     ;; FIFO list of pending events
  (resize-cb     nil)                     ;; resize listener callback
  (alt-screen-p  nil))                    ;; alternate screen toggled

;;; Frame record (per-frame state)

(cl-defstruct (emacs-tui-backend-frame
               (:constructor emacs-tui-backend--make-frame)
               (:copier nil)
               (:predicate emacs-tui-backend-framep))
  "A registered frame record inside a real TUI backend."
  (id      nil :read-only t)              ;; integer, unique within handle
  (name    nil :read-only t)              ;; user-visible label
  (width   nil)                           ;; current width
  (height  nil)                           ;; current height
  (params  nil)                           ;; alist of frame parameters
  (canvas  nil)                           ;; vector of vectors (row x col)
  (dirty-rows nil)                        ;; bitvector of dirty rows
  (cursor-row nil)                        ;; nil = hidden, else integer
  (cursor-col nil))                       ;; nil = hidden, else integer

(defun emacs-tui-backend-frame-set-dirty-rows (frame dirty-rows)
  "Set FRAME's dirty row bitvector to DIRTY-ROWS and return DIRTY-ROWS."
  (unless (emacs-tui-backend-framep frame)
    (signal 'emacs-tui-backend-bad-frame (list frame)))
  (setf (emacs-tui-backend-frame-dirty-rows frame) dirty-rows)
  dirty-rows)

(defconst emacs-tui-backend--blank-cell (cons ?\s nil)
  "Shared immutable blank canvas cell.
Canvas writes replace cells rather than mutating cell conses, so blank
slots can safely share this object.  This keeps frame creation from
allocating one cons per column before the first paint.")

(defvar emacs-tui-backend--blank-row-cache nil
  "Alist mapping row width to shared immutable blank row vectors.")

(defun emacs-tui-backend--blank-row (width)
  "Return the shared blank row vector for WIDTH."
  (let ((cell (assq width emacs-tui-backend--blank-row-cache)))
    (unless cell
      (setq cell (cons width (make-vector width emacs-tui-backend--blank-cell)))
      (push cell emacs-tui-backend--blank-row-cache))
    (cdr cell)))

(defun emacs-tui-backend--blank-string-p (string)
  "Return non-nil if STRING contains only spaces."
  (let ((i 0)
        (n (length string))
        (ok t))
    (while (and ok (< i n))
      (unless (eq (aref string i) ?\s)
        (setq ok nil))
      (setq i (1+ i)))
    ok))

;;; Module-private id counter

(defvar emacs-tui-backend--handle-counter 0
  "Monotonic counter for handle ids (printable as `tui-N').")

;;; Capability list (Doc 43 §2.5)

(defconst emacs-tui-backend-base-capabilities
  '(text basic-color keyboard resize layout-box layout-grid)
  "Doc 43 §2.5 TUI MVP minimum capability set.
The TUI real backend always declares these; additional caps may be
added based on `emacs-tui-backend-color-mode' (256-color / truecolor)
or future v2.x features.")

(defun emacs-tui-backend--capabilities-for-mode (color-mode)
  "Return the full capability list given COLOR-MODE.
Always includes the MVP minimum (`emacs-tui-backend-base-capabilities')
plus the optional color-tier capability if COLOR-MODE elevates."
  (let ((caps (copy-sequence emacs-tui-backend-base-capabilities)))
    (cond
     ((eq color-mode 'truecolor)
      (cl-pushnew '256-color caps)
      (cl-pushnew 'truecolor caps))
     ((eq color-mode '256-color)
      (cl-pushnew '256-color caps)))
    caps))

;;; A. backend lifecycle

;;;###autoload
(defun emacs-tui-backend-init (&optional capabilities)
  "Initialize a fresh TUI backend and return its handle.
CAPABILITIES, if non-nil, is a list of capability symbols that
overrides the default derived from `emacs-tui-backend-color-mode'.

Returns an `emacs-tui-backend-handle' satisfying
`emacs-tui-backend-handlep'.  The handle is alive until shutdown."
  (let* ((counter (cl-incf emacs-tui-backend--handle-counter))
         (id (intern (format "tui-%d" counter)))
         (caps (or capabilities
                   (emacs-tui-backend--capabilities-for-mode
                    emacs-tui-backend-color-mode)))
         (handle (emacs-tui-backend--make-handle
                  :id id
                  :alive-p t
                  :capabilities (copy-sequence caps)
                  :frames nil
                  :next-frame-id 1
                  :event-queue nil
                  :resize-cb nil
                  :alt-screen-p nil)))
    (emacs-tui-backend--log "init handle=%S caps=%S" id caps)
    handle))

;;;###autoload
(defun emacs-tui-backend-shutdown (handle)
  "Tear down HANDLE, restore terminal state, drop frames + queue.
Per Doc 43 §3.1, an MVP shutdown emits the `restore' sequence
(= cursor-show + alt-screen-off if it was toggled + SGR reset).

After shutdown, calling any operation other than
`emacs-tui-backend-handlep' on HANDLE signals
`emacs-tui-backend-bad-handle'.  Returns t."
  (emacs-tui-backend--check-handle handle)
  ;; Restore terminal state before tearing down.
  (emacs-tui-backend--emit emacs-tui-backend--cursor-show)
  (emacs-tui-backend--emit emacs-tui-backend--reset)
  (when (emacs-tui-backend-handle-alt-screen-p handle)
    (emacs-tui-backend--emit emacs-tui-backend--alt-screen-off)
    (setf (emacs-tui-backend-handle-alt-screen-p handle) nil))
  (emacs-tui-backend--log "shutdown handle=%S frames=%d events=%d"
                          (emacs-tui-backend-handle-id handle)
                          (length (emacs-tui-backend-handle-frames handle))
                          (length (emacs-tui-backend-handle-event-queue handle)))
  (setf (emacs-tui-backend-handle-alive-p handle) nil
        (emacs-tui-backend-handle-frames handle) nil
        (emacs-tui-backend-handle-event-queue handle) nil
        (emacs-tui-backend-handle-resize-cb handle) nil)
  t)

;;;###autoload
(defun emacs-tui-backend-enter-alt-screen (handle)
  "Switch HANDLE's terminal into the alternate screen buffer.
Emits `\\e[?1049h' + clear-screen + cursor-home so the next paint
takes over the full TTY without leaving the user's shell scrollback
in the middle of the canvas.  Idempotent: a second call is a no-op
when alt-screen is already active.  Returns t when the toggle was
emitted, nil when it was already on."
  (emacs-tui-backend--check-handle handle)
  (cond
   ((emacs-tui-backend-handle-alt-screen-p handle) nil)
   (t
    (emacs-tui-backend--emit emacs-tui-backend--alt-screen-on)
    (emacs-tui-backend--emit emacs-tui-backend--clear-screen)
    (emacs-tui-backend--emit (emacs-tui-backend--cup 0 0))
    (setf (emacs-tui-backend-handle-alt-screen-p handle) t)
    (emacs-tui-backend--log "enter-alt-screen handle=%S"
                            (emacs-tui-backend-handle-id handle))
    t)))

;;;###autoload
(defun emacs-tui-backend-leave-alt-screen (handle)
  "Reverse `emacs-tui-backend-enter-alt-screen'.  Emits cursor-show +
SGR reset before flipping back to the normal screen so the shell
prompt re-appears without leftover attribute state.  Idempotent:
returns nil when alt-screen is already off."
  (emacs-tui-backend--check-handle handle)
  (cond
   ((not (emacs-tui-backend-handle-alt-screen-p handle)) nil)
   (t
    (emacs-tui-backend--emit emacs-tui-backend--cursor-show)
    (emacs-tui-backend--emit emacs-tui-backend--reset)
    (emacs-tui-backend--emit emacs-tui-backend--alt-screen-off)
    (setf (emacs-tui-backend-handle-alt-screen-p handle) nil)
    (emacs-tui-backend--log "leave-alt-screen handle=%S"
                            (emacs-tui-backend-handle-id handle))
    t)))

(defun emacs-tui-backend--check-handle (handle)
  "Signal `emacs-tui-backend-bad-handle' unless HANDLE is alive."
  (unless (emacs-tui-backend-handlep handle)
    (signal 'emacs-tui-backend-bad-handle (list handle)))
  (unless (emacs-tui-backend-handle-alive-p handle)
    (signal 'emacs-tui-backend-bad-handle
            (list 'shutdown (emacs-tui-backend-handle-id handle)))))

;;; B. capability query (Doc 43 §2.5 / §2.5a)

(defun emacs-tui-backend-capabilities (handle)
  "Return HANDLE's declared capability list (a fresh copy)."
  (emacs-tui-backend--check-handle handle)
  (copy-sequence (emacs-tui-backend-handle-capabilities handle)))

(defun emacs-tui-backend-get-capability (handle cap-name)
  "Return non-nil iff CAP-NAME is declared by HANDLE.
Equivalent of `display-spec-capability-p' for the TUI backend.
Always returns t / nil; never raises for unknown CAP-NAME (the
Doc 43 §2.5a `pre-check guard' contract)."
  (emacs-tui-backend--check-handle handle)
  (and (memq cap-name (emacs-tui-backend-handle-capabilities handle)) t))

(defun emacs-tui-backend--require-capability (handle cap-name api-name)
  "Signal `display-spec-unsupported' unless CAP-NAME is declared.
HANDLE = backend, API-NAME = symbol naming the caller for the
condition data plist (Doc 43 §2.5a invariant 2)."
  (unless (emacs-tui-backend-get-capability handle cap-name)
    (signal 'display-spec-unsupported
            (list :capability cap-name
                  :api api-name
                  :backend 'tui))))

;;; C. frame management (Doc 34 §2.11 swap-in)

;;;###autoload
(defun emacs-tui-backend-frame-create (handle name &optional params)
  "Register a fresh frame named NAME in HANDLE and return it.
NAME is a string label; PARAMS is an optional alist of frame
parameters (= subset only — Doc 43 §2.1 invariant `real backend
respects width / height / name from PARAMS, ignores GUI-only keys').

Returns an `emacs-tui-backend-frame' with id unique within HANDLE.
Default width / height come from `emacs-tui-backend-frame-default-*';
PARAMS may override with `:width' / `:height' (positive integers)."
  (emacs-tui-backend--check-handle handle)
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((width  (or (and (integerp (cdr (assq :width  params)))
                          (> (cdr (assq :width  params)) 0)
                          (cdr (assq :width  params)))
                     emacs-tui-backend-frame-default-width))
         (height (or (and (integerp (cdr (assq :height params)))
                          (> (cdr (assq :height params)) 0)
                          (cdr (assq :height params)))
                     emacs-tui-backend-frame-default-height))
         (fid (emacs-tui-backend-handle-next-frame-id handle))
         (frame (emacs-tui-backend--make-frame
                 :id fid
                 :name name
                 :width width
                 :height height
                 :params (copy-sequence params)
                 :canvas (emacs-tui-backend--make-canvas width height)
                 :dirty-rows (make-bool-vector height t)
                 :cursor-row nil
                 :cursor-col nil)))
    (setf (emacs-tui-backend-handle-next-frame-id handle) (1+ fid))
    (push (cons fid frame) (emacs-tui-backend-handle-frames handle))
    (emacs-tui-backend--log "frame-create handle=%S id=%d name=%S %dx%d"
                            (emacs-tui-backend-handle-id handle)
                            fid name width height)
    frame))

;;;###autoload
(defun emacs-tui-backend-frame-destroy (handle frame)
  "Remove FRAME from HANDLE's registry and clear its terminal area.
Per Doc 34 §2.11, the real backend issues a clear-screen on the last
remaining frame (= the application is going dark).  Returns t on
success; raises `emacs-tui-backend-bad-frame' if FRAME is not
registered with HANDLE."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (let ((fid (emacs-tui-backend-frame-id frame)))
    (setf (emacs-tui-backend-handle-frames handle)
          (assq-delete-all fid (emacs-tui-backend-handle-frames handle)))
    (emacs-tui-backend--log "frame-destroy handle=%S id=%d"
                            (emacs-tui-backend-handle-id handle) fid)
    (when (null (emacs-tui-backend-handle-frames handle))
      ;; Last frame — clear the screen for clean exit.
      (emacs-tui-backend--emit emacs-tui-backend--clear-screen)
      (emacs-tui-backend--emit (emacs-tui-backend--cup 0 0))))
  t)

;;;###autoload
(defun emacs-tui-backend-frame-resize (handle frame width height)
  "Resize FRAME inside HANDLE to WIDTH x HEIGHT.
The real backend reallocates the canvas, marks all rows dirty so the
next flush re-paints.  Returns the frame.  Signals
`emacs-tui-backend-bad-frame' if FRAME is not registered, and
`wrong-type-argument' if WIDTH / HEIGHT are not positive integers."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (emacs-tui-backend--require-capability handle 'resize 'frame-resize)
  (unless (and (integerp width) (> width 0))
    (signal 'wrong-type-argument (list 'positive-integer width)))
  (unless (and (integerp height) (> height 0))
    (signal 'wrong-type-argument (list 'positive-integer height)))
  (setf (emacs-tui-backend-frame-width frame) width
        (emacs-tui-backend-frame-height frame) height
        (emacs-tui-backend-frame-canvas frame)
        (emacs-tui-backend--make-canvas width height)
        (emacs-tui-backend-frame-dirty-rows frame)
        (make-bool-vector height t))
  (emacs-tui-backend--log "frame-resize handle=%S id=%d %dx%d"
                          (emacs-tui-backend-handle-id handle)
                          (emacs-tui-backend-frame-id frame) width height)
  frame)

(defun emacs-tui-backend--check-frame (handle frame)
  "Signal `emacs-tui-backend-bad-frame' unless FRAME is registered."
  (unless (emacs-tui-backend-framep frame)
    (signal 'emacs-tui-backend-bad-frame (list 'not-frame frame)))
  (unless (assq (emacs-tui-backend-frame-id frame)
                (emacs-tui-backend-handle-frames handle))
    (signal 'emacs-tui-backend-bad-frame
            (list 'unknown-frame
                  (emacs-tui-backend-frame-id frame)
                  (emacs-tui-backend-handle-id handle)))))

;;; D. canvas drawing

(defun emacs-tui-backend--make-canvas (width height)
  "Allocate a fresh HEIGHT x WIDTH canvas filled with space + nil face.
Each cell is a (CHAR . FACE) cons; the canvas is a vector of vectors."
  (let ((rows (make-vector height nil)))
    (dotimes (r height)
      (aset rows r (emacs-tui-backend--blank-row width)))
    rows))

(defun emacs-tui-backend--mark-row-dirty (frame row)
  "Mark ROW dirty on FRAME (= flush will repaint it)."
  (let ((bv (emacs-tui-backend-frame-dirty-rows frame)))
    (when (and (>= row 0) (< row (length bv)))
      (aset bv row t))))

(defun emacs-tui-backend--blank-row-p (row-vec)
  "Return non-nil when ROW-VEC contains only blank nil-face cells."
  (let ((i 0)
        (n (length row-vec))
        (blank t))
    (while (and blank (< i n))
      (let ((cell (aref row-vec i)))
        (unless (or (eq cell emacs-tui-backend--blank-cell)
                    (and (consp cell)
                         (eq (car cell) ?\s)
                         (null (cdr cell))))
          (setq blank nil)))
      (setq i (1+ i)))
    blank))

;;;###autoload
(defun emacs-tui-backend-canvas-clear (handle frame)
  "Clear FRAME's canvas to spaces and emit the clear-screen escape.
Marks all rows dirty so the next `flush' is a full repaint.  Returns
the frame."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (let ((width  (emacs-tui-backend-frame-width  frame))
        (height (emacs-tui-backend-frame-height frame)))
    (setf (emacs-tui-backend-frame-canvas frame)
          (emacs-tui-backend--make-canvas width height)
          (emacs-tui-backend-frame-dirty-rows frame)
          (make-bool-vector height t)))
  (emacs-tui-backend--emit emacs-tui-backend--clear-screen)
  (emacs-tui-backend--log "canvas-clear handle=%S id=%d"
                          (emacs-tui-backend-handle-id handle)
                          (emacs-tui-backend-frame-id frame))
  frame)

;;;###autoload
(defun emacs-tui-backend-canvas-draw-text (handle frame row col text &optional face)
  "Paint TEXT at (ROW, COL) on FRAME's canvas with optional FACE.
Per Doc 43 §2.5 the TUI backend declares `text' + `basic-color', so
TEXT is accepted and FACE may use any color symbol from the 8/16
basic palette (= `red', `bright-blue', etc.).  Out-of-bounds writes
are clipped silently to the row width.

Updates the canvas only — the actual ANSI emission happens in
`emacs-tui-backend-canvas-flush' to allow batching.  Returns the
number of cells actually written (0 if fully clipped)."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (emacs-tui-backend--require-capability handle 'text 'canvas-draw-text)
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (unless (and (integerp row) (>= row 0))
    (signal 'wrong-type-argument (list 'natnum row)))
  (unless (and (integerp col) (>= col 0))
    (signal 'wrong-type-argument (list 'natnum col)))
  (let* ((width  (emacs-tui-backend-frame-width  frame))
         (height (emacs-tui-backend-frame-height frame))
         (canvas (emacs-tui-backend-frame-canvas frame))
         (written 0))
    (when (and (< row height) (< col width))
      (let* ((n (length text))
             (limit (min n (- width col))))
        (cond
         ((and (null face)
               (= col 0)
               (>= n width)
               (emacs-tui-backend--blank-string-p text))
          (let ((old-row (aref canvas row)))
            (aset canvas row (emacs-tui-backend--blank-row width))
            (unless (eq old-row (emacs-tui-backend--blank-row width))
              (emacs-tui-backend--mark-row-dirty frame row)))
          (setq written width)
          nil)
         (t
          (let ((row-vec (aref canvas row)))
            (when (eq row-vec (emacs-tui-backend--blank-row width))
              (setq row-vec (copy-sequence row-vec))
              (aset canvas row row-vec))
            (dotimes (i limit)
              (aset row-vec (+ col i) (cons (aref text i) face)))
            (setq written limit)
            (when (> written 0)
              (emacs-tui-backend--mark-row-dirty frame row)))))))
    (emacs-tui-backend--log "canvas-draw-text handle=%S id=%d (%d,%d) %S face=%S written=%d"
                            (emacs-tui-backend-handle-id handle)
                            (emacs-tui-backend-frame-id frame)
                            row col text face written)
    written))

(defun emacs-tui-backend--paint-row (frame row)
  "Emit ANSI escapes for ROW of FRAME.
Walks the row from left to right, batching consecutive cells with
the same face into a single (CUP + SGR + chars) sequence.  Always
emits `RESET' at the end of the row to keep terminal state clean."
  (let* ((row-vec (aref (emacs-tui-backend-frame-canvas frame) row))
         (width (length row-vec))
         (col 0))
    (cond
     ((eq row-vec (emacs-tui-backend--blank-row width))
      (emacs-tui-backend--emit (emacs-tui-backend--cup row 0))
      (emacs-tui-backend--emit (make-string width ?\s)))
     ((emacs-tui-backend--blank-row-p row-vec)
      (emacs-tui-backend--emit (emacs-tui-backend--cup row 0))
      (emacs-tui-backend--emit (make-string width ?\s)))
     (t
      (while (< col width)
        (let* ((cell (aref row-vec col))
               (face (cdr cell))
               (start col)
               (chars (list (car cell))))
          (setq col (1+ col))
          (while (and (< col width)
                      (equal face (cdr (aref row-vec col))))
            (push (car (aref row-vec col)) chars)
            (setq col (1+ col)))
          (let ((sgr (emacs-tui-backend--sgr-from-face face)))
            (emacs-tui-backend--emit (emacs-tui-backend--cup row start))
            (when (> (length sgr) 0)
              (emacs-tui-backend--emit sgr))
            (emacs-tui-backend--emit (concat (nreverse chars)))
            (when (> (length sgr) 0)
              (emacs-tui-backend--emit emacs-tui-backend--reset)))))))))

;;;###autoload
(defun emacs-tui-backend-canvas-flush (handle frame)
  "Flush FRAME's pending canvas writes by emitting ANSI escapes.
For each dirty row, build a CUP + SGR + chars sequence and write it
through `emacs-tui-backend--emit'.  Clears the dirty bits.  Returns
the number of rows actually painted (0 if no dirty rows)."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (let* ((dirty (emacs-tui-backend-frame-dirty-rows frame))
         (height (length dirty))
         (painted 0))
    (dotimes (r height)
      (when (aref dirty r)
        (emacs-tui-backend--paint-row frame r)
        (aset dirty r nil)
        (setq painted (1+ painted))))
    ;; If a cursor position is set, restore it after the paint pass.
    (let ((cr (emacs-tui-backend-frame-cursor-row frame))
          (cc (emacs-tui-backend-frame-cursor-col frame)))
      (when (and cr cc)
        (emacs-tui-backend--emit (emacs-tui-backend--cup cr cc))))
    (emacs-tui-backend--log "canvas-flush handle=%S id=%d painted=%d"
                            (emacs-tui-backend-handle-id handle)
                            (emacs-tui-backend-frame-id frame) painted)
    painted))

;;; E. event polling (Doc 43 §2.6 pull-on-demand)

;;;###autoload
(defun emacs-tui-backend-event-poll (handle &optional timeout-ms)
  "Pop and return the next pending event from HANDLE, or nil on empty.
TIMEOUT-MS, if non-nil, is a non-negative integer giving a maximum
wait time in milliseconds.  The current implementation polls only
the in-process event queue (`emacs-tui-event.el' will wire stdin
SELECT in Phase 11.A) — when TIMEOUT-MS is supplied and the queue
is empty, we do a short `sleep-for' loop, polling at 5ms intervals
so test injectors running on a separate thread / timer can land an
event mid-wait without us busy-spinning.

Returns the event (any Lisp object) or nil."
  (emacs-tui-backend--check-handle handle)
  (let ((q (emacs-tui-backend-handle-event-queue handle)))
    (cond
     (q
      (let ((ev (car q)))
        (setf (emacs-tui-backend-handle-event-queue handle) (cdr q))
        (emacs-tui-backend--log "event-poll handle=%S ev=%S"
                                (emacs-tui-backend-handle-id handle) ev)
        ev))
     ((and timeout-ms (> timeout-ms 0))
      (let* ((deadline (+ (float-time) (/ timeout-ms 1000.0)))
             (interval 0.005)
             (event nil))
        (while (and (null event)
                    (< (float-time) deadline))
          (sleep-for interval)
          (setq q (emacs-tui-backend-handle-event-queue handle))
          (when q
            (setq event (car q))
            (setf (emacs-tui-backend-handle-event-queue handle) (cdr q))))
        (when event
          (emacs-tui-backend--log "event-poll(wait) handle=%S ev=%S"
                                  (emacs-tui-backend-handle-id handle) event))
        event))
     (t nil))))

(defun emacs-tui-backend-event-inject (handle event)
  "Append EVENT to HANDLE's event queue (test helper / event-source bridge).
This is the integration point for `emacs-tui-event.el' (Phase 2
sibling module) — the stdin parser turns ANSI / control-char
sequences into structured events and pushes them here.  Returns the
new queue length."
  (emacs-tui-backend--check-handle handle)
  (setf (emacs-tui-backend-handle-event-queue handle)
        (append (emacs-tui-backend-handle-event-queue handle)
                (list event)))
  (length (emacs-tui-backend-handle-event-queue handle)))

;;; F. cursor

;;;###autoload
(defun emacs-tui-backend-cursor-show (handle frame row col)
  "Show the cursor at (ROW, COL) on FRAME and update internal state.
Clipped to the frame bounds (out-of-range → clamped at edge).
Emits the show + CUP sequence immediately, plus stores the new
position so subsequent `canvas-flush' calls re-park there."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (unless (and (integerp row) (>= row 0))
    (signal 'wrong-type-argument (list 'natnum row)))
  (unless (and (integerp col) (>= col 0))
    (signal 'wrong-type-argument (list 'natnum col)))
  (let* ((width  (emacs-tui-backend-frame-width  frame))
         (height (emacs-tui-backend-frame-height frame))
         (r (min row (1- height)))
         (c (min col (1- width))))
    (setf (emacs-tui-backend-frame-cursor-row frame) r
          (emacs-tui-backend-frame-cursor-col frame) c)
    (emacs-tui-backend--emit emacs-tui-backend--cursor-show)
    (emacs-tui-backend--emit (emacs-tui-backend--cup r c))
    (emacs-tui-backend--log "cursor-show handle=%S id=%d (%d,%d)"
                            (emacs-tui-backend-handle-id handle)
                            (emacs-tui-backend-frame-id frame) r c)
    (cons r c)))

;;;###autoload
(defun emacs-tui-backend-cursor-show-if-changed (handle frame row col)
  "Show cursor at ROW/COL only when FRAME's stored cursor position differs."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (unless (and (integerp row) (>= row 0))
    (signal 'wrong-type-argument (list 'natnum row)))
  (unless (and (integerp col) (>= col 0))
    (signal 'wrong-type-argument (list 'natnum col)))
  (let* ((width  (emacs-tui-backend-frame-width  frame))
         (height (emacs-tui-backend-frame-height frame))
         (r (min row (1- height)))
         (c (min col (1- width))))
    (if (and (equal (emacs-tui-backend-frame-cursor-row frame) r)
             (equal (emacs-tui-backend-frame-cursor-col frame) c))
        (cons r c)
      (emacs-tui-backend-cursor-show handle frame r c))))

;;;###autoload
(defun emacs-tui-backend-cursor-hide (handle frame)
  "Hide the cursor on FRAME and clear its stored position."
  (emacs-tui-backend--check-handle handle)
  (emacs-tui-backend--check-frame handle frame)
  (setf (emacs-tui-backend-frame-cursor-row frame) nil
        (emacs-tui-backend-frame-cursor-col frame) nil)
  (emacs-tui-backend--emit emacs-tui-backend--cursor-hide)
  (emacs-tui-backend--log "cursor-hide handle=%S id=%d"
                          (emacs-tui-backend-handle-id handle)
                          (emacs-tui-backend-frame-id frame))
  t)

;;; G. resize listener

;;;###autoload
(defun emacs-tui-backend-resize-listen (handle callback)
  "Register CALLBACK to be invoked on terminal resize (SIGWINCH).
CALLBACK is a function of two integers (WIDTH HEIGHT), called by
`emacs-tui-event.el' when the SIGWINCH handler fires.  This API
only stores the callback; the actual SIGWINCH wiring is the sibling
module's job.  Use `emacs-tui-backend--dispatch-resize' (private)
from tests to simulate a resize event.

Returns the previous callback (or nil)."
  (emacs-tui-backend--check-handle handle)
  (unless (or (null callback) (functionp callback))
    (signal 'wrong-type-argument (list 'functionp callback)))
  (let ((prev (emacs-tui-backend-handle-resize-cb handle)))
    (setf (emacs-tui-backend-handle-resize-cb handle) callback)
    prev))

(defun emacs-tui-backend--dispatch-resize (handle width height)
  "Invoke HANDLE's resize callback with (WIDTH HEIGHT) if registered.
Internal helper for `emacs-tui-event.el' SIGWINCH plumbing and for
ERT.  Returns the callback's return value, or nil if no callback is
registered."
  (emacs-tui-backend--check-handle handle)
  (let ((cb (emacs-tui-backend-handle-resize-cb handle)))
    (when cb
      (funcall cb width height))))

(provide 'emacs-tui-backend)

;;; emacs-tui-backend.el ends here
