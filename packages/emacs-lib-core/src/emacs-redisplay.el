;;; emacs-redisplay.el --- Phase 3 redisplay engine MVP  -*- lexical-binding: t; -*-

;; Phase 3 module per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2 §3.3),
;; mirroring NeLisp Doc 43 v2 §3.2 Phase 11.B redisplay engine MVP.
;; Layer: nelisp-emacs (Layer 3 inner = redisplay-driver + glyph-matrix).
;; Namespace: `emacs-redisplay-' so loading inside a host Emacs does
;; NOT shadow any `redisplay-' / `glyph-' / `display-' symbol.
;;
;; Foundation contracts (LOCKED):
;;   - Doc 01 v2 §3.3 Phase 3 = Phase 11.B redisplay engine MVP scope
;;     = redisplay-driver + glyph-matrix のみ MVP (~600-1000 LOC).
;;   - Doc 43 v2 §2.2 redisplay engine architecture
;;     (window-tree → frame canvas dirty propagation, force-mode-line-
;;     update / redraw-display trigger handler).
;;   - Doc 43 v2 §2.3 glyph matrix structure (window-private 2D char
;;     grid + face mapping, hash field for diff redraw, dirty-set
;;     bitset).
;;   - Doc 43 v2 §3.2 Phase 11.B MVP non-goals (bidi, composition,
;;     mouse-face deferred to v2.x).
;;
;; Role in the architecture:
;;   - This module is the *driver* between buffer text (nelisp-ec /
;;     emacs-buffer) + window tree (emacs-window) + display backend
;;     (emacs-tui-backend canvas API).  The only frame canvas writer
;;     is the backend; we never emit raw ANSI here.
;;   - Per-window glyph matrices live as window parameters keyed by
;;     `emacs-redisplay-glyph-matrix' so that window-{point,start}
;;     changes can incrementally re-fill / re-hash without rebuilding
;;     the global frame canvas every frame.
;;   - Face / display / overlay queries are routed to NeLisp
;;     upstream APIs (= `nelisp-face-resolve', `nelisp-display-resolve',
;;     `nelisp-ovly-overlays-in') with a graceful fallback so that the
;;     module loads + ERTs pass even when the upstream module is not
;;     yet installed (= MVP isolation).
;;
;; API surface (~13 public APIs):
;;
;;   A. driver lifecycle (3 APIs)
;;      emacs-redisplay-init             — return a fresh redisplay handle
;;      emacs-redisplay-shutdown         — tear down a handle
;;      emacs-redisplay-handlep          — predicate
;;
;;   B. redisplay drivers (4 APIs)
;;      emacs-redisplay-redisplay        — full-frame redisplay pass
;;      emacs-redisplay-redisplay-window — single-window redisplay pass
;;      emacs-redisplay-flush-frame      — flush via backend after redisplay
;;      emacs-redisplay-set-cursor       — park cursor at window-point
;;
;;   C. dirty tracking (2 APIs)
;;      emacs-redisplay-mark-frame-dirty  — invalidate every window's matrix
;;      emacs-redisplay-mark-window-dirty — invalidate one window's matrix
;;
;;   D. glyph matrix query (4 APIs)
;;      emacs-redisplay-glyph-matrix     — return a window's current matrix
;;      emacs-redisplay-text-to-glyphs   — buffer text → vector of glyph
;;      emacs-redisplay-glyph-row        — row accessor
;;      emacs-redisplay-glyph-row-text   — concatenated row chars (testing)
;;
;; Non-goals (deferred per Doc 43 §3.2 Phase 11.B v2.x):
;;   - bidi (双方向 text); MVP is LTR only.
;;   - composition (CJK glyph composite).
;;   - proportional font / variable glyph width.
;;   - jit-lock + lazy redisplay optimization.
;;   - line wrap edge cases / continuation glyph (basic clip only).
;;   - mouse-face (mouse event itself = Phase 11.A v2.1+).
;;   - display-property `image' / `space' / `slice' (capability declared).
;;   - `face-realize' incremental cache (= Phase 11.B v2.x).
;;   - 5x throughput diff-redraw bench (= Phase 3 close gate, not MVP).

;;; Code:

(require 'cl-lib)

;; The following modules live in the same nelisp-emacs repo
;; (Phase 1 / Phase 2 dependencies).
(require 'emacs-buffer)
(require 'emacs-window)
(require 'emacs-tui-backend)

;;; Errors

(define-error 'emacs-redisplay-error
  "emacs-redisplay error")

(define-error 'emacs-redisplay-bad-handle
  "Not an emacs-redisplay handle"
  'emacs-redisplay-error)

(define-error 'emacs-redisplay-no-backend
  "Redisplay handle has no associated backend frame"
  'emacs-redisplay-error)

;;; Contract version constants

(defconst emacs-redisplay-driver-contract-version 1
  "REDISPLAY_DRIVER_CONTRACT_VERSION per Doc 43 v2 §2.2.
Bumped on incompatible change to the per-frame redisplay invariants
(e.g. dirty-tracking semantics, matrix cache invalidation rules).")

(defconst emacs-redisplay-glyph-matrix-contract-version 1
  "GLYPH_MATRIX_CONTRACT_VERSION per Doc 43 v2 §2.3.
Bumped on incompatible change to the glyph / glyph-row / glyph-matrix
struct shape exposed via `emacs-redisplay-glyph-matrix'.")

;;; defcustom

(defgroup emacs-redisplay nil
  "Phase 3 redisplay engine MVP."
  :group 'emacs-tui-backend)

(defcustom emacs-redisplay-truncate-lines t
  "If non-nil, lines longer than the window width are truncated.
MVP behaviour: no continuation glyph / line wrap.  Lines longer than
the window width are clipped, mirroring `truncate-lines = t' in Emacs."
  :type 'boolean
  :group 'emacs-redisplay)

(defcustom emacs-redisplay-log-enabled nil
  "If non-nil, append redisplay diagnostic lines to *Messages*."
  :type 'boolean
  :group 'emacs-redisplay)

(defcustom emacs-redisplay-default-tab-width 8
  "Tab width used when expanding TAB characters into spaces."
  :type 'integer
  :group 'emacs-redisplay)

;;; Glyph / glyph-row / glyph-matrix struct (Doc 43 §2.3)

(cl-defstruct (emacs-redisplay-glyph
               (:constructor emacs-redisplay--make-glyph)
               (:copier      nil))
  "A single glyph (= one displayed cell) per Doc 43 §2.3."
  (char         ?\s)        ;; codepoint (integer)
  (face         nil)        ;; face spec (= nelisp-face-resolve input)
  (face-id      0)          ;; realized face id (MVP = 0 default)
  (width        1)          ;; glyph width in cells (1 ASCII / 2 CJK)
  (composition  nil)        ;; nil or composition reference (deferred)
  (display-spec nil)        ;; nil or display property override
  (buf-pos      nil))       ;; source buffer position (for tooltips, etc.)

(cl-defstruct (emacs-redisplay-glyph-row
               (:constructor emacs-redisplay--make-glyph-row)
               (:copier      nil))
  "A row of glyphs in a glyph matrix per Doc 43 §2.3."
  (glyphs    nil)           ;; vector of emacs-redisplay-glyph
  (used      0)             ;; integer = active glyph count
  (hash      0)             ;; row hash for diff propagation
  (start-pos nil)           ;; buffer position at row start
  (end-pos   nil)           ;; buffer position at row end (exclusive)
  (continuation-p nil))     ;; non-nil if this row continues the previous

(cl-defstruct (emacs-redisplay-glyph-matrix
               (:constructor emacs-redisplay--make-glyph-matrix)
               (:copier      nil))
  "A 2D glyph matrix attached to a window per Doc 43 §2.3."
  (rows      nil)           ;; vector of emacs-redisplay-glyph-row
  (width     0)             ;; column count
  (height    0)             ;; row count
  (window    nil)           ;; owning emacs-window leaf
  (dirty-set nil)           ;; bool-vector of dirty row indices
  (cursor    nil))          ;; cons (ROW . COL) or nil

;;; Driver handle

(cl-defstruct (emacs-redisplay-handle
               (:constructor emacs-redisplay--make-handle)
               (:copier      nil)
               (:predicate   emacs-redisplay-handlep))
  "Opaque handle returned by `emacs-redisplay-init'."
  (id          nil :read-only t)   ;; gensym-style id
  (alive-p     t)                  ;; nil after shutdown
  (backend     nil)                ;; emacs-tui-backend handle (nil OK)
  (window-cache nil))              ;; alist (window-id . glyph-matrix)

;;; Module-private id counter

(defvar emacs-redisplay--handle-counter 0
  "Monotonic counter for redisplay handle ids (printable as `rd-N').")

;;; Logging helper

(defun emacs-redisplay--log (fmt &rest args)
  "When logging is enabled, append a formatted line to *Messages*."
  (when emacs-redisplay-log-enabled
    (apply #'message (concat "[emacs-redisplay] " fmt) args)))

;;; Optional NeLisp upstream API bridges (graceful fallback)
;;
;; Phase 3 MVP can route face / display / overlay queries to NeLisp
;; upstream modules (`nelisp-emacs-compat-face',
;; `nelisp-textprop-display', `nelisp-overlay').  When those are not
;; loaded — e.g. running this module in a vanilla host Emacs for ERT
;; without the full NeLisp dist — we fall back to an inert pass-through
;; so the engine still drives a coherent frame canvas (= MVP scope:
;; rendering the buffer text + face mapping when available).

(defun emacs-redisplay--resolve-face (spec)
  "Resolve face SPEC, deferring to `nelisp-face-resolve' when available.
When upstream resolution returns nil (= face not yet defined) we keep
the raw SPEC so the glyph face slot stays observable for diff /
backend SGR mapping.  Returns nil only when SPEC itself is nil."
  (cond
   ((null spec) nil)
   ((fboundp 'nelisp-face-resolve)
    (condition-case _err
        (or (nelisp-face-resolve spec) spec)
      (error spec)))
   (t spec)))

(defun emacs-redisplay--resolve-display (spec frame)
  "Resolve display-property SPEC for FRAME, deferring to upstream API.
Falls back to the raw SPEC if upstream returns nil so callers can
still inspect the unresolved value."
  (cond
   ((null spec) nil)
   ((fboundp 'nelisp-display-resolve)
    (condition-case _err
        (or (nelisp-display-resolve spec frame) spec)
      (error spec)))
   (t spec)))

(defun emacs-redisplay--overlays-in (beg end &optional buffer)
  "Return overlays touching [BEG, END) in BUFFER, or nil if API absent."
  (cond
   ((fboundp 'nelisp-ovly-overlays-in)
    (condition-case _err
        (let ((nelisp-ec--current-buffer (or buffer
                                             (and (boundp 'nelisp-ec--current-buffer)
                                                  nelisp-ec--current-buffer))))
          (nelisp-ovly-overlays-in beg end))
      (error nil)))
   (t nil)))

(defun emacs-redisplay--ovly-prop (overlay prop)
  "Return PROP of OVERLAY via `nelisp-ovly-get', or nil."
  (when (and overlay (fboundp 'nelisp-ovly-get))
    (condition-case _err
        (nelisp-ovly-get overlay prop)
      (error nil))))

(defun emacs-redisplay--ovly-bounds (overlay)
  "Return (START . END) of OVERLAY, or nil if accessors absent."
  (when (and overlay
             (fboundp 'nelisp-ovly-start)
             (fboundp 'nelisp-ovly-end))
    (condition-case _err
        (cons (nelisp-ovly-start overlay) (nelisp-ovly-end overlay))
      (error nil))))

;;; Buffer text + text-property access (works against emacs-buffer or
;;; nelisp-ec depending on which is wired in the host).

(defun emacs-redisplay--buffer-string (buffer)
  "Return the full text of BUFFER as a string (current narrowing OK).
Works whether BUFFER is a `nelisp-ec-buffer' (Phase 1) or has been
made-current via the emacs-buffer compatibility layer.  Returns the
empty string when no text is reachable (= safe MVP default)."
  (cond
   ((null buffer) "")
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer))
    (condition-case _err
        (let ((nelisp-ec--current-buffer buffer))
          (nelisp-ec-buffer-string))
      (error "")))
   ((stringp buffer) buffer)  ;; test convenience
   (t "")))

(defun emacs-redisplay--buffer-substring (buffer start end)
  "Return BUFFER text in 1-based [START, END), with safe fallbacks."
  (cond
   ((null buffer) "")
   ((stringp buffer)
    (let* ((s0 (max 0 (min (length buffer) (1- start))))
           (e0 (max s0 (min (length buffer) (1- end)))))
      (substring buffer s0 e0)))
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer))
    (condition-case _err
        (let ((nelisp-ec--current-buffer buffer))
          (nelisp-ec-buffer-substring start end))
      (error "")))
   (t "")))

(defun emacs-redisplay--text-property-at (pos prop buffer)
  "Return the value of PROP at POS in BUFFER, or nil if no value.
Routes to `emacs-buffer-get-text-property' when available; otherwise
returns nil (= MVP face = default, display = no override)."
  (cond
   ((or (null pos) (null buffer)) nil)
   ((fboundp 'emacs-buffer-get-text-property)
    (condition-case _err
        (emacs-buffer-get-text-property pos prop buffer)
      (error nil)))
   (t nil)))

;;; Hash helper for diff propagation

(defun emacs-redisplay--row-hash (row-vec)
  "Return a stable hash for ROW-VEC (vector of glyphs)."
  (let ((h 0)
        (i 0)
        (len (length row-vec)))
    (while (< i len)
      (let* ((g (aref row-vec i))
             (c (if g (emacs-redisplay-glyph-char g) 0))
             (f (if g (emacs-redisplay-glyph-face g) nil)))
        ;; Mix char + sxhash of face into a 32-bit mask.
        (setq h (logand #xFFFFFFFF
                        (+ (* h 31)
                           (logxor c (sxhash-equal f))))))
      (setq i (1+ i)))
    h))

;;; Glyph matrix construction

(defun emacs-redisplay--make-empty-row (width)
  "Allocate an empty row of WIDTH spaces (face nil)."
  (let ((vec (make-vector width nil)))
    (dotimes (i width)
      (aset vec i (emacs-redisplay--make-glyph
                   :char ?\s :face nil :width 1)))
    (emacs-redisplay--make-glyph-row
     :glyphs vec :used 0 :hash 0
     :start-pos nil :end-pos nil
     :continuation-p nil)))

(defun emacs-redisplay--make-empty-matrix (window width height)
  "Build a fresh glyph-matrix sized WIDTH x HEIGHT for WINDOW."
  (let ((rows (make-vector height nil)))
    (dotimes (r height)
      (aset rows r (emacs-redisplay--make-empty-row width)))
    (emacs-redisplay--make-glyph-matrix
     :rows rows :width width :height height
     :window window
     :dirty-set (make-bool-vector height t)
     :cursor nil)))

;;; A. driver lifecycle

;;;###autoload
(defun emacs-redisplay-init (&optional args)
  "Initialize a fresh redisplay driver and return its handle.
ARGS is an optional plist:
  :backend BACKEND  — backend handle returned by
                      `emacs-tui-backend-init'.  May be left nil for
                      logical redisplay (= matrix building only)."
  (let* ((counter (cl-incf emacs-redisplay--handle-counter))
         (id (intern (format "rd-%d" counter)))
         (backend (plist-get args :backend))
         (handle (emacs-redisplay--make-handle
                  :id id
                  :alive-p t
                  :backend backend
                  :window-cache nil)))
    (emacs-redisplay--log "init handle=%S backend=%S" id backend)
    handle))

;;;###autoload
(defun emacs-redisplay-shutdown (handle)
  "Tear down HANDLE, dropping every cached glyph matrix.  Returns t."
  (emacs-redisplay--check-handle handle)
  (emacs-redisplay--log "shutdown handle=%S cache=%d"
                        (emacs-redisplay-handle-id handle)
                        (length (emacs-redisplay-handle-window-cache handle)))
  (setf (emacs-redisplay-handle-alive-p handle) nil
        (emacs-redisplay-handle-window-cache handle) nil
        (emacs-redisplay-handle-backend handle) nil)
  t)

(defun emacs-redisplay--check-handle (handle)
  "Signal `emacs-redisplay-bad-handle' unless HANDLE is alive."
  (unless (emacs-redisplay-handlep handle)
    (signal 'emacs-redisplay-bad-handle (list handle)))
  (unless (emacs-redisplay-handle-alive-p handle)
    (signal 'emacs-redisplay-bad-handle
            (list 'shutdown (emacs-redisplay-handle-id handle)))))

;;; Window-cache helpers

(defun emacs-redisplay--cache-key (window)
  "Return the cache key (= window id) for WINDOW."
  (and window (emacs-window-id window)))

(defun emacs-redisplay--get-matrix (handle window)
  "Return the cached glyph-matrix for WINDOW, or nil."
  (cdr (assq (emacs-redisplay--cache-key window)
             (emacs-redisplay-handle-window-cache handle))))

(defun emacs-redisplay--put-matrix (handle window matrix)
  "Store MATRIX as the cache entry for WINDOW under HANDLE."
  (let* ((key (emacs-redisplay--cache-key window))
         (cache (emacs-redisplay-handle-window-cache handle))
         (cell (assq key cache)))
    (if cell
        (setcdr cell matrix)
      (setf (emacs-redisplay-handle-window-cache handle)
            (cons (cons key matrix) cache))))
  matrix)

(defun emacs-redisplay--ensure-matrix (handle window)
  "Return WINDOW's glyph-matrix, allocating + caching it if missing.
Reallocates when window dimensions changed under the cached entry."
  (let* ((width  (emacs-window-window-width  window))
         (height (emacs-window-window-height window))
         (cur (emacs-redisplay--get-matrix handle window)))
    (cond
     ((and cur
           (= width  (emacs-redisplay-glyph-matrix-width  cur))
           (= height (emacs-redisplay-glyph-matrix-height cur)))
      cur)
     (t
      (let ((m (emacs-redisplay--make-empty-matrix window width height)))
        (emacs-redisplay--put-matrix handle window m))))))

;;; D. glyph matrix query (public)

(defun emacs-redisplay-glyph-matrix (handle window)
  "Return WINDOW's current glyph-matrix under HANDLE.
Returns nil when no redisplay pass has been run yet."
  (emacs-redisplay--check-handle handle)
  (emacs-redisplay--get-matrix handle window))

(defun emacs-redisplay-glyph-row (matrix row)
  "Return ROW (0-based) of MATRIX, or nil if out of range."
  (when (and matrix
             (integerp row)
             (>= row 0)
             (< row (emacs-redisplay-glyph-matrix-height matrix)))
    (aref (emacs-redisplay-glyph-matrix-rows matrix) row)))

(defun emacs-redisplay-glyph-row-text (row)
  "Return ROW's painted text as a string (used cells only).
Convenience helper for ERT — concatenates the `char' field of every
glyph in [0, used) and returns the result.  Spaces produced by
TAB-expansion or unused trailing cells are NOT included."
  (when row
    (let* ((used (emacs-redisplay-glyph-row-used row))
           (vec (emacs-redisplay-glyph-row-glyphs row))
           (out (make-string used ?\s)))
      (dotimes (i used)
        (let ((g (aref vec i)))
          (when g
            (aset out i (emacs-redisplay-glyph-char g)))))
      out)))

(defun emacs-redisplay-text-to-glyphs (handle buffer &optional start end)
  "Convert BUFFER text in [START, END) into a vector of glyphs.
If START / END are nil, the entire buffer (or string, when BUFFER is
literal) is converted.  Each glyph carries its source buffer position
in the `buf-pos' slot.  HANDLE may be nil — only used for logging."
  (when handle
    (emacs-redisplay--check-handle handle))
  (let* ((text (cond
                ((stringp buffer)
                 (cond
                  ((and start end)
                   (substring buffer (max 0 (1- start))
                              (min (length buffer) (1- end))))
                  (t buffer)))
                (t
                 (let ((s (or start 1))
                       (e (or end (1+ (length
                                       (emacs-redisplay--buffer-string
                                        buffer))))))
                   (emacs-redisplay--buffer-substring buffer s e)))))
         (offset (or start 1))
         (n (length text))
         (vec (make-vector n nil)))
    (dotimes (i n)
      (let* ((pos (+ offset i))
             (face (emacs-redisplay--text-property-at pos 'face buffer))
             (display (emacs-redisplay--text-property-at pos 'display buffer)))
        (aset vec i
              (emacs-redisplay--make-glyph
               :char (aref text i)
               :face (emacs-redisplay--resolve-face face)
               :face-id 0
               :width 1
               :composition nil
               :display-spec (emacs-redisplay--resolve-display display nil)
               :buf-pos pos))))
    vec))

;;; Line layout (= xdisp.c try_window_id MVP equivalent)

(defun emacs-redisplay--char-width (ch)
  "Return the visual width of CH (1 normally, 2 for CJK, special for TAB).
TAB returns -1 sentinel meaning the caller must expand to next tab stop;
control characters return 1."
  (cond
   ((eq ch ?\t) -1)
   ((eq ch ?\n) 0)
   ;; Rough CJK coverage — same heuristic as char-width when called on
   ;; a real buffer.  When `char-width' is bound, defer to it.
   ((and (fboundp 'char-width)
         (integerp ch))
    (condition-case _err (char-width ch) (error 1)))
   (t 1)))

(defun emacs-redisplay--apply-overlay-face (glyph overlays pos)
  "Merge overlay face attributes (highest priority wins) into GLYPH."
  (when overlays
    (let (best best-prio)
      (dolist (ov overlays)
        (let ((bounds (emacs-redisplay--ovly-bounds ov))
              (prio   (or (emacs-redisplay--ovly-prop ov 'priority) 0))
              (face   (emacs-redisplay--ovly-prop ov 'face)))
          (when (and bounds face
                     (<= (car bounds) pos)
                     (< pos (cdr bounds))
                     (or (null best) (> prio best-prio)))
            (setq best face
                  best-prio prio))))
      (when best
        (let* ((existing (emacs-redisplay-glyph-face glyph))
               (resolved (emacs-redisplay--resolve-face best))
               ;; When upstream resolve returns nil (= face not yet
               ;; defined or no match for current backend) fall back to
               ;; the raw spec so the merge stays observable.
               (eff (or resolved best)))
          (setf (emacs-redisplay-glyph-face glyph)
                (if existing
                    (cond
                     ((listp existing) (cons eff existing))
                     (t (list eff existing)))
                  eff)))))))

(defun emacs-redisplay--lay-out-line (line buffer-pos buffer overlays width)
  "Return a vector of `used' glyphs for LINE starting at BUFFER-POS.
LINE is a string (single logical line, no embedded newline).  WIDTH is
the maximum number of cells to occupy.  TABs are expanded.  When the
line exceeds WIDTH and `emacs-redisplay-truncate-lines' is non-nil,
the line is clipped (= MVP behaviour, no continuation glyph).  Returns
a cons (USED-VEC . NEXT-POS) where NEXT-POS is the buffer position
just past the consumed text (excluding any newline)."
  (let* ((tab-width (max 1 emacs-redisplay-default-tab-width))
         (pos buffer-pos)
         (col 0)
         (used (make-vector width nil))
         (i 0)
         (n (length line))
         (overflow nil))
    (while (and (< i n) (not overflow))
      (let* ((ch (aref line i))
             (cw (emacs-redisplay--char-width ch)))
        (cond
         ;; TAB → expand to next tab stop within window width.
         ((eq cw -1)
          (let* ((target (* (1+ (/ col tab-width)) tab-width))
                 (k (max 1 (- target col))))
            (dotimes (_ k)
              (when (< col width)
                (let ((g (emacs-redisplay--make-glyph
                          :char ?\s :face nil :face-id 0
                          :width 1 :buf-pos pos)))
                  (when overlays
                    (emacs-redisplay--apply-overlay-face g overlays pos))
                  (aset used col g)
                  (setq col (1+ col)))))
            (setq pos (1+ pos))))
         ;; Normal character (incl. CJK width 2).
         (t
          (let* ((face (emacs-redisplay--text-property-at pos 'face buffer))
                 (display (emacs-redisplay--text-property-at pos 'display buffer))
                 (g (emacs-redisplay--make-glyph
                     :char ch
                     :face (emacs-redisplay--resolve-face face)
                     :face-id 0
                     :width (max 1 cw)
                     :composition nil
                     :display-spec (emacs-redisplay--resolve-display
                                    display nil)
                     :buf-pos pos)))
            (when overlays
              (emacs-redisplay--apply-overlay-face g overlays pos))
            (cond
             ;; Overflow → stop (truncate).
             ((>= (+ col (max 1 cw)) (1+ width))
              (if emacs-redisplay-truncate-lines
                  (setq overflow t)
                ;; wrap path = post-MVP, treat as truncate too for now.
                (setq overflow t)))
             (t
              (aset used col g)
              (setq col (+ col (max 1 cw))
                    pos (1+ pos)))))))
        (setq i (1+ i))))
    (cons (let ((trimmed (make-vector col nil)))
            (dotimes (k col) (aset trimmed k (aref used k)))
            trimmed)
          pos)))

(defun emacs-redisplay--fill-row (row glyph-vec width buffer-pos end-pos)
  "Place GLYPH-VEC into ROW, padding to WIDTH with empty glyphs.
Updates ROW's used / hash / start-pos / end-pos accordingly."
  (let* ((vec (emacs-redisplay-glyph-row-glyphs row))
         (n (length glyph-vec)))
    (dotimes (i width)
      (aset vec i
            (if (< i n)
                (aref glyph-vec i)
              (emacs-redisplay--make-glyph
               :char ?\s :face nil :face-id 0 :width 1))))
    (setf (emacs-redisplay-glyph-row-used row) n
          (emacs-redisplay-glyph-row-start-pos row) buffer-pos
          (emacs-redisplay-glyph-row-end-pos row) end-pos
          (emacs-redisplay-glyph-row-hash row)
          (emacs-redisplay--row-hash vec))))

(defun emacs-redisplay--clear-row (row)
  "Reset ROW to empty (all spaces, used = 0)."
  (let ((vec (emacs-redisplay-glyph-row-glyphs row)))
    (dotimes (i (length vec))
      (aset vec i (emacs-redisplay--make-glyph
                   :char ?\s :face nil :face-id 0 :width 1)))
    (setf (emacs-redisplay-glyph-row-used row) 0
          (emacs-redisplay-glyph-row-start-pos row) nil
          (emacs-redisplay-glyph-row-end-pos row) nil
          (emacs-redisplay-glyph-row-hash row) 0)))

(defun emacs-redisplay--split-into-lines (text)
  "Split TEXT into a list of (LINE . NEWLINE-CONSUMED-COUNT) cons cells.
TEXT is consumed sequentially: each non-newline segment becomes one
LINE.  NEWLINE-CONSUMED-COUNT is 1 if a newline immediately followed
the segment, 0 if at end-of-text."
  (let ((result nil)
        (start 0)
        (n (length text)))
    (while (< start n)
      (let ((nl (cl-position ?\n text :start start)))
        (cond
         (nl
          (push (cons (substring text start nl) 1) result)
          (setq start (1+ nl)))
         (t
          (push (cons (substring text start) 0) result)
          (setq start n)))))
    (when (and (> n 0)
               (= (aref text (1- n)) ?\n))
      ;; Trailing newline → one more empty row.
      (push (cons "" 0) result))
    (when (zerop n)
      (push (cons "" 0) result))
    (nreverse result)))

;;; B. redisplay drivers

;;;###autoload
(defun emacs-redisplay-redisplay-window (handle window)
  "Run a redisplay pass on WINDOW under HANDLE.
Returns the (possibly newly built) glyph-matrix.  Does NOT flush to
backend — call `emacs-redisplay-flush-frame' for the actual emit."
  (emacs-redisplay--check-handle handle)
  (unless (emacs-window-p window)
    (signal 'wrong-type-argument (list 'emacs-window-p window)))
  (let* ((width  (emacs-window-window-width  window))
         (height (emacs-window-window-height window))
         (matrix (emacs-redisplay--ensure-matrix handle window))
         (buffer (emacs-window-buffer window))
         (start  (or (emacs-window-start window) 1))
         (text   (cond
                  ((null buffer) "")
                  ((stringp buffer) buffer)
                  (t (emacs-redisplay--buffer-string buffer))))
         ;; Compute the substring beginning at window-start (1-based).
         (visible
          (cond
           ((stringp text)
            (substring text (min (max 0 (1- start)) (length text))))
           (t "")))
         (lines (emacs-redisplay--split-into-lines visible))
         ;; Overlay scan for the visible region (when buffer-backed).
         (visible-end (+ start (length visible)))
         (overlays (and (not (stringp buffer))
                        (emacs-redisplay--overlays-in
                         start visible-end buffer)))
         (pos start)
         (row-idx 0)
         (rows (emacs-redisplay-glyph-matrix-rows matrix))
         (dirty (emacs-redisplay-glyph-matrix-dirty-set matrix)))
    ;; Reset every cached row before re-fill (MVP = full per-window
    ;; redraw; diff happens at the backend canvas level via row hash).
    (dotimes (r height)
      (emacs-redisplay--clear-row (aref rows r)))
    ;; Walk the lines, laying each one into a row.
    (while (and (< row-idx height) lines)
      (let* ((entry (pop lines))
             (line  (car entry))
             (nl-consumed (cdr entry))
             (laid (emacs-redisplay--lay-out-line
                    line pos buffer overlays width))
             (gvec (car laid))
             (next-pos (+ (cdr laid) nl-consumed)))
        (emacs-redisplay--fill-row (aref rows row-idx) gvec width
                                   pos next-pos)
        (setq pos next-pos
              row-idx (1+ row-idx))))
    ;; Mark every row dirty so backend flush repaints exactly once.
    (dotimes (r height)
      (aset dirty r t))
    ;; Compute cursor (window-point relative to window-start).
    (let* ((point (or (emacs-window-point window) start))
           (cursor (emacs-redisplay--cursor-for-point matrix point)))
      (setf (emacs-redisplay-glyph-matrix-cursor matrix) cursor))
    (emacs-redisplay--log "redisplay-window handle=%S w=%S %dx%d rows-painted=%d"
                          (emacs-redisplay-handle-id handle)
                          (emacs-redisplay--cache-key window)
                          width height row-idx)
    matrix))

(defun emacs-redisplay--cursor-for-point (matrix point)
  "Return (ROW . COL) in MATRIX corresponding to buffer POINT, or nil."
  (let* ((rows (emacs-redisplay-glyph-matrix-rows matrix))
         (h (emacs-redisplay-glyph-matrix-height matrix))
         (found nil))
    (catch 'done
      (dotimes (r h)
        (let* ((row (aref rows r))
               (s (emacs-redisplay-glyph-row-start-pos row))
               (e (emacs-redisplay-glyph-row-end-pos row)))
          (when (and s e (<= s point) (<= point e))
            (let* ((vec (emacs-redisplay-glyph-row-glyphs row))
                   (used (emacs-redisplay-glyph-row-used row))
                   (col 0))
              (catch 'col-done
                (dotimes (i used)
                  (let* ((g (aref vec i))
                         (bp (and g (emacs-redisplay-glyph-buf-pos g))))
                    (when (and bp (>= bp point))
                      (setq col i)
                      (throw 'col-done nil))
                    (setq col (1+ i)))))
              (setq found (cons r col)))
            (throw 'done nil)))))
    found))

;;;###autoload
(defun emacs-redisplay-redisplay (handle &optional _frame)
  "Run a redisplay pass over every live window.
FRAME is accepted for API compatibility (Phase 1 has a single implicit
frame) and currently ignored.  Returns the count of windows redisplayed."
  (emacs-redisplay--check-handle handle)
  (let ((count 0))
    (dolist (w (emacs-window-window-list))
      (when (and (emacs-window-p w) (emacs-window-leaf-p w))
        (emacs-redisplay-redisplay-window handle w)
        (setq count (1+ count))))
    (emacs-redisplay--log "redisplay handle=%S windows=%d"
                          (emacs-redisplay-handle-id handle) count)
    count))

;;; C. dirty tracking

;;;###autoload
(defun emacs-redisplay-mark-window-dirty (handle window)
  "Invalidate WINDOW's cached glyph-matrix under HANDLE.
The next call to `emacs-redisplay-redisplay-window' will rebuild from
scratch.  Returns t if a cached entry was dropped, nil otherwise."
  (emacs-redisplay--check-handle handle)
  (let* ((key (emacs-redisplay--cache-key window))
         (cache (emacs-redisplay-handle-window-cache handle))
         (cell (assq key cache)))
    (cond
     (cell
      (setf (emacs-redisplay-handle-window-cache handle)
            (assq-delete-all key cache))
      (emacs-redisplay--log "mark-window-dirty handle=%S w=%S"
                            (emacs-redisplay-handle-id handle) key)
      t)
     (t nil))))

;;;###autoload
(defun emacs-redisplay-mark-frame-dirty (handle &optional _frame)
  "Drop every cached glyph-matrix on HANDLE (frame-wide invalidation).
Returns the number of cache entries cleared."
  (emacs-redisplay--check-handle handle)
  (let ((n (length (emacs-redisplay-handle-window-cache handle))))
    (setf (emacs-redisplay-handle-window-cache handle) nil)
    (emacs-redisplay--log "mark-frame-dirty handle=%S cleared=%d"
                          (emacs-redisplay-handle-id handle) n)
    n))

;;; B (cont.).  flush + cursor

(defun emacs-redisplay--row-text-segments (row width)
  "Return a list of (COL TEXT FACE) painting segments for ROW.
Adjacent glyphs sharing the same face are batched into a single
segment so the backend `canvas-draw-text' call count stays low."
  (let* ((vec (emacs-redisplay-glyph-row-glyphs row))
         (n (min width (length vec)))
         (segments nil)
         (col 0))
    (while (< col n)
      (let* ((g (aref vec col))
             (face (and g (emacs-redisplay-glyph-face g)))
             (start col)
             (chars (list (if g (emacs-redisplay-glyph-char g) ?\s))))
        (setq col (1+ col))
        (while (and (< col n)
                    (let ((g2 (aref vec col)))
                      (equal face (and g2
                                       (emacs-redisplay-glyph-face g2)))))
          (push (let ((g2 (aref vec col)))
                  (if g2 (emacs-redisplay-glyph-char g2) ?\s))
                chars)
          (setq col (1+ col)))
        (push (list start (concat (nreverse chars)) face) segments)))
    (nreverse segments)))

;;;###autoload
(defun emacs-redisplay-flush-frame (handle frame)
  "Push every dirty cached row onto FRAME via the bound backend.
HANDLE must have been initialised with a backend; otherwise the call
is a no-op returning 0.  Returns the total segment count emitted."
  (emacs-redisplay--check-handle handle)
  (let ((backend (emacs-redisplay-handle-backend handle))
        (emitted 0))
    (cond
     ((null backend) 0)
     (t
      (let* ((edges-cache (make-hash-table :test 'eq))
             (windows
              (cl-remove-if-not
               (lambda (w) (and (emacs-window-p w)
                                (emacs-window-leaf-p w)))
               (emacs-window-window-list))))
        (dolist (w windows)
          (let ((m (emacs-redisplay--get-matrix handle w)))
            (when m
              (let* ((edges (or (gethash w edges-cache)
                                (puthash w (emacs-window-window-edges w)
                                         edges-cache)))
                     (left (nth 0 edges))
                     (top  (nth 1 edges))
                     (h    (emacs-redisplay-glyph-matrix-height m))
                     (width (emacs-redisplay-glyph-matrix-width  m))
                     (rows  (emacs-redisplay-glyph-matrix-rows   m))
                     (dirty (emacs-redisplay-glyph-matrix-dirty-set m)))
                (dotimes (r h)
                  (when (aref dirty r)
                    (let ((row (aref rows r)))
                      ;; Paint a clearing space band first to ensure
                      ;; trailing area is wiped (= MVP full repaint).
                      (emacs-tui-backend-canvas-draw-text
                       backend frame (+ top r) left
                       (make-string width ?\s) nil)
                      (dolist (seg (emacs-redisplay--row-text-segments
                                    row width))
                        (let ((c (nth 0 seg))
                              (txt (nth 1 seg))
                              (face (nth 2 seg)))
                          (emacs-tui-backend-canvas-draw-text
                           backend frame (+ top r) (+ left c) txt face)
                          (setq emitted (1+ emitted))))
                      (aset dirty r nil))))))))
        ;; Drive the backend's own batching pass.
        (emacs-tui-backend-canvas-flush backend frame))
      emitted))))

;;;###autoload
(defun emacs-redisplay-set-cursor (handle frame &optional window)
  "Park the backend cursor at WINDOW's window-point.
WINDOW defaults to the selected window.  Resolves the (ROW . COL) via
the cached glyph-matrix; falls back to the window's edges + (0, 0) if
no matrix has been built yet.  Returns the backend cursor cell, or
nil if no backend is bound."
  (emacs-redisplay--check-handle handle)
  (let ((backend (emacs-redisplay-handle-backend handle)))
    (when backend
      (let* ((w (or window (emacs-window-selected-window)))
             (edges (emacs-window-window-edges w))
             (left (nth 0 edges))
             (top  (nth 1 edges))
             (m (emacs-redisplay--get-matrix handle w))
             (cursor (and m (emacs-redisplay-glyph-matrix-cursor m)))
             (r (+ top (or (and cursor (car cursor)) 0)))
             (c (+ left (or (and cursor (cdr cursor)) 0))))
        (emacs-tui-backend-cursor-show backend frame r c)))))

(provide 'emacs-redisplay)

;;; emacs-redisplay.el ends here
