;;; emacs-redisplay.el --- Phase 3 redisplay engine MVP + face-realize + overlay-strings + 256/truecolor  -*- lexical-binding: t; -*-

;; Phase 3 module per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2 §3.3),
;; mirroring NeLisp Doc 43 v2 §3.2 Phase 11.B redisplay engine MVP.
;; Phase 3.B.1 adds face-realize MVP per Doc 43 v2 §2.4 (face / display
;; attribute system) — = the smallest shippable Phase 3.B sub-step:
;; face spec → backend-ready normalized SGR attribute alist
;; (foreground / background / weight / slant / underline /
;; inverse-video).  Inheritance + cascade are routed to upstream
;; `nelisp-face-resolve' when available; otherwise we use a local
;; registry + flattening logic so ERTs run in vanilla host Emacs.
;; Phase 3.B.2 (this file) adds overlay before-string / after-string
;; emission inside the glyph row build path: when an overlay covers a
;; buffer position, its `:before-string' is emitted as glyphs *before*
;; the buffer char at the overlay start, and its `:after-string' is
;; emitted as glyphs *after* the buffer char at the overlay end (-1 of
;; exclusive end).  Multiple overlays at the same position are emitted
;; in priority order (lower priority first → higher priority closest
;; to the buffer text), matching the Emacs convention.
;; Phase 3.B.3 (this file) extends the color value vocabulary recognized
;; by the realize layer + the SGR emit layer to include 256-color and
;; truecolor (24-bit) descriptors:
;;   - "#rrggbb" hex strings                → truecolor
;;   - (:r N :g N :b N) plist values        → truecolor
;;   - (palette N) / (palette . N) lists    → 256-color (N = 0..255)
;;   - :palette-N keyword                   → 256-color
;;   - existing 16-color symbols / names    → unchanged (regression-safe)
;; The new descriptors propagate as normalized cons / list values inside
;; the realized SGR alist (= `(palette N)' / `(rgb R G B)') and are
;; emitted by `emacs-tui-backend--sgr-from-face' as `\\e[38;5;Nm' /
;; `\\e[48;5;Nm' (256-color) or `\\e[38;2;R;G;Bm' / `\\e[48;2;R;G;Bm'
;; (truecolor) escape sequences per ECMA-48 / xterm conventions.
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
;; API surface (~17 public APIs):
;;
;;   A. driver lifecycle (3 APIs)
;;      emacs-redisplay-init             — return a fresh redisplay handle
;;      emacs-redisplay-shutdown         — tear down a handle
;;      emacs-redisplay-handlep          — predicate
;;
;;   B. redisplay drivers (6 APIs)
;;      emacs-redisplay-redisplay        — full-frame redisplay pass
;;      emacs-redisplay-redisplay-window — single-window redisplay pass
;;      emacs-redisplay-redraw-display   — full-frame dirty + redisplay
;;      emacs-redisplay-force-mode-line-update — mode-line dirty trigger
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
;;   E. face-realize MVP — Phase 3.B.1 / Doc 43 §2.4 (5 APIs)
;;      emacs-redisplay-realize-face       — face spec → SGR-ready alist
;;      emacs-redisplay-defface            — register a face spec locally
;;      emacs-redisplay-face-attributes    — registry lookup
;;      emacs-redisplay-face-cache-clear   — drop cached realizations
;;      emacs-redisplay--parse-color-spec  — Phase 3.B.3 color descriptor
;;                                            parser (16/256/truecolor)
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
(require 'emacs-cc-xdisp-1)
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

(defconst emacs-redisplay-glyph-matrix-contract-version 2
  "GLYPH_MATRIX_CONTRACT_VERSION per Doc 43 v2 §2.3.
Bumped on incompatible change to the glyph / glyph-row / glyph-matrix
struct shape exposed via `emacs-redisplay-glyph-matrix'.

History:
  v1 — Phase 3 MVP (T160): glyph slots = char/face/face-id/width/
       composition/display-spec/buf-pos.
  v2 — Phase 3.B.1: glyph slot `realized-face' added (= SGR-ready
       attribute alist computed via `emacs-redisplay-realize-face').
       The original `face' slot continues to hold the raw spec for
       diff / observability / overlay merge intermediate state.")

(defconst emacs-redisplay-face-realize-contract-version 2
  "FACE_REALIZE_CONTRACT_VERSION per Doc 43 v2 §2.4.
Bumped on incompatible change to `emacs-redisplay-realize-face'
output shape (= SGR attribute alist canonical form).

History:
  v1 — Phase 3.B.1: alist values are 16-color palette symbols
       (`red', `bright-blue', `default', ...) plus boolean flags.
  v2 — Phase 3.B.3: alist values may additionally be 256-color
       descriptor `(palette N)' (cons-list, N = 0..255) or truecolor
       descriptor `(rgb R G B)' (cons-list, R/G/B = 0..255).  Existing
       symbol values continue to mean 16-color (backward-compatible).")

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

(defcustom emacs-redisplay-default-mode-line-format " %b "
  "Fallback MVP mode-line format used when BUFFER has no local value.
Supported percent escapes are intentionally small in Phase 3 MVP:
`%b' expands to the buffer name and `%%' expands to a literal percent."
  :type 'string
  :group 'emacs-redisplay)

(defcustom emacs-redisplay-default-header-line-format nil
  "Fallback `header-line-format' when BUFFER has no local value.
nil means no header line (Emacs default).  Uses the same %-spec vocabulary as
the mode line (Doc 06 E6)."
  :type '(choice (const nil) string sexp)
  :group 'emacs-redisplay)

(defcustom emacs-redisplay-default-cursor-type 'box
  "Fallback cursor shape when BUFFER has no local `cursor-type' (Doc 06 E6).
One of `box', `hollow', `bar', `(bar . WIDTH)', `hbar', `(hbar . HEIGHT)',
t (frame default) or nil (no cursor) — matching Emacs `cursor-type'."
  :type 'sexp
  :group 'emacs-redisplay)

(defvar emacs-redisplay-paint-mode-line-p t
  "Non-nil means reserve the last row of each window for a mode line.")

;;; Glyph / glyph-row / glyph-matrix struct (Doc 43 §2.3)

(cl-defstruct (emacs-redisplay-glyph
               (:constructor emacs-redisplay--make-glyph)
               (:copier      nil))
  "A single glyph (= one displayed cell) per Doc 43 §2.3 / §2.4.
The `face' slot stores the raw / merged source face spec (symbol or
plist or list), preserved for overlay-merge intermediate state and
test observability.  The `realized-face' slot stores the *normalized*
SGR-ready attribute alist consumable by `emacs-tui-backend' (= Phase
3.B.1 face-realize MVP, Doc 43 §2.4)."
  (char          ?\s)        ;; codepoint (integer)
  (face          nil)        ;; raw face spec (symbol/plist/list)
  (realized-face nil)        ;; SGR-ready attribute alist (Phase 3.B.1)
  (face-id       0)          ;; realized face id (MVP = 0 default)
  (width         1)          ;; glyph width in cells (1 ASCII / 2 CJK)
  (composition   nil)        ;; nil or composition reference (deferred)
  (display-spec  nil)        ;; nil or display property override
  (mouse-face    nil)        ;; nil or `mouse-face' spec (hover highlight)
  (buf-pos       nil))       ;; source buffer position (for tooltips, etc.)

(cl-defstruct (emacs-redisplay-glyph-row
               (:constructor emacs-redisplay--make-glyph-row)
               (:copier      nil))
  "A row of glyphs in a glyph matrix per Doc 43 §2.3."
  (glyphs    nil)           ;; vector of emacs-redisplay-glyph
  (used      0)             ;; integer = active glyph count
  (hash      0)             ;; row hash for diff propagation
  (pos-delta 0)             ;; Phase 3.B.6 lazy buf-pos shift (skip path)
  (start-pos nil)           ;; buffer position at row start
  (end-pos   nil)           ;; buffer position at row end (exclusive)
  (continuation-p nil)      ;; non-nil if this row continues the previous
  (direction 'left-to-right)) ;; base paragraph direction (UAX #9 P2/P3)

(cl-defstruct (emacs-redisplay-glyph-matrix
               (:constructor emacs-redisplay--make-glyph-matrix)
               (:copier      nil))
  "A 2D glyph matrix attached to a window per Doc 43 §2.3."
  (rows      nil)           ;; vector of emacs-redisplay-glyph-row
  (width     0)             ;; column count
  (height    0)             ;; row count
  (window    nil)           ;; owning emacs-window leaf
  (dirty-set nil)           ;; bool-vector of dirty row indices
  (cursor    nil)           ;; cons (ROW . COL) or nil
  (fingerprint nil)         ;; Phase 3.B.5 rebuild short-circuit key
  (line-cache nil))         ;; Phase 3.B.6 per-row (LINE . OVLY-FP) input cache

;;; Driver handle

(cl-defstruct (emacs-redisplay-handle
               (:constructor emacs-redisplay--make-handle)
               (:copier      nil)
               (:predicate   emacs-redisplay-handlep))
  "Opaque handle returned by `emacs-redisplay-init'."
  (id          nil :read-only t)   ;; gensym-style id
  (alive-p     t)                  ;; nil after shutdown
  (backend     nil)                ;; emacs-tui-backend handle (nil OK)
  (window-cache nil)               ;; alist (window-id . glyph-matrix)
  (text-cache  nil))               ;; Phase 3.B.7 buffer-string LRU

(defun emacs-redisplay-glyph-matrix-dirty-rows (matrix)
  "Backward-compatible alias for MATRIX's dirty row bitvector."
  (vconcat (emacs-redisplay-glyph-matrix-dirty-set matrix)))

;;; Module-private id counter

(defvar emacs-redisplay--handle-counter 0
  "Monotonic counter for redisplay handle ids (printable as `rd-N').")

;;; Logging helper

(defun emacs-redisplay--log (fmt &rest args)
  "When logging is enabled, append a formatted line to *Messages*."
  (when emacs-redisplay-log-enabled
    (apply #'message (concat "[emacs-redisplay] " fmt) args)))

;;; Face registry + face-realize (Phase 3.B.1, Doc 43 §2.4 MVP)
;;
;; `emacs-redisplay-realize-face' is a SGR-oriented normalizer: it
;; takes any face spec form (nil / face-symbol / plist / cascade-list)
;; and returns a flat attribute alist consumable by
;; `emacs-tui-backend--sgr-from-face'.  Inheritance (= `:inherit') is
;; expanded; numeric / string color names are mapped to the backend's
;; symbolic palette (= `red' / `bright-blue' / `default' / nil).
;;
;; The cache key is the raw spec value (compared with `equal').  We
;; intentionally do NOT plug into a global LRU at the MVP stage; the
;; cache is single-table reset by `emacs-redisplay-face-cache-clear'
;; (called e.g. on backend swap, per Doc 43 §2.4 invariant 4).

(defvar emacs-redisplay--face-registry (make-hash-table :test 'eq)
  "Local face-name → attribute-plist registry (= MVP fallback).
Populated by `emacs-redisplay-defface'.  When upstream
`nelisp-face-attributes' is available *and* returns a value, we prefer
that; otherwise we fall back to this table so ERT runs in a vanilla
host Emacs.")

(defvar emacs-redisplay--face-cache (make-hash-table :test 'equal)
  "Memoization cache: raw face spec → realized attribute alist.")

(defcustom emacs-redisplay-face-realize-default-foreground nil
  "Optional default foreground (symbol) when realized face has none."
  :type '(choice (const nil) symbol)
  :group 'emacs-redisplay)

(defcustom emacs-redisplay-face-realize-default-background nil
  "Optional default background (symbol) when realized face has none."
  :type '(choice (const nil) symbol)
  :group 'emacs-redisplay)

(defconst emacs-redisplay--face-color-name-map
  '(("black" . black) ("red" . red) ("green" . green)
    ("yellow" . yellow) ("blue" . blue) ("magenta" . magenta)
    ("cyan" . cyan) ("white" . white)
    ("brightblack" . bright-black) ("brightred" . bright-red)
    ("brightgreen" . bright-green) ("brightyellow" . bright-yellow)
    ("brightblue" . bright-blue) ("brightmagenta" . bright-magenta)
    ("brightcyan" . bright-cyan) ("brightwhite" . bright-white)
    ("gray" . bright-black) ("grey" . bright-black)
    ("default" . default) ("none" . default))
  "Lowercase color-name string → backend palette symbol map (MVP subset).
Unknown strings are passed through as `default' (= no SGR emitted).")

(defun emacs-redisplay--parse-color-spec (spec)
  "Parse SPEC into a normalized color descriptor.

Returns a plist `(:type TYPE :value V)' where TYPE is one of:
  16        — V is a backend palette symbol (`red', `bright-blue', ...)
  256       — V is an integer 0..255 (xterm 256-color palette)
  truecolor — V is a list `(R G B)' with each component 0..255

Returns nil for SPEC = nil / `unspecified' / unrecognized shapes
(callers degrade to `default' = no SGR emitted for that channel).

Recognized SPEC shapes (Phase 3.B.3, Doc 43 §2.4 v2):
  nil / `unspecified'                       → nil
  symbol like `red' / `bright-blue'         → 16-color (registry lookup)
  symbol like `:palette-N' (keyword)        → 256-color (N = 0..255)
  string \"#rrggbb\" or \"#RRGGBB\"           → truecolor
  string \"red\" (lowercase color name)     → 16-color (registry lookup)
  list (palette N) / cons (palette . N)     → 256-color
  list (rgb R G B) / cons (rgb R G B)       → truecolor (already normal)
  plist (:r R :g G :b B)                    → truecolor

Out-of-range integers (negative / >255) are clamped silently to keep
the SGR pipeline robust against bad face data (= MVP graceful
degrade).  This contract is consumed by
`emacs-redisplay--face-color->symbol' (= realize layer) and by
`emacs-tui-backend--color-code' (= SGR emit layer)."
  (cl-flet ((clamp (n) (cond ((< n 0) 0) ((> n 255) 255) (t n))))
    (cond
     ;; nil / unspecified
     ((null spec) nil)
     ((eq spec 'unspecified) nil)
     ;; Keyword `:palette-N'
     ((and (symbolp spec)
           (let ((name (symbol-name spec)))
             (string-match-p "\\`:palette-[0-9]+\\'" name)))
      (let* ((name (symbol-name spec))
             (n (string-to-number (substring name (length ":palette-")))))
        (list :type 256 :value (clamp n))))
     ;; Plain symbol = 16-color registry symbol (validated downstream)
     ((symbolp spec)
      (list :type 16 :value spec))
     ;; "#rrggbb" hex string
     ((and (stringp spec)
           (string-match "\\`#\\([0-9a-fA-F]\\{6\\}\\)\\'" spec))
      (let* ((hex (match-string 1 spec))
             (r (string-to-number (substring hex 0 2) 16))
             (g (string-to-number (substring hex 2 4) 16))
             (b (string-to-number (substring hex 4 6) 16)))
        (list :type 'truecolor :value (list r g b))))
     ;; "red" / lowercase color name
     ((stringp spec)
      (let* ((key (downcase (replace-regexp-in-string "[ \t-]+" ""
                                                      spec)))
             (sym (cdr (assoc key emacs-redisplay--face-color-name-map))))
        (when sym
          (list :type 16 :value sym))))
     ;; Plist (:r R :g G :b B)
     ((and (listp spec)
           (keywordp (car spec))
           (plist-member spec :r)
           (plist-member spec :g)
           (plist-member spec :b))
      (let ((r (plist-get spec :r))
            (g (plist-get spec :g))
            (b (plist-get spec :b)))
        (when (and (integerp r) (integerp g) (integerp b))
          (list :type 'truecolor
                :value (list (clamp r) (clamp g) (clamp b))))))
     ;; (palette N) or (palette . N)
     ((and (consp spec) (eq (car spec) 'palette))
      (let ((n (if (consp (cdr spec)) (cadr spec) (cdr spec))))
        (when (integerp n)
          (list :type 256 :value (clamp n)))))
     ;; (rgb R G B) or (rgb R G B)
     ((and (consp spec) (eq (car spec) 'rgb)
           (= (length spec) 4)
           (cl-every #'integerp (cdr spec)))
      (list :type 'truecolor
            :value (mapcar #'clamp (cdr spec))))
     (t nil))))

(defun emacs-redisplay--face-color->symbol (color)
  "Map COLOR to a backend-ready color descriptor or palette symbol.

Returns nil when COLOR resolves to no SGR override (= unspecified).

For 16-color (= legacy MVP) inputs, returns a plain palette *symbol*
(`red', `bright-blue', `default', ...) so existing alist consumers
(downstream `emacs-tui-backend--sgr-from-face') stay compatible.

For 256-color / truecolor inputs (Phase 3.B.3, Doc 43 §2.4 v2),
returns a normalized cons / list descriptor:
  256-color  → (palette N)        (N = 0..255)
  truecolor  → (rgb R G B)        (R/G/B = 0..255)

The backend SGR layer dispatches on the descriptor shape."
  (let ((parsed (emacs-redisplay--parse-color-spec color)))
    (cond
     ((null parsed)
      ;; Unknown / unspecified — for legacy strings degrade to
      ;; `default' so the SGR pass simply skips the channel; for
      ;; everything else nil.
      (cond
       ((stringp color) 'default)
       (t nil)))
     (t
      (pcase (plist-get parsed :type)
        (16        (plist-get parsed :value))
        (256       (list 'palette (plist-get parsed :value)))
        ('truecolor (cons 'rgb (plist-get parsed :value))))))))

(defun emacs-redisplay--face-weight->bold (weight)
  "Return non-nil iff WEIGHT (a symbol) means bold-or-bolder."
  (memq weight '(bold semi-bold extra-bold ultra-bold heavy black)))

(defun emacs-redisplay--face-attributes-from-registry (sym)
  "Lookup SYM in upstream + local registry; return attribute plist."
  (or (and (fboundp 'nelisp-face-attributes)
           (condition-case _err
               (nelisp-face-attributes sym)
             (error nil)))
      (gethash sym emacs-redisplay--face-registry)))

(defun emacs-redisplay--face-resolve-spec (spec depth seen)
  "Internal recursive resolver of SPEC into a flat attribute plist.
Mirrors `nelisp-face-resolve' shape so the two stay interchangeable;
falls back to the local registry when upstream is absent.  DEPTH
bounds the `:inherit' chain (cap 16); SEEN guards cycles."
  (cond
   ((null spec) nil)
   ((>= depth 16) nil)
   ((symbolp spec)
    (cond
     ((memq spec seen) nil)
     (t
      (let ((own (emacs-redisplay--face-attributes-from-registry spec)))
        (cond
         ((null own) nil)
         (t
          (let ((inherit (plist-get own :inherit))
                (base (cl-loop for (k v) on own by #'cddr
                               unless (eq k :inherit)
                               nconc (list k v))))
            (if (null inherit)
                base
              (emacs-redisplay--face-merge-plists
               base
               (emacs-redisplay--face-resolve-spec
                inherit (1+ depth) (cons spec seen))))))))))
    )
   ((and (listp spec) (keywordp (car spec)))
    ;; Raw plist with possible :inherit
    (let ((inherit (plist-get spec :inherit))
          (base (cl-loop for (k v) on spec by #'cddr
                         unless (eq k :inherit)
                         nconc (list k v))))
      (if (null inherit)
          base
        (emacs-redisplay--face-merge-plists
         base
         (emacs-redisplay--face-resolve-spec
          inherit (1+ depth) seen)))))
   ((listp spec)
    ;; Cascade — left wins.
    (let (acc)
      (dolist (entry spec)
        (let ((piece (emacs-redisplay--face-resolve-spec
                      entry depth seen)))
          (when piece
            (setq acc (emacs-redisplay--face-merge-plists acc piece)))))
      acc))
   (t nil)))

(defun emacs-redisplay--face-merge-plists (left right)
  "Return LEFT overlaid on RIGHT (LEFT wins on key conflict)."
  (let ((result (copy-sequence left)))
    (cl-loop for (k v) on right by #'cddr
             unless (plist-member result k)
             do (setq result (nconc result (list k v))))
    result))

(defun emacs-redisplay--face-plist->alist (plist)
  "Translate Emacs-vocab attribute PLIST to backend SGR alist.

Recognized keys:
  :foreground / :background  → (:foreground . SYM) / (:background . SYM)
  :weight (bold-or-bolder)   → (:bold . t)
  :slant (italic / oblique)  → (:italic . t)  -- not yet emitted by SGR
  :underline (non-nil)       → (:underline . t)
  :inverse-video (non-nil)   → (:reverse . t)
  :reverse (non-nil)         → (:reverse . t)
  :bold (non-nil)            → (:bold . t)   -- short-hand pass-through
  :italic (non-nil)          → (:italic . t)

Unknown / `unspecified' values are skipped.  Returns nil when no
attribute survives normalization."
  (let ((out nil))
    (cl-loop for (k v) on plist by #'cddr do
             (pcase k
               (:foreground
                (let ((sym (emacs-redisplay--face-color->symbol v)))
                  (when sym (push (cons :foreground sym) out))))
               (:background
                (let ((sym (emacs-redisplay--face-color->symbol v)))
                  (when sym (push (cons :background sym) out))))
               (:weight
                (when (emacs-redisplay--face-weight->bold v)
                  (push (cons :bold t) out)))
               (:bold
                (when v (push (cons :bold t) out)))
               (:slant
                (when (memq v '(italic oblique))
                  (push (cons :italic t) out)))
               (:italic
                (when v (push (cons :italic t) out)))
               (:underline
                (when v (push (cons :underline t) out)))
               (:inverse-video
                (when v (push (cons :reverse t) out)))
               (:reverse
                (when v (push (cons :reverse t) out)))
               (_ nil)))
    ;; Apply defaults if no fg / bg survived.
    (when (and emacs-redisplay-face-realize-default-foreground
               (not (assq :foreground out)))
      (push (cons :foreground
                  emacs-redisplay-face-realize-default-foreground)
            out))
    (when (and emacs-redisplay-face-realize-default-background
               (not (assq :background out)))
      (push (cons :background
                  emacs-redisplay-face-realize-default-background)
            out))
    (nreverse out)))

(defun emacs-redisplay-realize-face (spec)
  "Realize face SPEC into a backend-ready SGR attribute alist.

Accepts:
  nil                       — returns nil (= default face).
  FACE-SYMBOL               — registry lookup with `:inherit' chain.
  (:foreground STR ...)     — raw plist; `:inherit' supported.
  (FACE ...)                — cascade, left-wins merge.

Returns a flat alist consumable by
`emacs-tui-backend--sgr-from-face' with keys `:foreground' /
`:background' / `:bold' / `:italic' / `:underline' / `:reverse'.
Unknown / `unspecified' values are dropped.

Result is memoized in `emacs-redisplay--face-cache'; call
`emacs-redisplay-face-cache-clear' on backend swap or registry
mutation to invalidate.

This API is the Phase 3.B.1 face-realize MVP per Doc 43 v2 §2.4."
  (cond
   ((null spec) nil)
   (t
    (let ((cached (gethash spec emacs-redisplay--face-cache 'miss)))
      (cond
       ((not (eq cached 'miss)) cached)
       (t
        (let* ((upstream (and (fboundp 'nelisp-face-resolve)
                              (condition-case _err
                                  (nelisp-face-resolve spec)
                                (error nil))))
               ;; Always merge in the local registry's view as fallback
               ;; so faces registered via `emacs-redisplay-defface' work
               ;; even when upstream `nelisp-face-resolve' is loaded but
               ;; has no entry for the spec (= ERT in vanilla host).
               (local (emacs-redisplay--face-resolve-spec spec 0 nil))
               (plist (emacs-redisplay--face-merge-plists
                       (or upstream nil) (or local nil)))
               (alist (emacs-redisplay--face-plist->alist plist)))
          (puthash spec alist emacs-redisplay--face-cache)
          alist)))))))

(defun emacs-redisplay-defface (name attr-plist)
  "Register face NAME with ATTR-PLIST in the local face registry.
The same name registered via upstream `nelisp-face-define' takes
precedence; this helper is the MVP fallback so ERTs run in a vanilla
host Emacs.  Returns NAME."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (unless (and (listp attr-plist) (zerop (mod (length attr-plist) 2)))
    (signal 'wrong-type-argument (list 'plistp attr-plist)))
  (puthash name attr-plist emacs-redisplay--face-registry)
  ;; Mutating the registry invalidates the realization cache.
  (emacs-redisplay-face-cache-clear)
  name)

(defun emacs-redisplay-face-attributes (name)
  "Return the registered attribute plist for face NAME, or nil.
Defers to upstream `nelisp-face-attributes' first; falls back to the
local registry maintained by `emacs-redisplay-defface'."
  (emacs-redisplay--face-attributes-from-registry name))

(defun emacs-redisplay-face-cache-clear ()
  "Drop every cached face realization.  Returns the entry count cleared.
Call after backend swap (Doc 43 §2.4 invariant 4) or after registry
mutation."
  (let ((n (hash-table-count emacs-redisplay--face-cache)))
    (clrhash emacs-redisplay--face-cache)
    n))

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

(defun emacs-redisplay--space-display-width (spec col)
  "Cell width of a `(space ...)' display SPEC at column COL, or nil.
SPEC may be a single `(space ...)' spec or a list of display specs (the
normalized form `nelisp-display-resolve' / Emacs produce, e.g.
`((space :width 5))').  Honors `(space :width N)' (N cells) and
`(space :align-to C)' (stretch to column C).  Returns nil when no
`(space ...)' spec is present so callers fall back to the normal
character width.  Image / slice specs are not handled yet (they need
backend glyph support)."
  (cond
   ((not (consp spec)) nil)
   ((eq (car spec) 'space)
    (let ((w (plist-get (cdr spec) :width))
          (a (plist-get (cdr spec) :align-to)))
      (cond
       ((numberp w) (max 1 (truncate w)))
       ((numberp a) (max 1 (- (truncate a) col)))
       (t 1))))
   (t
    ;; a list of display specs -- use the first `(space ...)' found
    (let (result)
      (dolist (s spec)
        (when (and (null result) (consp s) (eq (car s) 'space))
          (setq result (emacs-redisplay--space-display-width s col))))
      result))))

(defun emacs-redisplay--display-replacement-string (spec)
  "Return the string SPEC tells redisplay to render in place of the buffer
text, or nil when SPEC layers onto the text rather than replacing it.

- a bare string                      -> itself (`'display \"x\"')
- `(image ... :string S ...)'        -> S (the TTY text fallback), else nil
                                        (a real image is a backend placeholder)
- a list of specs `(SPEC...)'        -> the first replacement found in it
- `(space/raise/height/slice ...)'   -> nil (width/attribute specs, not text)

Mirrors how Emacs treats a `display' property whose value is a string or
image (Doc 06 E3)."
  (cond
   ((stringp spec) spec)
   ((not (consp spec)) nil)
   ((eq (car spec) 'image)
    (let ((s (plist-get (cdr spec) :string)))
      (and (stringp s) s)))
   ((memq (car spec)
          '(space raise height slice when margin left-fringe right-fringe
                  left-margin right-margin))
    nil)
   (t
    ;; A list of display specs: first string element, or a nested image's
    ;; :string fallback.
    (let ((result nil) (rest spec))
      (while (and (consp rest) (null result))
        (let ((el (car rest)))
          (cond
           ((stringp el) (setq result el))
           ((and (consp el) (eq (car el) 'image))
            (setq result (emacs-redisplay--display-replacement-string el)))))
        (setq rest (cdr rest)))
      result))))

(defun emacs-redisplay--display-attribute (spec key)
  "Extract the KEY (`raise' or `height') factor from display SPEC, or nil.
SPEC may be `(raise N)' / `(height N)' directly or a list containing one.
These attributes layer onto the covered text (no TTY column change); a GUI
backend reads them off the glyph's `display-spec' (Doc 06 E3)."
  (cond
   ((not (consp spec)) nil)
   ((eq (car spec) key) (cadr spec))
   ((memq (car spec) '(image space slice when margin)) nil)
   (t
    (let ((result nil) (rest spec))
      (while (and (consp rest) (null result))
        (let ((el (car rest)))
          (when (and (consp el) (eq (car el) key))
            (setq result (cadr el))))
        (setq rest (cdr rest)))
      result))))

(defun emacs-redisplay--overlays-in (beg end &optional buffer)
  "Return rendering overlays through the owning buffer/overlay APIs."
  (if (emacs-redisplay--standard-buffer-p buffer)
      (with-current-buffer buffer (overlays-in beg end))
    (append (and (fboundp 'emacs-buffer-overlays-in)
                 (emacs-buffer-overlays-in beg end buffer))
            (and (fboundp 'nelisp-ovly-overlays-in)
                 (let ((nelisp-ec--current-buffer buffer))
                   (nelisp-ovly-overlays-in beg end))))))

(defun emacs-redisplay--ovly-prop (overlay prop)
  "Return PROP from OVERLAY using its public owner API."
  (cond
   ((and (fboundp 'overlayp) (overlayp overlay)) (overlay-get overlay prop))
   ((and (fboundp 'emacs-buffer-overlayp) (emacs-buffer-overlayp overlay))
    (emacs-buffer-overlay-get overlay prop))
   ((fboundp 'nelisp-ovly-get) (nelisp-ovly-get overlay prop))))

(defun emacs-redisplay--ovly-bounds (overlay)
  "Return OVERLAY's public (START . END) bounds."
  (cond
   ((and (fboundp 'overlayp) (overlayp overlay))
    (let ((start (overlay-start overlay)) (end (overlay-end overlay)))
      (and start end (cons start end))))
   ((and (fboundp 'emacs-buffer-overlayp) (emacs-buffer-overlayp overlay))
    (let ((start (emacs-buffer-overlay-start overlay))
          (end (emacs-buffer-overlay-end overlay)))
      (and start end (cons start end))))
   ((fboundp 'nelisp-ovly-start)
    (let ((start (nelisp-ovly-start overlay)) (end (nelisp-ovly-end overlay)))
      (and start end (cons start end))))))

;;; Buffer text + text-property access (works against emacs-buffer or
;;; nelisp-ec depending on which is wired in the host).

(defun emacs-redisplay--standard-buffer-p (buffer)
  "Non-nil for buffers owned by the host or the standalone buffer API.
Legacy `nelisp-ec-buffer' objects continue through their prefixed API."
  (and (fboundp 'bufferp) (bufferp buffer)
       (or (not (fboundp 'nelisp--repr))
           (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p buffer)))))

(defun emacs-redisplay--buffer-string (buffer)
  "Return the full text of BUFFER as a string (current narrowing OK).
Works whether BUFFER is a `nelisp-ec-buffer' (Phase 1) or has been
made-current via the emacs-buffer compatibility layer.  Returns the
empty string when no text is reachable (= safe MVP default)."
  (cond
   ((null buffer) "")
   ((emacs-redisplay--standard-buffer-p buffer)
    (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer))
    (condition-case _err
        (let ((nelisp-ec--current-buffer buffer))
          (nelisp-ec-buffer-string))
      (error "")))
   ((stringp buffer) buffer)  ;; test convenience
   (t "")))

(defconst emacs-redisplay--text-cache-size 2
  "Max LRU entries in `emacs-redisplay-handle-text-cache'.")

(defun emacs-redisplay--cached-buffer-string (handle buffer)
  "Return BUFFER's text via HANDLE's text-cache (Phase 3.B.7).
Cache key = (BUFFER + TEXT-TICK + accessible bounds for native buffers).
When BUFFER is a string or nil,
falls back to the uncached path because there is no tick to gate on.
Cache holds at most `emacs-redisplay--text-cache-size' entries with
LRU ordering — the head is most-recent."
  (cond
   ((or (null buffer) (stringp buffer))
    (emacs-redisplay--buffer-string buffer))
   (t
    (let* ((tick (if (emacs-redisplay--standard-buffer-p buffer)
                     (with-current-buffer buffer
                       (list (buffer-chars-modified-tick) (point-min) (point-max)))
                   (and (fboundp 'emacs-buffer-buffer-text-tick)
                        (emacs-buffer-buffer-text-tick buffer))))
           (cache (emacs-redisplay-handle-text-cache handle))
           (hit (and tick
                     (cl-loop for entry in cache
                              when (and (eq (car entry) buffer)
                                        (equal (cadr entry) tick))
                              return entry))))
      (cond
       (hit
        (unless (eq (car cache) hit)
          (setf (emacs-redisplay-handle-text-cache handle)
                (cons hit (delq hit cache))))
        (cddr hit))
       (t
        (let* ((text (emacs-redisplay--buffer-string buffer))
               (new-entry (cons buffer (cons (or tick 0) text)))
               (trimmed (if (> (length cache)
                               (1- emacs-redisplay--text-cache-size))
                            (cl-subseq cache 0
                                       (1- emacs-redisplay--text-cache-size))
                          cache)))
          (setf (emacs-redisplay-handle-text-cache handle)
                (cons new-entry trimmed))
          text)))))))

(defun emacs-redisplay--buffer-substring (buffer start end)
  "Return BUFFER text in 1-based [START, END), with safe fallbacks."
  (cond
   ((null buffer) "")
   ((emacs-redisplay--standard-buffer-p buffer)
    (with-current-buffer buffer (buffer-substring-no-properties start end)))
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
   ((emacs-redisplay--standard-buffer-p buffer)
    (get-text-property pos prop buffer))
   ((fboundp 'emacs-buffer-get-text-property)
    (condition-case _err
        (emacs-buffer-get-text-property pos prop buffer)
      (error nil)))
   (t nil)))

(defun emacs-redisplay--invisibility-value (value spec)
  "Resolve invisible VALUE against SPEC, returning nil, t, or 2 (ellipsis)."
  (cond
   ((null value) nil)
   ((eq spec t) t)
   ((null spec) nil)
   (t
    (let ((result nil) (tags (if (listp value) value (list value))))
      (dolist (entry spec)
        (when (memq (if (consp entry) (car entry) entry) tags)
          (setq result (if (and (consp entry) (cdr entry)) 2 (or result t)))))
      result))))

(defun emacs-redisplay--invisible-at-p (pos buffer &optional overlays)
  "Return the visibility state at POS: nil, hidden t, or ellipsis 2."
  (let ((value (emacs-redisplay--text-property-at pos 'invisible buffer))
        (spec (emacs-redisplay--ml-local 'buffer-invisibility-spec buffer t)))
    (dolist (ov overlays)
      (let ((bounds (emacs-redisplay--ovly-bounds ov)))
        (when (and bounds (<= (car bounds) pos) (< pos (cdr bounds))
                   (emacs-redisplay--ovly-prop ov 'invisible))
          (setq value (emacs-redisplay--ovly-prop ov 'invisible)))))
    (emacs-redisplay--invisibility-value value spec)))

;;; Hash helper for diff propagation

(defun emacs-redisplay--row-hash (row-vec)
  "Return a stable hash for ROW-VEC (vector of glyphs).
The hash mixes char + the *realized* face alist (Phase 3.B.1) so a
text-property face change that resolves to a different SGR triggers
diff redraw even if the raw spec stayed identical (= e.g. registry
mutation under the same face name)."
  (let ((h 0)
        (i 0)
        (len (length row-vec)))
    (while (< i len)
      (let* ((g (aref row-vec i))
             (c (if g (emacs-redisplay-glyph-char g) 0))
             (f (if g (or (emacs-redisplay-glyph-realized-face g)
                          (emacs-redisplay-glyph-face g))
                  nil)))
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
    ;; Nil cells are blank cells, as for the tail of a wide glyph.  Avoid
    ;; allocating thousands of unused glyph objects before the first layout.
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
     :cursor nil
     :line-cache (make-vector height nil))))

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
         glyphs)
    (dotimes (i n)
      (let* ((pos (+ offset i))
             (face (emacs-redisplay--text-property-at pos 'face buffer))
             (display (emacs-redisplay--text-property-at pos 'display buffer))
             (resolved (emacs-redisplay--resolve-face face)))
        (unless (emacs-redisplay--invisible-at-p pos buffer)
          (push (emacs-redisplay--make-glyph
                 :char (aref text i)
                 :face resolved
                 :realized-face (emacs-redisplay-realize-face face)
                 :face-id 0
                 :width 1
                 :composition nil
                 :display-spec (emacs-redisplay--resolve-display display nil)
                 :mouse-face (emacs-redisplay--text-property-at
                              pos 'mouse-face buffer)
                 :buf-pos pos)
                glyphs))))
    (vconcat (nreverse glyphs))))

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

(defun emacs-redisplay--combining-mark-p (ch)
  "Return non-nil when CH composes with the character before it.
`emacs-redisplay--char-width' is not a usable test for this.  It defers
to `char-width', and on a host with no display configured `char-width'
answers 2 for U+0301 COMBINING ACUTE ACCENT -- whose Unicode general
category is Mn, nonspacing mark.  The category is what actually says a
character carries no column of its own, so ask for it where the
property tables exist and fall back to the width elsewhere."
  (let ((category (and (integerp ch)
                       (fboundp 'get-char-code-property)
                       (condition-case _err
                           (get-char-code-property ch 'general-category)
                         (error nil)))))
    (if category
        ;; Mn nonspacing, Me enclosing.  Mc is a *spacing* combining
        ;; mark and does take a column, so it is deliberately absent.
        (memq category '(Mn Me))
      (= (emacs-redisplay--char-width ch) 0))))

(defun emacs-redisplay--char-bidi-class (ch)
  "Return the simplified bidi class of CH: `L', `R', `AL', or `neutral'.
Only the strong directional classes needed for base-direction detection
(UAX #9 P2/P3) are distinguished; weak / neutral types collapse to
`neutral'.  Hebrew is R, Arabic-family scripts are AL, ASCII / Latin /
CJK letters are L."
  (cond
   ;; strong R: Hebrew + Hebrew presentation forms
   ((or (and (>= ch #x0590) (<= ch #x05FF))
        (and (>= ch #xFB1D) (<= ch #xFB4F)))
    'R)
   ;; strong AL: Arabic / Syriac / Thaana + Arabic presentation forms
   ((or (and (>= ch #x0600) (<= ch #x07BF))
        (and (>= ch #x08A0) (<= ch #x08FF))
        (and (>= ch #xFB50) (<= ch #xFDFF))
        (and (>= ch #xFE70) (<= ch #xFEFF)))
    'AL)
   ;; EN: European numbers (read left-to-right even inside RTL)
   ((and (>= ch ?0) (<= ch ?9)) 'EN)
   ;; AN: Arabic-Indic / Extended Arabic-Indic numbers
   ((or (and (>= ch #x0660) (<= ch #x0669))
        (and (>= ch #x06F0) (<= ch #x06F9)))
    'AN)
   ;; ES: European number separator (+ -)
   ((or (= ch ?+) (= ch ?-)) 'ES)
   ;; ET: European number terminator ($ % # currencies degree)
   ((or (= ch ?$) (= ch ?%) (= ch ?#)
        (= ch #x00B0) (= ch #x00A3) (= ch #x00A5) (= ch #x20AC))
    'ET)
   ;; CS: common number separator (, . : /)
   ((or (= ch ?,) (= ch ?.) (= ch ?:) (= ch ?/)) 'CS)
   ;; strong L: ASCII letters, Latin-1/Extended letters, CJK and beyond
   ((or (and (>= ch ?A) (<= ch ?Z))
        (and (>= ch ?a) (<= ch ?z))
        (and (>= ch #x00C0) (<= ch #x024F))
        (>= ch #x2E80))
    'L)
   (t 'neutral)))

(defun emacs-redisplay--base-direction (string)
  "Return the base paragraph direction of STRING (UAX #9 rules P2/P3).
The direction is set by the first strong (L / R / AL) character: L yields
`left-to-right', R or AL yields `right-to-left'.  With no strong character
the direction defaults to `left-to-right'."
  (let ((i 0) (n (length string)) (dir nil))
    (while (and (< i n) (null dir))
      (let ((cls (emacs-redisplay--char-bidi-class (aref string i))))
        (cond
         ((eq cls 'L) (setq dir 'left-to-right))
         ((or (eq cls 'R) (eq cls 'AL)) (setq dir 'right-to-left))))
      (setq i (1+ i)))
    (or dir 'left-to-right)))

(defun emacs-redisplay--reverse-glyph-vector (vec)
  "Return a new vector with VEC's glyphs in reverse visual order.
This is the UAX #9 level-1 reorder applied to a right-to-left paragraph:
correct for pure-RTL text (each glyph keeps its own `buf-pos', so the visual-
first glyph is the logical-last char).  Mixed L/R run reordering with proper
embedding levels (so an embedded Latin/number run keeps its internal order) is
a follow-up."
  (let* ((n (length vec))
         (out (make-vector n nil)))
    (dotimes (i n)
      (aset out i (aref vec (- n 1 i))))
    out))


(defun emacs-redisplay--char-level (cls base-level)
  "Embedding level for a glyph of bidi CLS given BASE-LEVEL (0 or 1).
Simplified UAX #9 level assignment: a strong char matching the base direction
stays at BASE-LEVEL; an opposite-direction strong char gets BASE-LEVEL+1 (an
embedded run); European numbers (EN) read left-to-right, so in an RTL base they
get BASE-LEVEL+1 (even level); neutrals take the base level."
  (let ((base-rtl (= (logand base-level 1) 1)))
    (cond
     ((eq cls 'L)  (if base-rtl (1+ base-level) base-level))
     ((or (eq cls 'R) (eq cls 'AL)) (if base-rtl base-level (1+ base-level)))
     ((or (eq cls 'EN) (eq cls 'AN)) (if base-rtl (1+ base-level) base-level))
     (t base-level))))

(defun emacs-redisplay--reverse-subrange (vec i j)
  "Reverse VEC[I..J] (inclusive) in place."
  (while (< i j)
    (let ((tmp (aref vec i)))
      (aset vec i (aref vec j))
      (aset vec j tmp))
    (setq i (1+ i) j (1- j))))

(defun emacs-redisplay--bidi-neutral-p (cls)
  "Non-nil when bidi class CLS is a neutral (resolved by rules N1/N2)."
  (eq cls 'neutral))

(defun emacs-redisplay--bidi-dir-of (cls base-level)
  "Resolved direction symbol (`L' or `R') of a strong/number class CLS.
For neutral resolution (rule N1) numbers act as R.  A nil class (a line edge)
yields the base direction."
  (cond
   ((eq cls 'L) 'L)
   ((or (eq cls 'R) (eq cls 'AL) (eq cls 'EN) (eq cls 'AN)) 'R)
   (t (if (= (logand base-level 1) 1) 'R 'L))))

(defun emacs-redisplay--bidi-resolve-weak (classes base-level)
  "Resolve weak bidi types in CLASSES in place (UAX #9 W2-W7, simplified).
W1 (NSM) is handled upstream by glyph composition, so it is omitted.  After
this pass CLASSES holds only L, R, EN, AN, and neutral."
  (let ((n (length classes))
        (sos (if (= (logand base-level 1) 1) 'R 'L)))
    ;; W2: EN -> AN when the last strong type seen is AL.
    (let ((last-strong sos))
      (dotimes (i n)
        (let ((c (aref classes i)))
          (cond
           ((memq c '(L R AL)) (setq last-strong c))
           ((and (eq c 'EN) (eq last-strong 'AL)) (aset classes i 'AN))))))
    ;; W3: AL -> R.
    (dotimes (i n) (when (eq (aref classes i) 'AL) (aset classes i 'R)))
    ;; W4: a single ES or CS between two EN -> EN; a single CS between two AN -> AN.
    (dotimes (i n)
      (when (and (> i 0) (< i (1- n)))
        (let ((c (aref classes i))
              (p (aref classes (1- i)))
              (q (aref classes (1+ i))))
          (cond
           ((and (memq c '(ES CS)) (eq p 'EN) (eq q 'EN)) (aset classes i 'EN))
           ((and (eq c 'CS) (eq p 'AN) (eq q 'AN)) (aset classes i 'AN))))))
    ;; W5: a run of ET adjacent to EN -> EN.
    (let ((i 0))
      (while (< i n)
        (if (eq (aref classes i) 'ET)
            (let ((j i))
              (while (and (< j n) (eq (aref classes j) 'ET)) (setq j (1+ j)))
              (when (or (and (> i 0) (eq (aref classes (1- i)) 'EN))
                        (and (< j n) (eq (aref classes j) 'EN)))
                (let ((k i))
                  (while (< k j) (aset classes k 'EN) (setq k (1+ k)))))
              (setq i j))
          (setq i (1+ i)))))
    ;; W6: any remaining ES / ET / CS -> neutral (ON).
    (dotimes (i n)
      (when (memq (aref classes i) '(ES ET CS)) (aset classes i 'neutral)))
    ;; W7: EN -> L when the last strong type seen is L.
    (let ((last-strong sos))
      (dotimes (i n)
        (let ((c (aref classes i)))
          (cond
           ((memq c '(L R)) (setq last-strong c))
           ((and (eq c 'EN) (eq last-strong 'L)) (aset classes i 'L))))))
    classes))

(defun emacs-redisplay--bidi-open-bracket (ch)
  "Return the matching closing-bracket char for opening bracket CH, else nil."
  (cond ((eq ch ?\() ?\))
        ((eq ch ?\[) ?\])
        ((eq ch ?{) ?})
        (t nil)))

(defun emacs-redisplay--bidi-close-bracket-p (ch)
  "Non-nil when CH is a closing bracket."
  (memq ch '(?\) ?\] ?})))

(defun emacs-redisplay--bidi-bracket-dir (cls)
  "Direction (`L'/`R') of a resolved class CLS for N0 (EN/AN count as R), or nil."
  (cond ((eq cls 'L) 'L)
        ((memq cls '(R EN AN)) 'R)
        (t nil)))

(defun emacs-redisplay--bidi-prev-strong-dir (classes pos base-level)
  "Direction of the first strong class before POS in CLASSES; sos if none."
  (let ((i (1- pos)) (dir nil))
    (while (and (>= i 0) (null dir))
      (setq dir (emacs-redisplay--bidi-bracket-dir (aref classes i)))
      (setq i (1- i)))
    (or dir (if (= (logand base-level 1) 1) 'R 'L))))

(defun emacs-redisplay--bidi-apply-n0 (classes open close base-level)
  "Apply rule N0 to the bracket pair at OPEN/CLOSE positions in CLASSES.
Both brackets take the embedding direction when a matching strong type is
inside; the opposite direction only when the inside has it AND the preceding
context is also that direction; otherwise they are left for N1."
  (let* ((e (if (= (logand base-level 1) 1) 'R 'L))
         (o (if (eq e 'L) 'R 'L))
         (has-e nil) (has-o nil) (k (1+ open)))
    (while (< k close)
      (let ((d (emacs-redisplay--bidi-bracket-dir (aref classes k))))
        (cond ((eq d e) (setq has-e t))
              ((eq d o) (setq has-o t))))
      (setq k (1+ k)))
    (let ((resolved
           (cond
            (has-e e)
            (has-o (if (eq (emacs-redisplay--bidi-prev-strong-dir
                            classes open base-level)
                           o)
                       o e))
            (t nil))))
      (when resolved
        (aset classes open resolved)
        (aset classes close resolved)))))

(defun emacs-redisplay--bidi-resolve-brackets (vec classes base-level)
  "Resolve matched bracket pairs in CLASSES in place (UAX #9 N0, simplified).
Pairs are matched with a stack over (), [], {}; each pair is resolved by
`emacs-redisplay--bidi-apply-n0'.  Operates after the weak pass and before
N1 so unresolved (no-strong-inside) brackets fall through to N1."
  (let ((n (length vec)) (stack nil))
    (dotimes (i n)
      (let* ((g (aref vec i))
             (ch (and g (emacs-redisplay-glyph-char g)))
             (closer (and ch (emacs-redisplay--bidi-open-bracket ch))))
        (cond
         (closer (push (cons i closer) stack))
         ((and ch (emacs-redisplay--bidi-close-bracket-p ch)
               stack (eq (cdar stack) ch))
          (let ((open (caar stack)))
            (setq stack (cdr stack))
            (emacs-redisplay--bidi-apply-n0 classes open i base-level))))))
    classes))

(defun emacs-redisplay--bidi-levels (vec base-level)
  "Return a vector of embedding levels for VEC's glyphs (simplified UAX #9).
Resolve neutral runs by rule N1 -- a run of neutrals between two strong
contexts of the same direction takes that direction, else the base direction
(N2) -- then assign levels with `emacs-redisplay--char-level'.  This keeps a
space or punctuation inside an embedded opposite-direction run attached to that
run instead of splitting it."
  (let* ((n (length vec))
         (classes (make-vector n 'neutral))
         (levels (make-vector n base-level)))
    (dotimes (i n)
      (let ((g (aref vec i)))
        (aset classes i (if g (emacs-redisplay--char-bidi-class
                               (emacs-redisplay-glyph-char g))
                          'neutral))))
    ;; W2-W7 (weak), then N0 (bracket pairs), then N1/N2 (neutrals): UAX #9 order
    (emacs-redisplay--bidi-resolve-weak classes base-level)
    (emacs-redisplay--bidi-resolve-brackets vec classes base-level)
    (let ((i 0))
      (while (< i n)
        (if (emacs-redisplay--bidi-neutral-p (aref classes i))
            (let ((j i))
              (while (and (< j n)
                          (emacs-redisplay--bidi-neutral-p (aref classes j)))
                (setq j (1+ j)))
              (let* ((before (emacs-redisplay--bidi-dir-of
                              (and (> i 0) (aref classes (1- i))) base-level))
                     (after (emacs-redisplay--bidi-dir-of
                             (and (< j n) (aref classes j)) base-level))
                     (resolved (if (eq before after)
                                   before
                                 (if (= (logand base-level 1) 1) 'R 'L)))
                     (k i))
                (while (< k j)
                  (aset classes k resolved)
                  (setq k (1+ k))))
              (setq i j))
          (setq i (1+ i)))))
    (dotimes (i n)
      (aset levels i (emacs-redisplay--char-level (aref classes i) base-level)))
    levels))

(defun emacs-redisplay--bidi-mirror-char (ch)
  "Return the mirror image of CH (UAX #9 L4 / Bidi_Mirrored), or nil.
Covers the common mirrored characters: brackets, angle brackets, guillemets."
  (cond
   ((eq ch ?\() ?\)) ((eq ch ?\)) ?\()
   ((eq ch ?\[) ?\]) ((eq ch ?\]) ?\[)
   ((eq ch ?{)  ?})  ((eq ch ?})  ?{)
   ((eq ch ?<)  ?>)  ((eq ch ?>)  ?<)
   ((eq ch #x00AB) #x00BB) ((eq ch #x00BB) #x00AB)
   (t nil)))

(defun emacs-redisplay--bidi-reorder-glyphs (vec base-level)
  "Reorder VEC into visual order for BASE-LEVEL (UAX #9 L2, simplified).
Assign each glyph an embedding level (`emacs-redisplay--char-level'), then for
L from the maximum level down to 1, reverse every maximal contiguous run of
glyphs whose level is >= L.  This flips the overall flow for an RTL paragraph
while leaving embedded opposite-direction runs (a Latin word or a number inside
Hebrew) in their own internal order.  Returns a new vector; LTR-only text
(max level 0) is returned in logical order."
  (let ((n (length vec)))
    (if (= n 0)
        vec
      (let ((levels (emacs-redisplay--bidi-levels vec base-level))
            (out (make-vector n nil))
            (maxlevel base-level))
        (dotimes (i n)
          (when (> (aref levels i) maxlevel)
            (setq maxlevel (aref levels i)))
          (aset out i (aref vec i)))
        (let ((lev maxlevel))
          (while (>= lev 1)
            (let ((i 0))
              (while (< i n)
                (if (>= (aref levels i) lev)
                    (let ((j i))
                      (while (and (< j n) (>= (aref levels j) lev))
                        (setq j (1+ j)))
                      (emacs-redisplay--reverse-subrange out i (1- j))
                      (emacs-redisplay--reverse-subrange levels i (1- j))
                      (setq i j))
                  (setq i (1+ i)))))
            (setq lev (1- lev))))
        ;; L4: mirror Bidi_Mirrored glyphs at odd (RTL) levels for display.
        ;; `levels' was reversed in lock-step with `out', so levels[i] is the
        ;; embedding level of out[i].
        (dotimes (i n)
          (when (= (logand (aref levels i) 1) 1)
            (let* ((g (aref out i))
                   (m (and g (emacs-redisplay--bidi-mirror-char
                              (emacs-redisplay-glyph-char g)))))
              (when m (setf (emacs-redisplay-glyph-char g) m)))))
        out))))

(defun emacs-redisplay--right-align-glyphs (vec width)
  "Return a WIDTH-length vector with VEC's glyphs flush against the right.
Cells left of the content are nil (rendered blank).  Used to right-align an
RTL paragraph row against the window's right edge.  When VEC already fills (or
exceeds) WIDTH it is returned unchanged."
  (let ((n (length vec)))
    (if (>= n width)
        vec
      (let ((out (make-vector width nil))
            (pad (- width n)))
        (dotimes (i n)
          (aset out (+ pad i) (aref vec i)))
        out))))
(defun emacs-redisplay--buffer-name (buffer)
  "Return BUFFER's display name for the MVP mode-line."
  (cond
   ((emacs-redisplay--standard-buffer-p buffer) (buffer-name buffer))
   ((and buffer (nelisp-ec-buffer-p buffer))
    (nelisp-ec-buffer-name buffer))
   ((stringp buffer) "*string*")
   (t "")))
(defun emacs-redisplay--mode-line-modified-indicator (buffer)
  "Return \"*\" when BUFFER is modified, else \"-\" (mode-line `%*' / `%+')."
  (if (if (emacs-redisplay--standard-buffer-p buffer)
          (buffer-modified-p buffer)
        (and buffer (nelisp-ec-buffer-p buffer)
             (fboundp 'nelisp-ec-buffer-modified-p)
             (nelisp-ec-buffer-modified-p buffer)))
      "*" "-"))


(defun emacs-redisplay--mode-line-format (buffer)
  "Return BUFFER's mode-line format or the Phase 3 MVP fallback."
  (cond
   ((emacs-redisplay--standard-buffer-p buffer)
    (buffer-local-value 'mode-line-format buffer))
   ((and buffer (nelisp-ec-buffer-p buffer))
    (emacs-redisplay--ml-local
     'mode-line-format buffer emacs-redisplay-default-mode-line-format))
   (t emacs-redisplay-default-mode-line-format)))

(defun emacs-redisplay--header-line-format (buffer)
  "Return BUFFER's `header-line-format', or the default (nil = no header line).
Doc 06 E6 — header lines reuse the mode-line %-spec machinery."
  (cond
   ((emacs-redisplay--standard-buffer-p buffer)
    (buffer-local-value 'header-line-format buffer))
   ((and buffer (nelisp-ec-buffer-p buffer))
    (emacs-redisplay--ml-local
     'header-line-format buffer emacs-redisplay-default-header-line-format))
   (t emacs-redisplay-default-header-line-format)))

(defun emacs-redisplay--header-line-format-to-string (format buffer)
  "Render header-line FORMAT for BUFFER (same %-spec vocabulary as the mode
line, Doc 06 E6)."
  (emacs-redisplay--mode-line-format-to-string format buffer))

(defun emacs-redisplay--header-line-enabled-p (buffer)
  "Non-nil when BUFFER has a non-nil `header-line-format' (Doc 06 E6)."
  (let ((fmt (emacs-redisplay--header-line-format buffer)))
    (and fmt (not (equal fmt "")))))

(defun emacs-redisplay--cursor-type (buffer)
  "Return BUFFER's `cursor-type', or the default shape (Doc 06 E6)."
  (cond
   ((emacs-redisplay--standard-buffer-p buffer)
    (buffer-local-value 'cursor-type buffer))
   ((and buffer (nelisp-ec-buffer-p buffer))
    (emacs-redisplay--ml-local
     'cursor-type buffer emacs-redisplay-default-cursor-type))
   (t emacs-redisplay-default-cursor-type)))

(defun emacs-redisplay--ml-text-before-point (buffer)
  "Return BUFFER's text from point-min up to point, or nil."
  (if (emacs-redisplay--standard-buffer-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point)))
    (let ((nelisp-ec--current-buffer buffer))
    (and (fboundp 'nelisp-ec-point) (fboundp 'nelisp-ec-buffer-substring)
         (ignore-errors
           (nelisp-ec-buffer-substring (if (fboundp 'nelisp-ec-point-min)
                                           (nelisp-ec-point-min) 1)
                                       (nelisp-ec-point)))))))

(defun emacs-redisplay--ml-line (buffer)
  "Line number at point in BUFFER (Doc 06 E2, %l)."
  (let ((s (emacs-redisplay--ml-text-before-point buffer)) (n 1) (i 0))
    (when s
      (while (< i (length s))
        (when (eq (aref s i) ?\n) (setq n (1+ n)))
        (setq i (1+ i))))
    n))

(defun emacs-redisplay--ml-column (buffer)
  "Return the visual column at point, including tabs and wide characters."
  (let* ((text (or (emacs-redisplay--ml-text-before-point buffer) ""))
         (i (length text)) (column 0)
         (tab (max 1 (emacs-redisplay--ml-local 'tab-width buffer 8))))
    (while (and (> i 0) (/= (aref text (1- i)) ?\n)) (setq i (1- i)))
    (while (< i (length text))
      (let ((ch (aref text i)))
        (setq column (if (= ch ?\t) (* (1+ (/ column tab)) tab)
                       (+ column (emacs-redisplay--char-width ch)))))
      (setq i (1+ i)))
    column))

(defun emacs-redisplay--ml-narrowed-p (buffer)
  "Return non-nil when BUFFER is narrowed (Doc 06 E2, %n)."
  (if (emacs-redisplay--standard-buffer-p buffer)
      (with-current-buffer buffer
        (or (> (point-min) 1) (< (point-max) (1+ (buffer-size)))))
    (let ((nelisp-ec--current-buffer buffer))
    (and (fboundp 'nelisp-ec-point-min) (fboundp 'nelisp-ec-buffer-size)
         (or (> (nelisp-ec-point-min) 1)
             (and (fboundp 'nelisp-ec-point-max)
                  (<= (nelisp-ec-point-max) (nelisp-ec-buffer-size))))))))

(defun emacs-redisplay--ml-local (sym buffer default)
  "Return BUFFER's buffer-local value of SYM, or DEFAULT."
  (if (emacs-redisplay--standard-buffer-p buffer)
      (if (with-current-buffer buffer (boundp sym))
          (buffer-local-value sym buffer) default)
    (if (and (fboundp 'emacs-buffer-local-variable-p)
           (emacs-buffer-local-variable-p sym buffer))
      (emacs-buffer-buffer-local-value sym buffer)
    default)))

(defvar emacs-redisplay--mode-line-window nil
  "Window whose mode/header line is being formatted.")
(defvar emacs-redisplay--mode-line-end nil
  "Exclusive visible end of the window currently being formatted.")

;; The standalone mode shim supplies only a placeholder.  These defaults
;; belong to redisplay, and must never replace host Emacs or user formats.
(when (fboundp 'nelisp--repr)
  (dolist (entry
           '((mode-line-front-space . "-")
             (mode-line-mule-info . ("%Z"))
             (mode-line-frame-identification . (" %F  "))
             (mode-line-modified . ("%1*%1+"))
             (mode-line-remote . ("%1@"))
             (mode-line-buffer-identification . ((:propertize "%12b" face mode-line-buffer-id)))
             (mode-line-position . ((-3 "%p") (line-number-mode (6 " L%l"))))
             (mode-line-modes-delimiters . ("(" . ")"))
             (mode-line-minor-modes . (:eval (emacs-redisplay--minor-modes)))
             (mode-line-end-spaces . ("-%-"))))
    (unless (boundp (car entry)) (set (car entry) (cdr entry))))
  (when (equal (default-value 'mode-line-format) " %b ")
    (setq-default mode-line-format
                  '("%e" mode-line-front-space mode-line-mule-info
                    mode-line-client mode-line-modified mode-line-remote
                    mode-line-window-dedicated mode-line-frame-identification
                    mode-line-buffer-identification "   " mode-line-position
                    (project-mode-line project-mode-line-format) (vc-mode vc-mode)
                    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
  (emacs-redisplay-defface 'mode-line '(:inverse-video t))
  (emacs-redisplay-defface 'mode-line-inactive '(:inherit mode-line))
  (emacs-redisplay-defface 'mode-line-buffer-id '(:weight bold)))

(defun emacs-redisplay--minor-modes ()
  "Return enabled minor-mode constructs in their display order."
  (let (out)
    (dolist (entry minor-mode-alist)
      (when (and (boundp (car entry)) (symbol-value (car entry)))
        (push (cdr entry) out)))
    (nreverse out)))

(defun emacs-redisplay--ml-percent (buffer)
  "Return the scroll position of the formatted window, as in GNU `%p'."
  (let* ((text (emacs-redisplay--buffer-string buffer))
         (minimum (if (emacs-redisplay--standard-buffer-p buffer)
                      (with-current-buffer buffer (point-min)) 1))
         (maximum (+ minimum (length text)))
         (start (if emacs-redisplay--mode-line-window
                    (or (emacs-window-start emacs-redisplay--mode-line-window) minimum)
                  minimum))
         (end (or emacs-redisplay--mode-line-end maximum)))
    (cond ((and (<= start minimum) (>= end maximum)) "All")
          ((<= start minimum) "Top")
          ((>= end maximum) "Bot")
          (t (format "%d%%" (min 99 (/ (+ (* 100 (- start minimum))
                                          (max 0 (1- (- maximum minimum))))
                                       (max 1 (- maximum minimum)))))))))

(defun emacs-redisplay--coding-mnemonic (coding)
  "Return CODING's mode-line mnemonic, using the coding registry when present."
  (let ((mnemonic (and (fboundp 'coding-system-get)
                       (ignore-errors (coding-system-get coding 'mnemonic)))))
    (if (integerp mnemonic) (string mnemonic)
      (cond ((memq coding '(undecided undecided-unix)) "-")
            ((memq coding '(no-conversion raw-text raw-text-unix)) "=")
            ((string-match "utf" (format "%s" coding)) "U")
            (t "-")))))

(defun emacs-redisplay--coding-eol (coding)
  "Return the GNU terminal end-of-line marker for CODING."
  (let ((type (and (fboundp 'coding-system-eol-type)
                   (ignore-errors (coding-system-eol-type coding)))))
    (cond ((eq type 1) "\\") ((eq type 2) "/") (t ":"))))

(defun emacs-redisplay--ml-escape (char buffer width)
  "Expand one mode-line percent CHAR for BUFFER at WIDTH."
  (let ((read-only (emacs-redisplay--ml-local 'buffer-read-only buffer nil)))
    (cond
     ((eq char ?b) (emacs-redisplay--buffer-name buffer))
     ((eq char ?f) (or (emacs-redisplay--ml-local 'buffer-file-name buffer nil) ""))
     ((eq char ?*) (if read-only "%" (emacs-redisplay--mode-line-modified-indicator buffer)))
     ((eq char ?+) (if (equal (emacs-redisplay--mode-line-modified-indicator buffer) "*")
                      "*" (if read-only "%" "-")))
     ((eq char ?&) (emacs-redisplay--mode-line-modified-indicator buffer))
     ((eq char ?%) "%")
     ((eq char ?-) (make-string (max 0 width) ?-))
     ((eq char ?l) (number-to-string (emacs-redisplay--ml-line buffer)))
     ((eq char ?c) (number-to-string (emacs-redisplay--ml-column buffer)))
     ((eq char ?C) (number-to-string (1+ (emacs-redisplay--ml-column buffer))))
     ((memq char '(?p ?P)) (emacs-redisplay--ml-percent buffer))
     ((eq char ?n) (if (emacs-redisplay--ml-narrowed-p buffer) " Narrow" ""))
     ((eq char ?m) (emacs-redisplay--ml-local 'mode-name buffer "Fundamental"))
     ((memq char '(?i ?I)) (number-to-string (length (emacs-redisplay--buffer-string buffer))))
     ((eq char ?@) (if (and (fboundp 'file-remote-p)
                            (file-remote-p (emacs-redisplay--ml-local 'default-directory buffer "")))
                       "@" "-"))
     ((eq char ?F)
      (let* ((frame (and emacs-redisplay--mode-line-window
                         (emacs-window-window-frame emacs-redisplay--mode-line-window)))
             (name (and (fboundp 'emacs-frame-p) (emacs-frame-p frame)
                        (emacs-frame-name frame))))
        (or name "F1")))
     ((memq char '(?z ?Z))
      (let* ((coding (or (emacs-redisplay--ml-local 'buffer-file-coding-system buffer nil)
                          (emacs-redisplay--ml-local 'default-buffer-file-coding-system buffer 'utf-8-unix)))
             (keyboard (or (and (fboundp 'keyboard-coding-system)
                                (ignore-errors (keyboard-coding-system))) 'utf-8-unix))
             (terminal (or (and (fboundp 'terminal-coding-system)
                                (ignore-errors (terminal-coding-system))) 'utf-8-unix)))
        (concat (emacs-redisplay--coding-mnemonic keyboard)
                (emacs-redisplay--coding-mnemonic terminal)
                (emacs-redisplay--coding-mnemonic coding)
                (if (eq char ?Z) (emacs-redisplay--coding-eol coding) ""))))
     ((memq char '(?\[ ?\]))
      (make-string (if (fboundp 'emacs-command-loop-recursion-depth)
                       (emacs-command-loop-recursion-depth) 0) char))
     (t ""))))

(defun emacs-redisplay--ml-string (text buffer face width literal)
  "Return styled spans for TEXT, expanding escapes unless LITERAL."
  (let ((i 0) (n (length text)) out)
    (while (< i n)
      (let* ((own (get-text-property i 'face text))
             (effective (if own (list own face) face))
             (ch (aref text i)) (piece (string ch)))
        (when (and (not literal) (= ch ?%) (< (1+ i) n))
          (setq i (1+ i))
          (let ((minimum 0))
            (while (and (< i n) (>= (aref text i) ?0) (<= (aref text i) ?9))
              (setq minimum (+ (* minimum 10) (- (aref text i) ?0)) i (1+ i)))
            (setq piece (if (< i n) (emacs-redisplay--ml-escape (aref text i) buffer width) ""))
            (when (< (string-width piece) minimum)
              (setq piece (concat piece (make-string (- minimum (string-width piece)) ?\s))))))
        (push (cons piece effective) out))
      (setq i (1+ i)))
    (nreverse out)))

(defun emacs-redisplay--ml-fit-spans (spans length face)
  "Pad SPANS to positive LENGTH or truncate to its negative absolute width."
  (let ((used 0) (limit (abs length)) out)
    (dolist (span spans)
      (let ((s (car span)) (i 0) (piece ""))
        (while (and (< i (length s))
                    (or (>= length 0) (<= (+ used (emacs-redisplay--char-width (aref s i))) limit)))
          (setq piece (concat piece (string (aref s i)))
                used (+ used (emacs-redisplay--char-width (aref s i))) i (1+ i)))
        (push (cons piece (cdr span)) out)))
    (when (and (> length 0) (< used limit))
      (push (cons (make-string (- limit used) ?\s) face) out))
    (nreverse out)))

(defun emacs-redisplay--ml-spans (format buffer face width depth &optional literal)
  "Interpret FORMAT into (TEXT . FACE) spans, bounding recursion by DEPTH."
  (cond
   ((or (null format) (> depth 50)) nil)
   ((stringp format) (emacs-redisplay--ml-string format buffer face width literal))
   ((symbolp format)
    (let ((value (emacs-redisplay--ml-local format buffer nil)))
      (unless (eq value format)
        (emacs-redisplay--ml-spans value buffer face width (1+ depth) (stringp value)))))
   ((consp format)
    (cond
     ((eq (car format) :eval)
      (let ((value (if (emacs-redisplay--standard-buffer-p buffer)
                       (with-current-buffer buffer (ignore-errors (eval (cadr format) t)))
                     (let ((nelisp-ec--current-buffer buffer))
                       (ignore-errors (eval (cadr format) t))))))
        (emacs-redisplay--ml-spans value buffer face width (1+ depth))))
     ((eq (car format) :propertize)
      (let ((own (plist-get (cddr format) 'face)))
        (emacs-redisplay--ml-spans (cadr format) buffer
                                  (if own (list own face) face) width (1+ depth))))
     ((integerp (car format))
      (emacs-redisplay--ml-fit-spans
       (emacs-redisplay--ml-spans (cdr format) buffer face width (1+ depth))
       (car format) face))
     ((and (symbolp (car format)) (car format))
      (emacs-redisplay--ml-spans
       (if (emacs-redisplay--ml-local (car format) buffer nil) (cadr format) (nth 2 format))
       buffer face width (1+ depth)))
     (t
      (let (out)
        (dolist (part format)
          (setq out (append out (emacs-redisplay--ml-spans part buffer face width (1+ depth)))))
        out))))
   (t nil)))

(defun emacs-redisplay--mode-line-format-to-string (format buffer)
  "Render mode/header line FORMAT for BUFFER using shared GNU constructs."
  (mapconcat #'car (emacs-redisplay--ml-spans format buffer nil 80 0) ""))

(defun emacs-redisplay--format-line-glyphs (format buffer width face)
  "Render a fixed WIDTH mode/header line with base FACE and styled fields."
  (let ((vec (make-vector (max 0 width) nil)) (col 0))
    (dolist (span (emacs-redisplay--ml-spans format buffer face width 0))
      (let* ((s (car span))
             (f (or (emacs-redisplay--face-resolve-spec (cdr span) 0 nil)
                    (and (memq face '(mode-line mode-line-inactive))
                         '(:inverse-video t)))))
        (dotimes (i (length s))
          (let ((w (max 1 (emacs-redisplay--char-width (aref s i)))))
            (when (<= (+ col w) width)
              (aset vec col (emacs-redisplay--make-glyph
                             :char (aref s i) :width w
                             :face (if (eq (cdr span) 'header-line) 'header-line f)
                             :realized-face (emacs-redisplay-realize-face f))))
            (setq col (+ col w))))))
    (while (< col width)
      (aset vec col (emacs-redisplay--make-glyph
                     :char ?\s :width 1
                     :face (or (emacs-redisplay--face-resolve-spec face 0 nil)
                               (and (memq face '(mode-line mode-line-inactive))
                                    '(:inverse-video t)))
                     :realized-face (emacs-redisplay-realize-face
                                     (or (emacs-redisplay--face-resolve-spec face 0 nil)
                                         (and (memq face '(mode-line mode-line-inactive))
                                              '(:inverse-video t))))))
      (setq col (1+ col)))
    vec))

(defun emacs-redisplay--mode-line-glyphs (buffer width)
  "Return BUFFER's fixed-width mode line, choosing its window's active face."
  (emacs-redisplay--format-line-glyphs
   (emacs-redisplay--mode-line-format buffer) buffer width
   (if (or (null emacs-redisplay--mode-line-window)
           (eq emacs-redisplay--mode-line-window (emacs-window-selected-window)))
       'mode-line 'mode-line-inactive)))

(defun emacs-redisplay--header-line-glyphs (buffer width)
  "Return BUFFER's fixed-width header line with styled format fields."
  (emacs-redisplay--format-line-glyphs
   (emacs-redisplay--header-line-format buffer) buffer width 'header-line))

;;; Phase 3.B.2 — overlay before-string / after-string emission

(defun emacs-redisplay--ovly-priority (overlay)
  "Return OVERLAY's numeric primary priority, defaulting to zero."
  (let ((p (emacs-redisplay--ovly-prop overlay 'priority)))
    (if (consp p) (or (car p) 0) (if (integerp p) p 0))))

(defun emacs-redisplay--overlays-with-before-string-at (overlays pos)
  "Return OVERLAYS that start exactly at POS and carry a non-empty
`before-string', sorted by priority ascending so that higher-priority
strings are emitted last (= closest to the buffer character).  Each
list element is a cons (OVERLAY . STRING)."
  (let (result)
    (dolist (ov overlays)
      (let ((bounds (emacs-redisplay--ovly-bounds ov))
            (str (emacs-redisplay--ovly-prop ov 'before-string)))
        (when (and bounds str (stringp str) (> (length str) 0)
                   (= (car bounds) pos))
          (push (cons ov str) result))))
    (sort result
          (lambda (a b)
            (< (emacs-redisplay--ovly-priority (car a))
               (emacs-redisplay--ovly-priority (car b)))))))

(defun emacs-redisplay--overlays-with-after-string-ending-at (overlays pos)
  "Return OVERLAYS whose exclusive end equals POS and carry a non-empty
`after-string', sorted by priority ascending so that higher-priority
strings are emitted last (= farther from the buffer character on the
right side, but consistent with Emacs ordering).  Each element is a
cons (OVERLAY . STRING)."
  (let (result)
    (dolist (ov overlays)
      (let ((bounds (emacs-redisplay--ovly-bounds ov))
            (str (emacs-redisplay--ovly-prop ov 'after-string)))
        (when (and bounds str (stringp str) (> (length str) 0)
                   (= (cdr bounds) pos))
          (push (cons ov str) result))))
    (sort result
          (lambda (a b)
            (< (emacs-redisplay--ovly-priority (car a))
               (emacs-redisplay--ovly-priority (car b)))))))

(defun emacs-redisplay--string-face-at (string idx fallback-face)
  "Return the effective face for STRING char at IDX.
If STRING has a `face' text-property at IDX, return that; otherwise
return FALLBACK-FACE (= the overlay's `face' property)."
  (let ((own (and (> (length string) idx)
                  (get-text-property idx 'face string))))
    (or own fallback-face)))

(defun emacs-redisplay--emit-overlay-string (str overlay used col width
                                                 anchor-pos)
  "Emit STRING as glyphs into USED starting at COL, clipped to WIDTH.
Each glyph receives the overlay's face (or the string's own face
text-property when present), and its `buf-pos' is set to ANCHOR-POS so
cursor positioning + diff hashing remain stable.  Returns the new COL
after emission (= COL when WIDTH is exhausted before any char)."
  (let* ((ov-face (emacs-redisplay--ovly-prop overlay 'face))
         (n (length str))
         (i 0)
         (overflow nil))
    (while (and (< i n) (not overflow))
      (let* ((ch (aref str i))
             (cw (emacs-redisplay--char-width ch))
             (cw* (max 1 (if (eq cw -1) 1 cw))))
        (cond
         ;; Skip embedded newlines / control chars cleanly: render as
         ;; a single space so the row stays well-formed (= MVP, no
         ;; multi-row before-string).
         ((or (eq ch ?\n) (eq cw -1))
          (when (< col width)
            (let* ((face (emacs-redisplay--string-face-at str i ov-face))
                   (g (emacs-redisplay--make-glyph
                       :char ?\s
                       :face (emacs-redisplay--resolve-face face)
                       :realized-face (emacs-redisplay-realize-face face)
                       :face-id 0 :width 1
                       :buf-pos anchor-pos)))
              (aset used col g)
              (setq col (1+ col)))))
         (t
          (cond
           ((>= (+ col cw*) (1+ width))
            (setq overflow t))
           (t
            (let* ((face (emacs-redisplay--string-face-at str i ov-face))
                   (g (emacs-redisplay--make-glyph
                       :char ch
                       :face (emacs-redisplay--resolve-face face)
                       :realized-face (emacs-redisplay-realize-face face)
                       :face-id 0 :width cw*
                       :buf-pos anchor-pos)))
              (aset used col g)
              (setq col (+ col cw*))))))))
      (setq i (1+ i)))
    col))

(defun emacs-redisplay--emit-before-strings (overlays pos used col width)
  "Emit all overlay before-strings at POS and return the new COL."
  (let ((out col))
    (when overlays
      (dolist (entry (emacs-redisplay--overlays-with-before-string-at
                      overlays pos))
        (setq out (emacs-redisplay--emit-overlay-string
                   (cdr entry) (car entry) used out width pos))))
    out))

(defun emacs-redisplay--emit-after-strings (overlays pos used col width)
  "Emit all overlay after-strings ending at POS and return the new COL."
  (let ((out col))
    (when overlays
      (dolist (entry (emacs-redisplay--overlays-with-after-string-ending-at
                      overlays pos))
        (setq out (emacs-redisplay--emit-overlay-string
                   (cdr entry) (car entry) used out width pos))))
    out))

(defun emacs-redisplay--apply-overlay-face (glyph overlays pos)
  "Merge overlay face attributes (highest priority wins) into GLYPH.
An overlay `mouse-face' at POS likewise overrides the glyph's
text-property `mouse-face' by the same priority rule."
  (when overlays
    (let (best best-prio best-mface best-mface-prio)
      (dolist (ov overlays)
        (let* ((bounds (emacs-redisplay--ovly-bounds ov))
               (prio   (emacs-redisplay--ovly-priority ov))
               (face   (emacs-redisplay--ovly-prop ov 'face))
               (mface  (emacs-redisplay--ovly-prop ov 'mouse-face))
               (inside (and bounds
                            (<= (car bounds) pos)
                            (< pos (cdr bounds)))))
          (when (and inside face (or (null best) (emacs-redisplay--overlay-before-p ov best-prio)))
            (setq best face
                  best-prio ov))
          (when (and inside mface (or (null best-mface) (emacs-redisplay--overlay-before-p ov best-mface-prio)))
            (setq best-mface mface
                  best-mface-prio ov))))
      (when best-mface
        (setf (emacs-redisplay-glyph-mouse-face glyph) best-mface))
      (when best
        (let* ((existing (emacs-redisplay-glyph-face glyph))
               (resolved (emacs-redisplay--resolve-face best))
               ;; When upstream resolve returns nil (= face not yet
               ;; defined or no match for current backend) fall back to
               ;; the raw spec so the merge stays observable.
               (eff (or resolved best))
               (merged (if existing
                           (cond
                            ((listp existing) (cons eff existing))
                            (t (list eff existing)))
                         eff)))
          (setf (emacs-redisplay-glyph-face glyph) merged)
          ;; Phase 3.B.1: re-realize the merged spec into the SGR-
          ;; ready alist so the backend flush picks up the overlay
          ;; contribution without an extra realize call per row seg.
          (setf (emacs-redisplay-glyph-realized-face glyph)
                (emacs-redisplay-realize-face merged)))))))

(defun emacs-redisplay--make-text-glyph (char pos face display buffer overlays)
  "Make a rendering glyph at POS, honoring text and overlay FACE."
  (let ((g (emacs-redisplay--make-glyph
            :char char :buf-pos pos :face (emacs-redisplay--resolve-face
                                         (emacs-redisplay--region-face buffer pos face))
            :realized-face (emacs-redisplay-realize-face
                            (emacs-redisplay--region-face buffer pos face))
            :width (max 1 (emacs-redisplay--char-width char))
            :display-spec display
            :mouse-face (emacs-redisplay--text-property-at pos 'mouse-face buffer))))
    (emacs-redisplay--apply-overlay-face g overlays pos)
    g))

(defun emacs-redisplay--string-tokens (string pos face display buffer overlays col)
  "Return (REVERSED-TOKENS . COLUMN) for a display STRING anchored at POS."
  (let ((out nil) (previous nil) (i 0)
        (tab-width (max 1 emacs-redisplay-default-tab-width)))
    (while (< i (length string))
      (let* ((ch (aref string i))
             (own-face (emacs-redisplay--string-face-at string i face)))
        (cond
         ((= ch ?\n)
          (push (cons 'newline pos) out)
          (setq col 0 previous nil))
         ((and previous (emacs-redisplay--combining-mark-p ch))
          (setf (emacs-redisplay-glyph-composition previous)
                (append (emacs-redisplay-glyph-composition previous) (list ch))))
         ((= ch ?\t)
          (let ((spaces (- tab-width (% col tab-width))))
            (dotimes (_ spaces)
              (push (emacs-redisplay--make-text-glyph
                     ?\s pos own-face display buffer overlays) out))
            (setq col (+ col spaces))))
         (t
          (setq previous (emacs-redisplay--make-text-glyph
                          ch pos own-face display buffer overlays))
          (push previous out)
          (setq col (+ col (emacs-redisplay-glyph-width previous))))))
      (setq i (1+ i)))
    (cons out col)))

(defun emacs-redisplay--display-tokens (text start buffer overlays)
  "Convert TEXT to glyph/newline tokens before choosing visual row breaks.
Visibility, replacement ranges, tabs and overlay boundary strings all affect
row length.  Source positions remain absolute even when display text shrinks."
  (let ((out nil) (i 0) (col 0) (hidden-before nil) (previous nil)
        (n (length text)))
    (while (<= i n)
      (let ((pos (+ start i)))
        ;; Nonempty after-strings precede before-strings at a shared boundary.
        ;; An empty overlay's own before-string precedes its after-string.
        (let ((strings (append
                        (cl-remove-if
                         (lambda (entry)
                           (let ((bounds (emacs-redisplay--ovly-bounds (car entry))))
                             (= (car bounds) (cdr bounds))))
                         (emacs-redisplay--overlays-with-after-string-ending-at overlays pos))
                        (emacs-redisplay--overlays-with-before-string-at overlays pos)
                        (cl-remove-if-not
                         (lambda (entry)
                           (let ((bounds (emacs-redisplay--ovly-bounds (car entry))))
                             (= (car bounds) (cdr bounds))))
                         (emacs-redisplay--overlays-with-after-string-ending-at overlays pos)))))
          (dolist (entry strings)
            (let ((laid (emacs-redisplay--string-tokens
                         (cdr entry) pos (emacs-redisplay--ovly-prop (car entry) 'face)
                         nil buffer nil col)))
              (setq out (append (car laid) out) col (cdr laid)))))
        (when (< i n)
          (let* ((ch (aref text i))
                 (hidden (emacs-redisplay--invisible-at-p pos buffer overlays))
                 (face (emacs-redisplay--text-property-at pos 'face buffer))
                 (display (emacs-redisplay--text-property-at pos 'display buffer))
                 (spec (emacs-redisplay--resolve-display display nil))
                 (space (emacs-redisplay--space-display-width spec col))
                 (replacement (and (not space)
                                   (emacs-redisplay--display-replacement-string spec))))
            (cond
             (hidden
              (when (and (eq hidden 2) (not hidden-before))
                (let ((laid (emacs-redisplay--string-tokens
                             "..." pos face nil buffer overlays col)))
                  (setq out (append (car laid) out) col (cdr laid))))
              (setq hidden-before hidden))
             ((or space (stringp replacement))
              ;; One replacement per contiguous property value, including an
              ;; empty replacement and a replacement covering a newline.
              (let* ((end (1+ i))
                     (laid (if space
                               (let ((g (emacs-redisplay--make-text-glyph
                                         ?\s pos face display buffer overlays)))
                                 (setf (emacs-redisplay-glyph-width g) space)
                                 (cons (list g) (+ col space)))
                             (emacs-redisplay--string-tokens
                              replacement pos face display buffer overlays col))))
                (while (and (< end n)
                            (eq display (emacs-redisplay--text-property-at
                                         (+ start end) 'display buffer)))
                  (setq end (1+ end)))
                (setq out (append (car laid) out) col (cdr laid)
                      i (1- end) hidden-before nil previous nil)))
             ((= ch ?\n)
              (push (list 'newline (1+ pos)
                          (emacs-redisplay--region-face buffer pos face)) out)
              (setq col 0 hidden-before nil previous nil))
             ((and previous (emacs-redisplay--combining-mark-p ch))
              (setf (emacs-redisplay-glyph-composition previous)
                    (append (emacs-redisplay-glyph-composition previous) (list ch)))
              (setq hidden-before nil))
             (t
              (let ((laid (emacs-redisplay--string-tokens
                           (string ch) pos face display buffer overlays col)))
                (setq out (append (car laid) out) col (cdr laid)
                      previous (car (car laid)) hidden-before nil)))))))
      (setq i (1+ i)))
    (nreverse out)))

(defun emacs-redisplay--token-newline-p (token)
  "Non-nil for a source newline boundary TOKEN."
  (and (consp token) (eq (car token) 'newline)))

(defun emacs-redisplay--glyph-list-vector (glyphs width &optional indicator)
  "Pack GLYPHS into cells, adding INDICATOR through any wrap-edge gap."
  (let ((vec (make-vector width nil)) (col 0))
    (dolist (g glyphs)
      (let ((w (emacs-redisplay-glyph-width g)))
        (when (<= (+ col w) width)
          (aset vec col g)
          (setq col (+ col w)))))
    (when indicator
      (while (< col width)
        (aset vec col (emacs-redisplay--make-glyph
                       :char indicator :width 1 :face 'escape-glyph
                       :realized-face (emacs-redisplay-realize-face 'escape-glyph)))
        (setq col (1+ col))))
    ;; Keep `used' as occupied cells; nil wide-glyph tails remain in the vector.
    (if (= col width) vec
      (let ((out (make-vector col nil)))
        (dotimes (i col) (aset out i (aref vec i)))
        out))))

(defun emacs-redisplay--token-rows (tokens start end width)
  "Wrap TOKENS into (GLYPHS START END CONTINUATION-P) visual rows.
Terminal continuation marks occupy the final column; an unfit wide glyph
leaves continuation marks in both edge cells, as GNU terminal redisplay does."
  (let ((rows nil) (glyphs nil) (col 0) (row-start start)
        (continuation nil) (truncated nil)
        (capacity (if emacs-redisplay-truncate-lines width (max 1 (1- width)))))
    (dolist (token tokens)
      (cond
       ((emacs-redisplay--token-newline-p token)
        (let* ((next (if (integerp (cdr token)) (cdr token) (cadr token)))
               (face (and (consp (cdr token)) (nth 2 token)))
               (vec (emacs-redisplay--glyph-list-vector
                     (nreverse glyphs) width (and truncated ?$))))
          (when (and face (not truncated) (< col width))
            (when (<= (length vec) col)
              (setq vec (vconcat vec (make-vector (1+ (- col (length vec))) nil))))
            (aset vec col (emacs-redisplay--make-glyph
                           :char ?\s :width 1 :buf-pos (1- next) :face face
                           :realized-face (emacs-redisplay-realize-face face))))
          (push (list vec row-start next continuation) rows)
          (setq glyphs nil col 0 row-start next continuation nil truncated nil)))
       (truncated nil)
       (t
        (let ((advance (emacs-redisplay-glyph-width token))
              (pos (or (emacs-redisplay-glyph-buf-pos token) row-start)))
          (when (> (+ col advance) capacity)
            (if emacs-redisplay-truncate-lines
                (progn
                  ;; Remove a glyph occupying the final column, including the
                  ;; first half of a wide glyph that spans that column.
                  (when (>= col width)
                    (setq col (- col (emacs-redisplay-glyph-width (car glyphs)))
                          glyphs (cdr glyphs)))
                  (setq truncated t))
              (let ((break-tail (and emacs-redisplay-word-wrap
                                     glyphs
                                     (or (< (emacs-redisplay-glyph-width token) 2)
                                         (memq (emacs-redisplay-glyph-char (car glyphs))
                                               '(?\s ?\t)))
                                     glyphs))
                    (carry nil))
                ;; The latest whitespace ends a word-wrapped row.  Move the
                ;; following word's glyphs intact; no terminal continuation
                ;; mark is needed at an ordinary word boundary.
                (while (and break-tail
                            (not (memq (emacs-redisplay-glyph-char
                                        (car break-tail)) '(?\s ?\t))))
                  (push (car break-tail) carry)
                  (setq break-tail (cdr break-tail)))
                (if break-tail
                    (progn
                      (push (list (emacs-redisplay--glyph-list-vector
                                   (nreverse break-tail) width)
                                  row-start
                                  (if carry (emacs-redisplay-glyph-buf-pos
                                             (car carry)) pos)
                                  continuation) rows)
                      (setq row-start (if carry
                                          (emacs-redisplay-glyph-buf-pos (car carry))
                                        pos)
                            col 0)
                      (dolist (g carry)
                        (setq col (+ col (emacs-redisplay-glyph-width g))))
                      (setq glyphs (nreverse carry) continuation t))
                  (push (list (emacs-redisplay--glyph-list-vector
                               (nreverse glyphs) width ?\\)
                              row-start pos continuation) rows)
                  (setq glyphs nil col 0 row-start pos continuation t)))))
          (unless truncated
            (push token glyphs)
            (setq col (+ col advance)))))))
    (push (list (emacs-redisplay--glyph-list-vector
                 (nreverse glyphs) width (and truncated ?$))
                row-start end continuation) rows)
    (nreverse rows)))

(defun emacs-redisplay--lay-out-line (line buffer-pos buffer overlays width)
  "Lay out LINE into cells, returning (GLYPHS . NEXT-SOURCE-POS).
This single-row query shares visibility and replacement-range semantics with
the window renderer.  Window wrapping happens after display-token creation."
  (let* ((tokens (emacs-redisplay--display-tokens line buffer-pos buffer overlays))
         (glyphs nil) (col 0) (overflow nil) (next (+ buffer-pos (length line))))
    (while (and tokens (not overflow))
      (let ((g (pop tokens)))
        (unless (emacs-redisplay--token-newline-p g)
          (if (> (+ col (emacs-redisplay-glyph-width g)) width)
              (setq overflow t next (emacs-redisplay-glyph-buf-pos g))
            (push g glyphs)
            (setq col (+ col (emacs-redisplay-glyph-width g)))))))
    (when (and overflow emacs-redisplay-truncate-lines (>= col width))
      (setq glyphs (cdr glyphs)))
    (cons (emacs-redisplay--glyph-list-vector
           (nreverse glyphs) width (and overflow emacs-redisplay-truncate-lines ?$))
          next)))

(defun emacs-redisplay--fill-row (row glyph-vec width buffer-pos end-pos)
  "Place GLYPH-VEC into ROW, padding to WIDTH with empty glyphs.
Updates ROW's used / hash / start-pos / end-pos and resets pos-delta."
  (let* ((vec (emacs-redisplay-glyph-row-glyphs row))
         (n (length glyph-vec)))
    (dotimes (i width)
      (aset vec i
            (if (< i n)
                (aref glyph-vec i)
              nil)))
    (setf (emacs-redisplay-glyph-row-used row) n
          (emacs-redisplay-glyph-row-start-pos row) buffer-pos
          (emacs-redisplay-glyph-row-end-pos row) end-pos
          (emacs-redisplay-glyph-row-pos-delta row) 0
          (emacs-redisplay-glyph-row-hash row)
          (emacs-redisplay--row-hash vec))))

(defun emacs-redisplay--clear-row (row)
  "Reset ROW to empty (all spaces, used = 0)."
  (let ((vec (emacs-redisplay-glyph-row-glyphs row)))
    (dotimes (i (length vec))
      (aset vec i nil))
    (setf (emacs-redisplay-glyph-row-used row) 0
          (emacs-redisplay-glyph-row-start-pos row) nil
          (emacs-redisplay-glyph-row-end-pos row) nil
          (emacs-redisplay-glyph-row-pos-delta row) 0
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

(defvar emacs-redisplay-word-wrap nil
  "If non-nil, wrapped lines break at word boundaries (whitespace) rather
than at exact column boundaries, mirroring Emacs `word-wrap' /
`visual-line-mode'.  Only consulted when `emacs-redisplay-truncate-lines' is
nil (Doc 06 E1).")

(defun emacs-redisplay--wrap-line-segments (line width &optional word-wrap)
  "Split LINE into a list of substrings, each occupying at most WIDTH display
columns (TAB- and CJK-width aware).  Char-wrap by default; with WORD-WRAP
non-nil, break at the last whitespace that fits rather than mid-word (falling
back to char-wrap for a word longer than WIDTH).  A LINE that already fits
within WIDTH returns a one-element list (Doc 06 E1)."
  (let ((n (length line))
        (tab-width (max 1 emacs-redisplay-default-tab-width)))
    (if (or (<= width 0) (= n 0))
        (list line)
      (let ((segs nil) (seg-start 0) (i 0) (col 0))
        (while (< i n)
          (let* ((ch (aref line i))
                 (cw (emacs-redisplay--char-width ch))
                 (adv (if (eq cw -1)
                          (- (* (1+ (/ col tab-width)) tab-width) col)
                        (max 1 cw))))
            (if (<= (+ col adv) width)
                ;; Fits on the current visual row.
                (setq col (+ col adv) i (1+ i))
              ;; Overflow: cut a segment ending before char I.
              (let ((cut i))
                ;; Word-wrap: only back up to the last fitting whitespace when
                ;; the char-wrap boundary would split a word — i.e. both sides
                ;; of the boundary are non-whitespace.  If either side is
                ;; whitespace the char-wrap cut is already a clean break.
                (when (and word-wrap (> i seg-start)
                           (not (memq (aref line i) '(?\s ?\t)))
                           (not (memq (aref line (1- i)) '(?\s ?\t))))
                  (let ((j (1- i)) (space nil))
                    (while (and (>= j seg-start) (not space))
                      (when (memq (aref line j) '(?\s ?\t))
                        (setq space j))
                      (setq j (1- j)))
                    (when space (setq cut (1+ space)))))
                ;; Never emit an empty segment (word/char wider than WIDTH).
                (when (<= cut seg-start) (setq cut (1+ seg-start)))
                (push (substring line seg-start cut) segs)
                (setq seg-start cut i cut col 0)))))
        (push (substring line seg-start) segs)
        (nreverse segs)))))

(defun emacs-redisplay--split-into-visual-lines (text width)
  "Like `emacs-redisplay--split-into-lines', but when
`emacs-redisplay-truncate-lines' is nil expand each over-WIDTH logical line
into several visual rows.  Returns a list of (LINE NL-CONSUMED CONTINUATION-P)
3-element lists; CONTINUATION-P is non-nil for visual rows that continue the
previous row of the same logical line (Doc 06 E1)."
  (let ((logical (emacs-redisplay--split-into-lines text))
        (out nil))
    (dolist (entry logical)
      (let ((line (car entry)) (nl (cdr entry)))
        (if emacs-redisplay-truncate-lines
            (push (list line nl nil) out)
          (let ((segs (emacs-redisplay--wrap-line-segments
                       line width emacs-redisplay-word-wrap)))
            (if (= (length segs) 1)
                (push (list line nl nil) out)
              (let ((k 0) (m (length segs)))
                (dolist (seg segs)
                  (push (list seg (if (= k (1- m)) nl 0) (> k 0)) out)
                  (setq k (1+ k)))))))))
    (nreverse out)))

;;; B. redisplay drivers

(defun emacs-redisplay--snapshot-fingerprint (window buffer width height)
  "Cheap fingerprint of inputs that affect WINDOW's glyph-matrix output.
Two consecutive redisplays with `equal' fingerprints can reuse the
cached matrix and skip the rebuild (= Phase 3.B.5 short-circuit).
Captures: buffer identity + size + point + narrow bounds + modification
tick (including property changes for native buffers),
plus window start/point/width/height.  Does NOT cover overlay set
or face-registry mutations — callers that mutate those must invoke
`emacs-redisplay-mark-window-dirty' (or the family of force-*
helpers) so the matrix is dropped and a fresh rebuild is forced."
  (let ((buf-size 0)
        (buf-point 0)
        (buf-narrow-start nil)
        (buf-narrow-end nil)
        (buf-tick 0))
    (cond
     ((null buffer))
     ((stringp buffer)
      (setq buf-size (length buffer)))
     ((emacs-redisplay--standard-buffer-p buffer)
      (with-current-buffer buffer
        (setq buf-size (buffer-size) buf-point (point)
              buf-narrow-start (point-min) buf-narrow-end (point-max)
              ;; Property changes affect layout even when character text is
              ;; unchanged.  Keep the text cache's chars-only tick separate.
              buf-tick (buffer-modified-tick))))
     (t
      (setq buf-size  (nelisp-ec-buffer-size  buffer)
            buf-point (nelisp-ec-buffer-point buffer)
            buf-narrow-start (nelisp-ec-buffer-narrow-start buffer)
            buf-narrow-end   (nelisp-ec-buffer-narrow-end   buffer)
            buf-tick (or (emacs-buffer-buffer-chars-modified-tick buffer) 0))))
    (vector buffer buf-size buf-point buf-narrow-start buf-narrow-end
            buf-tick
            (or (emacs-window-start window) 1)
            (or (emacs-window-point window) buf-point)
            width height emacs-redisplay-truncate-lines
            emacs-redisplay-word-wrap emacs-redisplay-default-tab-width
            (emacs-redisplay--ml-local 'buffer-invisibility-spec buffer t)
            (emacs-redisplay--mode-line-format buffer)
            (emacs-redisplay--header-line-format buffer)
            (emacs-redisplay--ml-local 'display-line-numbers buffer nil)
            (emacs-redisplay--ml-local 'display-line-numbers-width buffer nil)
            (emacs-redisplay--ml-local 'selective-display buffer nil)
            (emacs-redisplay--ml-local 'selective-display-ellipses buffer t)
            (emacs-redisplay--ml-local 'mark-active buffer nil)
            (emacs-redisplay--ml-local 'transient-mark-mode buffer nil)
            (and (emacs-redisplay--standard-buffer-p buffer)
                 (with-current-buffer buffer (ignore-errors (mark t))))
            (let ((emacs-redisplay--mode-line-window window)
                  (emacs-redisplay--mode-line-end
                   (emacs-window-window-parameter window 'emacs-redisplay-window-end)))
              (list (emacs-redisplay--ml-spans (emacs-redisplay--mode-line-format buffer) buffer nil width 0)
                    (emacs-redisplay--ml-spans (emacs-redisplay--header-line-format buffer) buffer nil width 0))))))

;;;###autoload
(defun emacs-redisplay-redisplay-window (handle window)
  "Run a redisplay pass on WINDOW under HANDLE.
Returns the (possibly newly built) glyph-matrix.  Does NOT flush to
backend — call `emacs-redisplay-flush-frame' for the actual emit.

Phase 3.B.5 short-circuit: when a cached matrix already exists and
its `fingerprint' (buffer size/point/narrow + window start/point/
dim) is `equal' to the current snapshot, the rebuild is skipped and
the cached matrix is returned with its current dirty bits intact.
Callers that mutate state outside the fingerprint (overlays /
face registry) must invoke
`emacs-redisplay-mark-window-dirty' or
`emacs-redisplay-force-mode-line-update' to drop the cache."
  (emacs-redisplay--check-handle handle)
  (unless (emacs-window-p window)
    (signal 'wrong-type-argument (list 'emacs-window-p window)))
  (let* ((buffer (emacs-window-buffer window))
         (standard (emacs-redisplay--standard-buffer-p buffer))
         (emacs-redisplay-truncate-lines
          (if standard (buffer-local-value 'truncate-lines buffer)
            emacs-redisplay-truncate-lines))
         (emacs-redisplay-word-wrap
          (if standard (buffer-local-value 'word-wrap buffer)
            emacs-redisplay-word-wrap))
         (emacs-redisplay-default-tab-width
          (if standard (buffer-local-value 'tab-width buffer)
            emacs-redisplay-default-tab-width))
         (width  (emacs-window-window-width  window))
         (height (emacs-window-window-height window))
         (old-matrix (emacs-redisplay--get-matrix handle window))
         (matrix (emacs-redisplay--ensure-matrix handle window))
         (fresh-matrix-p (null old-matrix))
         (new-fp (emacs-redisplay--snapshot-fingerprint
                  window buffer width height))
         (_long-line-state
          (and buffer (not (stringp buffer))
               ;; The legacy long-line observer reads nelisp-ec slots; native
               ;; buffers own their clipping state through the buffer API.
               (not (emacs-redisplay--standard-buffer-p buffer))
               (emacs-cc-xdisp-1--update-long-line-state
                buffer (emacs-buffer-buffer-text-tick buffer)
                (lambda () (emacs-redisplay--buffer-string buffer)))))
         (short-circuit-p
          (and (not fresh-matrix-p)
               (eq matrix old-matrix)
               (let ((old-fp (emacs-redisplay-glyph-matrix-fingerprint
                              matrix)))
                 (and old-fp (equal old-fp new-fp))))))
    (cond
     (short-circuit-p
      (emacs-redisplay--log "redisplay-window handle=%S w=%S short-circuit"
                            (emacs-redisplay-handle-id handle)
                            (emacs-redisplay--cache-key window))
      matrix)
     (t
      (emacs-redisplay--redisplay-window-rebuild
       handle window matrix old-matrix fresh-matrix-p
       buffer width height new-fp)))))

(defun emacs-redisplay--overlays-in-row-range (overlays row-pos row-end)
  "Filter OVERLAYS to those whose range intersects [ROW-POS, ROW-END)."
  (when overlays
    (cl-loop for ov in overlays
             for bounds = (emacs-redisplay--ovly-bounds ov)
             when (and bounds
                       (< (car bounds) row-end)
                       (> (cdr bounds) row-pos))
             collect ov)))

(defun emacs-redisplay--overlay-row-fingerprint (overlays row-pos row-end)
  "Return a stable fingerprint of overlays affecting [ROW-POS, ROW-END).
Returns nil when no overlay intersects the range."
  (let ((subset (emacs-redisplay--overlays-in-row-range
                 overlays row-pos row-end)))
    (when subset
      (mapcar (lambda (ov)
                (list ov
                      (emacs-redisplay--ovly-bounds ov)
                      (emacs-redisplay--ovly-prop ov 'face)
                      (emacs-redisplay--ovly-prop ov 'before-string)
                      (emacs-redisplay--ovly-prop ov 'after-string)
                      (emacs-redisplay--ovly-prop ov 'invisible)
                      (emacs-redisplay--ovly-prop ov 'priority)))
              subset))))

(defun emacs-redisplay--textprop-row-fingerprint (buffer row-pos row-end)
  "Return a stable fingerprint of rendering-relevant text-props in
[ROW-POS, ROW-END).  Captures only `face' / `display' / `invisible'
intervals (= the props that affect glyph layout).  Uses ROW-POS-relative
offsets so position-shift edits don't invalidate unrelated rows.
Returns nil when no relevant interval intersects the range."
  (when (and buffer (not (stringp buffer))
             (fboundp 'emacs-buffer-text-property-view))
    (let (out)
      (dolist (span (emacs-buffer-text-property-view
                     row-pos row-end '(face display invisible) buffer))
        (let ((s (nth 0 span))
              (e (nth 1 span))
              (p (nth 2 span)))
          (push (list (- s row-pos)
                      (- e row-pos)
                      (plist-get p 'face)
                      (plist-get p 'display)
                      (plist-get p 'invisible))
                out)))
      out)))

(defun emacs-redisplay--shift-row-positions (row delta new-start new-end)
  "Reuse ROW's glyph contents but update its position bookkeeping.
Sets `start-pos' / `end-pos' to NEW-START / NEW-END.  DELTA is added
to ROW's `pos-delta' slot — glyphs' `:buf-pos' values stay numerically
stale but are read via the `effective-buf-pos' helper that re-applies
the row's accumulated delta lazily.  This is O(1) per row instead of
O(used cells)."
  (setf (emacs-redisplay-glyph-row-start-pos row) new-start
        (emacs-redisplay-glyph-row-end-pos   row) new-end)
  (unless (zerop delta)
    (cl-incf (emacs-redisplay-glyph-row-pos-delta row) delta)))

(defun emacs-redisplay--effective-buf-pos (row glyph)
  "Return GLYPH's effective buffer position within ROW.
Adds ROW's `pos-delta' to the glyph's stored `:buf-pos'.  Returns nil
when the glyph carries no buffer position (e.g., overlay-string fill
or padding glyphs)."
  (let ((bp (emacs-redisplay-glyph-buf-pos glyph)))
    (and bp (+ bp (emacs-redisplay-glyph-row-pos-delta row)))))

(defun emacs-redisplay--row-input-key (vec start)
  "Return a rendering and relative-position key for VEC at START."
  (mapcar (lambda (g)
            (and g (list (emacs-redisplay-glyph-char g)
                         (emacs-redisplay-glyph-width g)
                         (emacs-redisplay-glyph-realized-face g)
                         (emacs-redisplay-glyph-composition g)
                         (emacs-redisplay-glyph-display-spec g)
                         (emacs-redisplay-glyph-mouse-face g)
                         (and (emacs-redisplay-glyph-buf-pos g)
                              (- (emacs-redisplay-glyph-buf-pos g) start)))))
          (append vec nil)))

(defun emacs-redisplay-display-line-numbers-mode (&optional arg)
  "Toggle a buffer's line-number gutter, or enable it according to ARG."
  (interactive "P")
  (let ((enabled (if (null arg)
                     (not (emacs-redisplay--ml-local 'display-line-numbers (current-buffer) nil))
                   (> (prefix-numeric-value arg) 0))))
    (dolist (entry (list (cons 'display-line-numbers-mode enabled)
                        (cons 'display-line-numbers
                              (and enabled (if (boundp 'display-line-numbers-type)
                                               display-line-numbers-type t)))))
      (if (and (fboundp 'nelisp--repr)
               (fboundp 'emacs-buffer-set-buffer-local-toplevel-value))
          (emacs-buffer-set-buffer-local-toplevel-value (car entry) (cdr entry))
        (set (make-local-variable (car entry)) (cdr entry))))
    (when (fboundp 'force-mode-line-update) (force-mode-line-update))
    nil))

(defun emacs-redisplay--line-number-at (buffer pos)
  "Return the logical line containing absolute POS in BUFFER."
  (let* ((text (emacs-redisplay--buffer-string buffer))
         (minimum (if (emacs-redisplay--standard-buffer-p buffer)
                      (with-current-buffer buffer (point-min)) 1))
         (limit (min (length text) (max 0 (- pos minimum))))
         (i 0) (line 1))
    (while (< i limit)
      (when (= (aref text i) ?\n) (setq line (1+ line)))
      (setq i (1+ i)))
    line))

(defun emacs-redisplay--numbered-rows (entries buffer width margin point)
  "Prefix ENTRIES with a MARGIN-wide logical or relative number gutter."
  (let ((current (emacs-redisplay--line-number-at buffer point))
        (relative (memq (emacs-redisplay--ml-local 'display-line-numbers buffer nil)
                        '(relative visual))) out)
    (dolist (entry entries)
      (let* ((line (emacs-redisplay--line-number-at buffer (nth 1 entry)))
             (active (= line current))
             (label (if (or (nth 3 entry)
                            (and (emacs-redisplay--standard-buffer-p buffer)
                                 (= (nth 1 entry) (with-current-buffer buffer (point-max)))
                                 (/= point (nth 1 entry)))) ""
                      (number-to-string (if (and relative (not active))
                                            (abs (- line current)) line))))
             (text (concat (make-string (max 1 (- margin 1 (length label))) ?\s) label " "))
             (face (if active 'line-number-current-line 'line-number))
             (vec (make-vector width nil)) (body (car entry)))
        (dotimes (i margin)
          (aset vec i (emacs-redisplay--make-glyph
                       :char (if (< i (length text)) (aref text i) ?\s)
                       :width 1 :face face
                       :realized-face (emacs-redisplay-realize-face face))))
        (dotimes (i (min (length body) (- width margin)))
          (aset vec (+ i margin) (aref body i)))
        (push (cons vec (cdr entry)) out)))
    (nreverse out)))

(defun emacs-redisplay--region-face (buffer pos face)
  "Merge the active transient region into FACE at POS in BUFFER."
  (if (and (emacs-redisplay--ml-local 'transient-mark-mode buffer nil)
           (emacs-redisplay--ml-local 'mark-active buffer nil)
           (or (null emacs-redisplay--mode-line-window)
               (eq emacs-redisplay--mode-line-window (emacs-window-selected-window)))
           (emacs-redisplay--standard-buffer-p buffer))
      (with-current-buffer buffer
        (let ((mark (ignore-errors (mark t))) (pt (point)))
          (if (and mark (<= (min mark pt) pos) (< pos (max mark pt)))
              (list 'region face) face)))
    face))

(defun emacs-redisplay--overlay-before-p (a b)
  "Whether overlay A precedes B by primary, secondary, then nesting priority."
  (let* ((pa (emacs-redisplay--ovly-prop a 'priority))
         (pb (emacs-redisplay--ovly-prop b 'priority))
         (aa (if (consp pa) (or (car pa) 0) (if (integerp pa) pa 0)))
         (ab (if (consp pb) (or (car pb) 0) (if (integerp pb) pb 0)))
         (sa (if (consp pa) (or (cdr pa) 0) 0))
         (sb (if (consp pb) (or (cdr pb) 0) 0))
         (ba (emacs-redisplay--ovly-bounds a))
         (bb (emacs-redisplay--ovly-bounds b)))
    (or (> aa ab)
        (and (= aa ab)
             (or (> sa sb)
                 (and (= sa sb) ba bb
                      (< (- (cdr ba) (car ba)) (- (cdr bb) (car bb)))))))))

(defun emacs-redisplay--selective-tokens (text start buffer overlays)
  "Tokenize TEXT after GNU selective-display folding, preserving positions."
  (let ((selective (emacs-redisplay--ml-local 'selective-display buffer nil)))
    (if (not (or (eq selective t) (and (integerp selective) (> selective 0))))
        (emacs-redisplay--display-tokens text start buffer overlays)
      (let ((i 0) (n (length text)) (run 0) out)
        (while (< i n)
          (let ((hidden
                 (if (eq selective t) (= (aref text i) ?\r)
                   (and (or (= i 0) (= (aref text (1- i)) ?\n))
                        (let ((j i) (indent 0))
                          (while (and (< j n) (memq (aref text j) '(?\s ?\t)))
                            (setq indent (if (= (aref text j) ?\t)
                                             (* (1+ (/ indent emacs-redisplay-default-tab-width))
                                                emacs-redisplay-default-tab-width)
                                           (1+ indent))
                                  j (1+ j)))
                          (>= indent selective))))))
            (if (not hidden) (setq i (1+ i))
              ;; Indentation folding hides the preceding newline as well.
              (let ((beg (if (and (not (eq selective t)) (> i 0)) (1- i) i)))
                (setq out (append out (emacs-redisplay--display-tokens
                                       (substring text run beg) (+ start run) buffer overlays)))
                (if (eq selective t)
                    (while (and (< i n) (/= (aref text i) ?\n)) (setq i (1+ i)))
                  (let ((again t))
                    (while (and again (< i n))
                      (while (and (< i n) (/= (aref text i) ?\n)) (setq i (1+ i)))
                      (setq i (min n (1+ i)))
                      (let ((j i) (indent 0))
                        (while (and (< j n) (memq (aref text j) '(?\s ?\t)))
                          (setq indent (if (= (aref text j) ?\t)
                                           (* (1+ (/ indent emacs-redisplay-default-tab-width))
                                              emacs-redisplay-default-tab-width)
                                         (1+ indent)) j (1+ j)))
                        (setq again (and (< i n) (>= indent selective)))))))
                (when (emacs-redisplay--ml-local 'selective-display-ellipses buffer t)
                  (setq out (append out (nreverse (car (emacs-redisplay--string-tokens
                                                        "..." (+ start beg) nil nil buffer overlays 0))))))
                (when (and (< i n) (not (eq selective t)))
                  (setq out (append out (list (cons 'newline (+ start i))))))
                (setq run i)))))
        (append out (emacs-redisplay--display-tokens (substring text run) (+ start run) buffer overlays))))))

(defun emacs-redisplay--plain-scroll-start (text start buffer overlays width height point)
  "Return a recentered START for short, unadorned source lines, or nil.
Scan line boundaries before allocating glyphs.  The general token path still
owns wrapping, overlays, properties and folding; this shortcut applies only
when every source line is guaranteed to fit even with double-width glyphs."
  (when (and (null overlays)
             (null (emacs-redisplay--ml-local 'selective-display buffer nil))
             (not emacs-redisplay-word-wrap)
             (if (fboundp 'nelisp--repr)
                 (null (emacs-buffer-text-property-view start (+ start (length text)) nil buffer))
               (if (emacs-redisplay--standard-buffer-p buffer)
                   (with-current-buffer buffer
                     (and (null (text-properties-at start))
                          (>= (next-property-change start nil (point-max)) (point-max))))
                 (null (emacs-buffer-text-property-view start (+ start (length text)) nil buffer)))))
    (let ((i 0) (last 0) (starts (list start)) (valid t) (point-row 0) (row 0))
      (while (and valid (< i (length text)))
        (let ((ch (aref text i)))
          (cond ((memq ch '(?\t ?\r)) (setq valid nil))
                ((= ch ?\n)
                 (when (> (* 2 (- i last)) (1- width)) (setq valid nil))
                 (setq last (1+ i) row (1+ row))
                 (push (+ start last) starts)
                 (when (<= (+ start last) point) (setq point-row row)))))
        (setq i (1+ i)))
      (when (and valid (<= (* 2 (- (length text) last)) (1- width))
                 (>= point-row height))
        (nth (max 0 (- point-row (/ height 2))) (nreverse starts))))))

(defun emacs-redisplay--right-divider-p (window)
  "Whether WINDOW has another leaf to its right in the same frame tree."
  (let ((right (nth 2 (emacs-window-window-edges window))) (found nil))
    (dolist (w (emacs-window-window-list))
      (when (> (nth 2 (emacs-window-window-edges w)) right) (setq found t)))
    found))

(defun emacs-redisplay--redisplay-window-rebuild
    (handle window matrix _old-matrix fresh-matrix-p buffer width height new-fp)
  "Rebuild WINDOW from displayed tokens, preserving unchanged row glyphs.
Raw source lines cannot determine visual breaks: hidden newlines join lines,
replacement ranges render once, and overlay strings contribute to row width."
  (let* ((emacs-redisplay--mode-line-window window)
         (emacs-redisplay--mode-line-end nil)
         (rows (emacs-redisplay-glyph-matrix-rows matrix))
         (old-hashes (mapcar #'emacs-redisplay-glyph-row-hash (append rows nil)))
         (start (or (emacs-window-start window) 1))
         (text (emacs-redisplay--cached-buffer-string handle buffer))
         (minimum (if (emacs-redisplay--standard-buffer-p buffer)
                      (with-current-buffer buffer (point-min))
                    (if (and buffer (not (stringp buffer)))
                        (or (nelisp-ec-buffer-narrow-start buffer) 1) 1)))
         (visible (substring text (min (max 0 (- start minimum)) (length text))))
         (end (+ start (length visible)))
         (overlays (and buffer (not (stringp buffer))
                        (emacs-redisplay--overlays-in start end buffer)))
         (divider (emacs-redisplay--right-divider-p window))
         (body-width (max 1 (- width (if divider 1 0))))
         (minibuffer-p (emacs-window-window-parameter window 'minibuffer))
         (mode-p (and (not minibuffer-p) emacs-redisplay-paint-mode-line-p (> height 1)
                      (emacs-redisplay--mode-line-format buffer)))
         (header-p (and (not minibuffer-p) (> height (if mode-p 2 1))
                        (emacs-redisplay--header-line-enabled-p buffer)))
         (header-rows (if header-p 1 0))
         (content-height (- height header-rows (if mode-p 1 0)))
         (point (or (emacs-window-point window) start))
         (numbers (emacs-redisplay--ml-local 'display-line-numbers buffer nil))
         (margin (if numbers
                     (+ 2 (max 2 (or (emacs-redisplay--ml-local 'display-line-numbers-width buffer nil) 0)
                               (length (number-to-string
                                        (emacs-redisplay--line-number-at buffer end))))) 0))
         (plain-start (emacs-redisplay--plain-scroll-start
                       visible start buffer overlays (max 1 (- body-width margin)) content-height point))
         (_plain-scroll (when plain-start
                          (setq visible (substring visible (- plain-start start)) start plain-start)
                          (emacs-window-set-window-start window start)
                          (setq new-fp (emacs-redisplay--snapshot-fingerprint window buffer width height))))
         (entries (emacs-redisplay--token-rows
                   (emacs-redisplay--selective-tokens visible start buffer overlays)
                   start end (max 1 (- body-width margin))))
         (point-row 0) (index 0)
         (cache (emacs-redisplay-glyph-matrix-line-cache matrix))
         (dirty (emacs-redisplay-glyph-matrix-dirty-set matrix)))
    ;; A point beyond the visible body recenters, using display rows rather
    ;; than a byte-count estimate.  Explicit starts that already show point
    ;; remain untouched.
    (dolist (entry entries)
      (when (<= (nth 1 entry) point) (setq point-row index))
      (setq index (1+ index)))
    (when (>= point-row content-height)
      (setq entries (nthcdr (max 0 (- point-row (/ content-height 2))) entries)
            start (nth 1 (car entries)))
      (emacs-window-set-window-start window start)
      (setq new-fp (emacs-redisplay--snapshot-fingerprint window buffer width height)))
    (setq emacs-redisplay--mode-line-end
          (if (> (length entries) content-height)
              (nth 1 (nth content-height entries)) end))
    (emacs-window-set-window-parameter window 'emacs-redisplay-window-end emacs-redisplay--mode-line-end)
    (setq new-fp (emacs-redisplay--snapshot-fingerprint window buffer width height))
    (when numbers
      (setq entries (emacs-redisplay--numbered-rows
                     (cl-subseq entries 0 (min (length entries) content-height)) buffer body-width margin point)))
    (dotimes (r content-height)
      (let* ((entry (pop entries)) (vec (or (car entry) []))
             (row (aref rows (+ header-rows r)))
             (row-start (nth 1 entry)) (row-end (nth 2 entry))
             (chars (mapconcat (lambda (g) (if g (string (emacs-redisplay-glyph-char g)) ""))
                               (append vec nil) ""))
             (dir (emacs-redisplay--base-direction chars))
             (key (and entry (emacs-redisplay--row-input-key vec row-start))))
        (if (and (not fresh-matrix-p) (equal key (aref cache (+ header-rows r))))
            (when row-start
              (emacs-redisplay--shift-row-positions
               row (- row-start (or (emacs-redisplay-glyph-row-start-pos row) row-start))
               row-start row-end))
          ;; Fresh rows already contain nil cells.  A populated row is filled
          ;; completely below; only a formerly populated, now empty row needs
          ;; a separate clear pass.
          (unless (or entry fresh-matrix-p)
            (emacs-redisplay--clear-row row))
          (when entry
            (setq vec (emacs-redisplay--bidi-reorder-glyphs
                       vec (if (eq dir 'right-to-left) 1 0)))
            (when (eq dir 'right-to-left)
              (setq vec (emacs-redisplay--right-align-glyphs vec body-width)))
            (emacs-redisplay--fill-row row vec width row-start row-end))
          (aset cache (+ header-rows r) key))
        (setf (emacs-redisplay-glyph-row-continuation-p row) (nth 3 entry)
              (emacs-redisplay-glyph-row-direction row) dir)))
    (when mode-p
      (let* ((r (1- height)) (row (aref rows r))
             (vec (emacs-redisplay--mode-line-glyphs buffer body-width))
             (key (cons :mode (emacs-redisplay--row-input-key vec 0))))
        (unless (and (not fresh-matrix-p) (equal key (aref cache r)))
          (emacs-redisplay--fill-row row vec width nil nil)
          (aset cache r key))))
    (when header-p
      (let* ((row (aref rows 0))
             (vec (emacs-redisplay--header-line-glyphs buffer body-width))
             (key (cons :header (emacs-redisplay--row-input-key vec 0))))
        (unless (and (not fresh-matrix-p) (equal key (aref cache 0)))
          (emacs-redisplay--fill-row row vec width nil nil)
          (aset cache 0 key))))
    (dotimes (r height)
      (let ((row (aref rows r)))
        (when divider
          (aset (emacs-redisplay-glyph-row-glyphs row) (1- width)
                (emacs-redisplay--make-glyph
                 :char ?| :width 1 :face 'vertical-border
                 :realized-face (emacs-redisplay-realize-face 'vertical-border)))
          (setf (emacs-redisplay-glyph-row-used row) width
                (emacs-redisplay-glyph-row-hash row)
                (emacs-redisplay--row-hash (emacs-redisplay-glyph-row-glyphs row))))
        (aset dirty r (or fresh-matrix-p
                         (/= (nth r old-hashes) (emacs-redisplay-glyph-row-hash row))))))
    (setf (emacs-redisplay-glyph-matrix-cursor matrix)
          (emacs-redisplay--cursor-for-point matrix point)
          (emacs-redisplay-glyph-matrix-fingerprint matrix) new-fp)
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
                   (dir (emacs-redisplay-glyph-row-direction row)))
              (if (eq dir 'right-to-left)
                  ;; RTL: POINT's glyph sits at its visual column; end-of-line
                  ;; sits just left of the visual-leftmost (logical-last) char.
                  (let ((col nil) (leftmost nil) (n (length vec)) (i 0))
                    (while (< i n)
                      (let* ((g (aref vec i))
                             (bp (and g (emacs-redisplay--effective-buf-pos
                                         row g))))
                        (when bp
                          (when (null leftmost) (setq leftmost i))
                          (when (and (null col) (= bp point)) (setq col i))))
                      (setq i (1+ i)))
                    (setq found
                          (cons r (or col
                                      (if leftmost (max 0 (1- leftmost)) 0)))))
                ;; LTR: first glyph at or past POINT (logical order).
                (let ((col 0))
                  (catch 'col-done
                    (dotimes (i used)
                      (let* ((g (aref vec i))
                             (bp (and g (emacs-redisplay--effective-buf-pos
                                         row g))))
                        (when (and bp (>= bp point))
                          (setq col i)
                          (throw 'col-done nil))
                        (setq col (1+ i)))))
                  (setq found (cons r col)))))
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

;;;###autoload
(defun emacs-redisplay-redraw-display (handle &optional frame)
  "Force a full-display redraw under HANDLE.
FRAME is accepted for API compatibility and passed through to
`emacs-redisplay-redisplay'.  Returns the count of windows redisplayed."
  (emacs-redisplay--check-handle handle)
  (emacs-redisplay-mark-frame-dirty handle frame)
  (emacs-redisplay-redisplay handle frame))

;;;###autoload
(defun emacs-redisplay-force-mode-line-update (handle &optional all window)
  "Invalidate mode-line display state under HANDLE.
When ALL is non-nil, invalidate every cached window matrix.  Otherwise
invalidate WINDOW, defaulting to the selected window.  The Phase 3 MVP
stores the mode-line as the final row of the window matrix, so cache
invalidation is enough to force the next redisplay+flush to repaint it."
  (emacs-redisplay--check-handle handle)
  (cond
   (all
    (emacs-redisplay-mark-frame-dirty handle))
   (t
    (emacs-redisplay-mark-window-dirty
     handle (or window (emacs-window-selected-window))))))

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

(defun emacs-redisplay--glyph-output-string (g)
  "Painting text for glyph G: its char plus any composition (combining) chars.
A nil glyph (an empty / continuation cell) renders as a single space.  The
combining chars carry no extra column — the terminal composes them over the
base char — so the column accounting in `emacs-redisplay--row-text-segments'
stays correct."
  (if (null g)
      " "
    (let ((comp (emacs-redisplay-glyph-composition g)))
      (if comp
          (concat (string (emacs-redisplay-glyph-char g))
                  (apply #'string comp))
        (string (emacs-redisplay-glyph-char g))))))

(defun emacs-redisplay--row-text-segments (row width)
  "Return a list of (COL TEXT FACE) painting segments for ROW.
Adjacent glyphs sharing the same realized face are batched into a
single segment so the backend `canvas-draw-text' call count stays low.
The FACE element of each tuple is the *realized* SGR-ready alist (=
Phase 3.B.1 face-realize MVP output) — not the raw spec — so the
  backend can emit the correct SGR escape directly without a per-segment
  realize call.  When `realized-face' is nil we fall back to the raw
  `face' slot for back-compat with overlays carrying spec the realizer
  does not know how to translate."
  (let* ((vec (emacs-redisplay-glyph-row-glyphs row))
         (used (max 0 (emacs-redisplay-glyph-row-used row)))
         (n (min width (length vec) used))
         (segments nil)
         (col 0))
    (cl-flet ((paint-face (g)
                (and g (or (emacs-redisplay-glyph-realized-face g)
                           (emacs-redisplay-glyph-face g)))))
      (while (< col n)
        (let* ((g (aref vec col))
               (face (paint-face g))
               (start col)
               (text (emacs-redisplay--glyph-output-string g)))
          (setq col (1+ col))
          (while (and (< col n)
                      (equal face (paint-face (aref vec col))))
            (setq text (concat text
                               (emacs-redisplay--glyph-output-string
                                (aref vec col))))
            (setq col (1+ col)))
          (push (list start text face) segments))))
    (nreverse segments)))

(defvar emacs-redisplay--flush-hash-cache (make-hash-table :test 'eq)
  "Per-glyph-matrix vector of row hashes last emitted by flush-frame.")

(defun emacs-redisplay--flush-hash-vector (matrix height)
  "Return MATRIX's flush-hash vector, resizing it to HEIGHT as needed."
  (let ((vec (gethash matrix emacs-redisplay--flush-hash-cache)))
    (unless (and (vectorp vec) (= (length vec) height))
      (setq vec (make-vector height nil))
      (puthash matrix vec emacs-redisplay--flush-hash-cache))
    vec))

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
                     (dirty (emacs-redisplay-glyph-matrix-dirty-set m))
                     (flush-hashes
                      (emacs-redisplay--flush-hash-vector m h)))
                (dotimes (r h)
                  (when (aref dirty r)
                    (let ((row (aref rows r)))
                      (unless (equal (aref flush-hashes r)
                                     (emacs-redisplay-glyph-row-hash row))
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
                        (aset flush-hashes r
                              (emacs-redisplay-glyph-row-hash row)))
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

;;; --- diff-redraw cache (Phase 3.B.5) ---------------------------------------

;;;###autoload
(defun emacs-redisplay-flush-hash-clear (&optional matrix)
  "Drop the row-hash cache used by `emacs-redisplay-flush-frame'.

When MATRIX is non-nil, clear only that glyph-matrix entry.  Otherwise
clear the whole flush-hash cache.  Returns nil."
  (if matrix
      (remhash matrix emacs-redisplay--flush-hash-cache)
    (clrhash emacs-redisplay--flush-hash-cache))
  nil)

(defun emacs-redisplay-core-initial-paint (handle frame)
  "Compatibility wrapper for the lightweight core initial-paint API.
The full redisplay engine satisfies the same contract by rendering the
selected window once and emitting one observable row write."
  (let* ((window (emacs-window-selected-window))
         (matrix (and window (emacs-redisplay-redisplay-window handle window)))
         (edges (and window (emacs-window-window-edges window)))
         (height (and window (emacs-window-window-height window)))
         (rows (and matrix (emacs-redisplay-glyph-matrix-rows matrix)))
         (row-idx (and height rows (max 0 (1- (min height (length rows))))))
         (row (and row-idx rows (aref rows row-idx)))
         (text (and row (emacs-redisplay-glyph-row-text row))))
    (when (and text
               edges
               (fboundp 'emacs-tui-backend--emit)
               (fboundp 'emacs-tui-backend--cup))
      (emacs-tui-backend--emit
       (concat (emacs-tui-backend--cup
                (+ (nth 1 edges) row-idx)
                (nth 0 edges))
               text))))
  (emacs-redisplay-flush-frame handle frame)
  t)

(provide 'emacs-redisplay)
(when (fboundp 'nelisp--repr) (require 'emacs-redisplay-builtins))

;;; emacs-redisplay.el ends here
