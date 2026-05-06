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
  (window-cache nil))              ;; alist (window-id . glyph-matrix)

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

(defun emacs-redisplay--invisible-at-p (pos buffer &optional overlays)
  "Return non-nil when POS should be suppressed by `invisible'.
This covers the Phase 3 MVP `invisible' text property, and also
honors overlay `invisible' when overlay accessors are available.  The
MVP treats any non-nil value as invisible; selective-display list
semantics are deferred to a later compatibility pass."
  (or (emacs-redisplay--text-property-at pos 'invisible buffer)
      (catch 'hidden
        (dolist (ov overlays)
          (let ((bounds (emacs-redisplay--ovly-bounds ov)))
            (when (and bounds
                       (<= (car bounds) pos)
                       (< pos (cdr bounds))
                       (emacs-redisplay--ovly-prop ov 'invisible))
              (throw 'hidden t))))
        nil)))

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

(defun emacs-redisplay--buffer-name (buffer)
  "Return BUFFER's display name for the MVP mode-line."
  (cond
   ((and buffer (nelisp-ec-buffer-p buffer))
    (nelisp-ec-buffer-name buffer))
   ((stringp buffer) "*string*")
   (t "")))

(defun emacs-redisplay--mode-line-format (buffer)
  "Return BUFFER's mode-line format or the Phase 3 MVP fallback."
  (cond
   ((and buffer (nelisp-ec-buffer-p buffer))
    (condition-case nil
        (emacs-buffer-buffer-local-value 'mode-line-format buffer)
      (void-variable emacs-redisplay-default-mode-line-format)))
   (t emacs-redisplay-default-mode-line-format)))

(defun emacs-redisplay--mode-line-format-to-string (format buffer)
  "Render MVP mode-line FORMAT for BUFFER."
  (cond
   ((stringp format)
    (let ((out "")
          (i 0)
          (n (length format)))
      (while (< i n)
        (let ((ch (aref format i)))
          (if (and (eq ch ?%) (< (1+ i) n))
              (let ((esc (aref format (1+ i))))
                (setq out
                      (concat out
                              (cond
                               ((eq esc ?b)
                                (emacs-redisplay--buffer-name buffer))
                               ((eq esc ?%) "%")
                               (t (string ?% esc)))))
                (setq i (+ i 2)))
            (setq out (concat out (string ch)))
            (setq i (1+ i)))))
      out))
   ((symbolp format) (symbol-name format))
   ((listp format)
    (mapconcat (lambda (part)
                 (emacs-redisplay--mode-line-format-to-string part buffer))
               format ""))
   (t (format "%s" format))))

(defun emacs-redisplay--mode-line-glyphs (buffer width)
  "Return a glyph vector for BUFFER's mode-line, clipped to WIDTH."
  (let* ((format (emacs-redisplay--mode-line-format buffer))
         (text (emacs-redisplay--mode-line-format-to-string format buffer))
         (face '(:inverse-video t))
         (realized (emacs-redisplay-realize-face face))
         (n (min width (length text)))
         (vec (make-vector n nil)))
    (dotimes (i n)
      (aset vec i
            (emacs-redisplay--make-glyph
             :char (aref text i)
             :face face
             :realized-face realized
             :width 1
             :buf-pos nil)))
    vec))

;;; Phase 3.B.2 — overlay before-string / after-string emission

(defun emacs-redisplay--ovly-priority (overlay)
  "Return the priority of OVERLAY (= integer, default 0)."
  (or (emacs-redisplay--ovly-prop overlay 'priority) 0))

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

(defun emacs-redisplay--lay-out-line (line buffer-pos buffer overlays width)
  "Return a vector of `used' glyphs for LINE starting at BUFFER-POS.
LINE is a string (single logical line, no embedded newline).  WIDTH is
the maximum number of cells to occupy.  TABs are expanded.  When the
line exceeds WIDTH and `emacs-redisplay-truncate-lines' is non-nil,
the line is clipped (= MVP behaviour, no continuation glyph).  Returns
a cons (USED-VEC . NEXT-POS) where NEXT-POS is the buffer position
just past the consumed text (excluding any newline).

Phase 3.B.2: overlays whose `:before-string' starts at the current
buffer position are emitted *before* that position's buffer char;
overlays whose `:after-string' end equals the next position are
emitted *after* the buffer char.  Multiple overlays at the same
position emit in priority order (lower priority first → higher
priority closest to the buffer text), matching Emacs convention.
The injected glyphs carry the overlay's `face' (or the string's
own `face' text-property when present), and their `buf-pos' is
the overlay anchor position."
  (let* ((tab-width (max 1 emacs-redisplay-default-tab-width))
         (pos buffer-pos)
         (col 0)
         (used (make-vector width nil))
         (i 0)
         (n (length line))
         (overflow nil))
    ;; Before-strings anchored at the line's first buffer position.
    (setq col (emacs-redisplay--emit-before-strings
               overlays pos used col width))
    (while (and (< i n) (not overflow))
      (let* ((ch (aref line i))
             (cw (emacs-redisplay--char-width ch)))
        (cond
         ;; TAB → expand to next tab stop within window width.
         ((eq cw -1)
          (unless (emacs-redisplay--invisible-at-p pos buffer overlays)
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
                    (setq col (1+ col)))))))
          (setq pos (1+ pos))
          (setq col (emacs-redisplay--emit-after-strings
                     overlays pos used col width))
          (when (< i (1- n))
            (setq col (emacs-redisplay--emit-before-strings
                       overlays pos used col width))))
         ;; Normal character (incl. CJK width 2).
         ((emacs-redisplay--invisible-at-p pos buffer overlays)
          (setq pos (1+ pos))
          (setq col (emacs-redisplay--emit-after-strings
                     overlays pos used col width))
          (when (< i (1- n))
            (setq col (emacs-redisplay--emit-before-strings
                       overlays pos used col width))))
         (t
          (let* ((face (emacs-redisplay--text-property-at pos 'face buffer))
                 (display (emacs-redisplay--text-property-at pos 'display buffer))
                 (g (emacs-redisplay--make-glyph
                     :char ch
                     :face (emacs-redisplay--resolve-face face)
                     :realized-face (emacs-redisplay-realize-face face)
                     :face-id 0
                     :width (max 1 cw)
                     :composition nil
                     :display-spec (emacs-redisplay--resolve-display
                                    display nil)
                     :buf-pos pos)))
            (when overlays
              (emacs-redisplay--apply-overlay-face g overlays pos))
            (cond
             ((>= (+ col (max 1 cw)) (1+ width))
              (setq overflow t))
             (t
              (aset used col g)
              (setq col (+ col (max 1 cw))
                    pos (1+ pos))
              (setq col (emacs-redisplay--emit-after-strings
                         overlays pos used col width))
              (when (< i (1- n))
                (setq col (emacs-redisplay--emit-before-strings
                           overlays pos used col width))))))))
        (setq i (1+ i))))
    (cons (let ((trimmed (make-vector col nil)))
            (dotimes (k col) (aset trimmed k (aref used k)))
            trimmed)
          pos)))

(defun emacs-redisplay--fill-row (row glyph-vec width buffer-pos end-pos)
  "Place GLYPH-VEC into ROW, padding to WIDTH with empty glyphs.
Updates ROW's used / hash / start-pos / end-pos and resets pos-delta."
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
          (emacs-redisplay-glyph-row-pos-delta row) 0
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

;;; B. redisplay drivers

(defun emacs-redisplay--snapshot-fingerprint (window buffer width height)
  "Cheap fingerprint of inputs that affect WINDOW's glyph-matrix output.
Two consecutive redisplays with `equal' fingerprints can reuse the
cached matrix and skip the rebuild (= Phase 3.B.5 short-circuit).
Captures: buffer identity + size + point + narrow bounds + chars-
modified-tick (Phase 3.C.1 = covers text-property face change),
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
            width height)))

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
text-properties / face registry) must invoke
`emacs-redisplay-mark-window-dirty' or
`emacs-redisplay-force-mode-line-update' to drop the cache."
  (emacs-redisplay--check-handle handle)
  (unless (emacs-window-p window)
    (signal 'wrong-type-argument (list 'emacs-window-p window)))
  (let* ((width  (emacs-window-window-width  window))
         (height (emacs-window-window-height window))
         (old-matrix (emacs-redisplay--get-matrix handle window))
         (matrix (emacs-redisplay--ensure-matrix handle window))
         (fresh-matrix-p (null old-matrix))
         (buffer (emacs-window-buffer window))
         (new-fp (emacs-redisplay--snapshot-fingerprint
                  window buffer width height))
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
             (boundp 'emacs-buffer--state))
    (let ((ext (gethash buffer emacs-buffer--state))
          (out nil))
      (when ext
        (dolist (cell (emacs-buffer--ext-text-props ext))
          (let ((s (emacs-buffer--tp-start cell))
                (e (emacs-buffer--tp-end cell))
                (p (emacs-buffer--tp-plist cell)))
            (when (and (< s row-end) (> e row-pos)
                       (or (plist-member p 'face)
                           (plist-member p 'display)
                           (plist-member p 'invisible)))
              (push (list (- (max s row-pos) row-pos)
                          (- (min e row-end) row-pos)
                          (plist-get p 'face)
                          (plist-get p 'display)
                          (plist-get p 'invisible))
                    out)))))
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

(defun emacs-redisplay--redisplay-window-rebuild
    (handle window matrix _old-matrix fresh-matrix-p buffer width height new-fp)
  "Rebuild path of `emacs-redisplay-redisplay-window' (slow path).

Phase 3.B.6 row-incremental: walks the line list and per-row decides
whether to reuse the cached glyphs (= line content + overlay
fingerprint match) or to do a full layout.  The cache lives in
`emacs-redisplay-glyph-matrix-line-cache' as a vector parallel to
rows, each entry a cons (LINE-STRING . OVERLAY-FP) or nil."
  (let* ((old-hashes
          (and (not fresh-matrix-p)
               (let* ((old-rows (emacs-redisplay-glyph-matrix-rows matrix))
                      (v (make-vector height 0)))
                 (dotimes (r height)
                   (aset v r (emacs-redisplay-glyph-row-hash
                              (aref old-rows r))))
                 v)))
         (start  (or (emacs-window-start window) 1))
         (text   (cond
                  ((null buffer) "")
                  ((stringp buffer) buffer)
                  (t (emacs-redisplay--buffer-string buffer))))
         (visible
          (cond
           ((stringp text)
            (substring text (min (max 0 (1- start)) (length text))))
           (t "")))
         (lines (emacs-redisplay--split-into-lines visible))
         (visible-end (+ start (length visible)))
         (overlays (and (not (stringp buffer))
                        (emacs-redisplay--overlays-in
                         start visible-end buffer)))
         (pos start)
         (row-idx 0)
         (rows (emacs-redisplay-glyph-matrix-rows matrix))
         (content-height (if (> height 1) (1- height) height))
         (dirty (emacs-redisplay-glyph-matrix-dirty-set matrix))
         (cache (or (emacs-redisplay-glyph-matrix-line-cache matrix)
                    (let ((v (make-vector height nil)))
                      (setf (emacs-redisplay-glyph-matrix-line-cache matrix) v)
                      v))))
    ;; Walk lines per-row.  Skip lay-out + fill when (line, ovly-fp,
    ;; tp-fp) all match the cached input.  Otherwise clear + lay-out
    ;; + fill the row and refresh the cache entry.
    (while (and (< row-idx content-height) lines)
      (let* ((entry (pop lines))
             (line  (car entry))
             (nl-consumed (cdr entry))
             (line-len (length line))
             (line-end-pos (+ pos line-len))
             (next-pos-est (+ line-end-pos nl-consumed))
             (ovly-fp (emacs-redisplay--overlay-row-fingerprint
                       overlays pos line-end-pos))
             (tp-fp (emacs-redisplay--textprop-row-fingerprint
                     buffer pos line-end-pos))
             (cached (aref cache row-idx))
             (row (aref rows row-idx)))
        (cond
         ((and (not fresh-matrix-p)
               cached
               (equal (car cached) line)
               (equal (cadr cached) ovly-fp)
               (equal (cddr cached) tp-fp))
          (let* ((old-start (or (emacs-redisplay-glyph-row-start-pos row) pos))
                 (delta (- pos old-start)))
            (emacs-redisplay--shift-row-positions
             row delta pos next-pos-est)))
         (t
          (emacs-redisplay--clear-row row)
          (let* ((laid (emacs-redisplay--lay-out-line
                        line pos buffer overlays width))
                 (gvec (car laid))
                 (next-pos (+ (cdr laid) nl-consumed)))
            (emacs-redisplay--fill-row row gvec width pos next-pos)
            (aset cache row-idx (cons line (cons ovly-fp tp-fp))))))
        (setq pos next-pos-est
              row-idx (1+ row-idx))))
    ;; Pad remaining content rows (= buffer shorter than window).
    (while (< row-idx content-height)
      (let ((row (aref rows row-idx)))
        (unless (and (zerop (emacs-redisplay-glyph-row-used row))
                     (null (aref cache row-idx)))
          (emacs-redisplay--clear-row row)
          (aset cache row-idx nil)))
      (setq row-idx (1+ row-idx)))
    ;; Mode-line row (Phase 3.B.6 cache: cons (TEXT . nil)).
    (when (> height 1)
      (let* ((row (aref rows (1- height)))
             (format (and buffer (not (stringp buffer))
                          (emacs-redisplay--mode-line-format buffer)))
             (text (and buffer (not (stringp buffer))
                        (emacs-redisplay--mode-line-format-to-string
                         format buffer)))
             (cached (aref cache (1- height))))
        (cond
         ((and (not fresh-matrix-p) cached
               (equal (car cached) text)
               (null (cdr cached)))
          ;; Skip — mode-line text unchanged.
          nil)
         (t
          (emacs-redisplay--clear-row row)
          (emacs-redisplay--fill-row
           row (emacs-redisplay--mode-line-glyphs buffer width)
           width nil nil)
          (aset cache (1- height) (cons text nil))))))
    ;; Diff dirty bits against captured old hashes.
    (dotimes (r height)
      (aset dirty r
            (or fresh-matrix-p
                (/= (aref old-hashes r)
                    (emacs-redisplay-glyph-row-hash (aref rows r))))))
    (let* ((point (or (emacs-window-point window) start))
           (cursor (emacs-redisplay--cursor-for-point matrix point)))
      (setf (emacs-redisplay-glyph-matrix-cursor matrix) cursor))
    (setf (emacs-redisplay-glyph-matrix-fingerprint matrix) new-fp)
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
                         (bp (and g (emacs-redisplay--effective-buf-pos
                                     row g))))
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
         (n (min width (length vec)))
         (segments nil)
         (col 0))
    (cl-flet ((paint-face (g)
                (and g (or (emacs-redisplay-glyph-realized-face g)
                           (emacs-redisplay-glyph-face g)))))
      (while (< col n)
        (let* ((g (aref vec col))
               (face (paint-face g))
               (start col)
               (chars (list (if g (emacs-redisplay-glyph-char g) ?\s))))
          (setq col (1+ col))
          (while (and (< col n)
                      (let ((g2 (aref vec col)))
                        (equal face (paint-face g2))))
            (push (let ((g2 (aref vec col)))
                    (if g2 (emacs-redisplay-glyph-char g2) ?\s))
                  chars)
            (setq col (1+ col)))
          (push (list start (concat (nreverse chars)) face) segments))))
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
