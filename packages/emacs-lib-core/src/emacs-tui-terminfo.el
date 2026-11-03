;;; emacs-tui-terminfo.el --- Phase 2 TUI terminfo (TERM env + capability detect)  -*- lexical-binding: t; -*-

;; Phase 2 module per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2 §3.2),
;; mirroring NeLisp Doc 43 v2 §3.1 Phase 11.A TUI MVP reference impl.
;; Layer: nelisp-emacs (Layer 2/3 extension on top of NeLisp).
;; Namespace: `emacs-tui-terminfo-' so loading inside a host Emacs does
;; NOT shadow any `tui-' / `terminfo-' / `terminal-' symbol.
;;
;; Foundation contracts (LOCKED):
;;   - Doc 43 v2 §2.5 capability matrix — *MVP minimum* = `text' /
;;     `basic-color' / `keyboard' / `resize' / `layout-box' /
;;     `layout-grid'.  This module is the *detector* that decides which
;;     additional capabilities (`256-color' / `truecolor' / `mouse' /
;;     `cursor') the running terminal actually supports based on TERM
;;     and COLORTERM env, then returns a normalised plist that
;;     `emacs-tui-backend.el' (T148) consumes via its `init' API.
;;   - Doc 43 v2 §2.5a degrade contract — capabilities not declared by
;;     this detector cause `display-spec-unsupported' downstream when
;;     the application calls a backend API requiring them.
;;
;; Role in the architecture:
;;   - Sibling of `emacs-tui-backend.el' (T148 SHIPPED) and
;;     `emacs-tui-event.el' (T152 SHIPPED).  Detection runs *before*
;;     `emacs-tui-backend-init' so the backend can be parameterised
;;     with a capability list that matches the actual terminal.
;;   - Pure Elisp + env queries; *no* POSIX terminfo file lookup
;;     (= deferred per non-goals).  The well-known TERM table covers
;;     the >95% common case (xterm-256color / screen-256color /
;;     tmux-256color / linux / dumb / etc.) without needing
;;     `tic` / `tput` shell-out.
;;   - Cache is in-memory (`emacs-tui-terminfo--cache') — clearing it
;;     forces a re-detect on the next `emacs-tui-terminfo-detect' call,
;;     which is useful when the host process changes TERM at runtime
;;     (e.g. `tmux attach' inside a fresh shell).
;;
;; Returned plist shape:
;;
;;   (:term         "xterm-256color"     ;; raw TERM string, "" if unset
;;    :colors       256                  ;; declared color count (8/16/256/16777216)
;;    :color-mode   256-color            ;; symbol for backend init
;;    :capabilities (text basic-color    ;; ordered superset incl. MVP minimum
;;                   256-color keyboard
;;                   resize mouse cursor-shape
;;                   layout-box layout-grid))
;;
;; API surface (~10 public APIs):
;;
;;   A. detection lifecycle (3 APIs)
;;      emacs-tui-terminfo-detect            — primary entry point (cached)
;;      emacs-tui-terminfo-from-env          — pure (ENV plist or process env)
;;      emacs-tui-terminfo-clear-cache       — invalidate the in-memory cache
;;
;;   B. capability query (3 APIs)
;;      emacs-tui-terminfo-supports-p        — bool capability membership test
;;      emacs-tui-terminfo-color-mode        — `(16-color | 256-color | truecolor)'
;;      emacs-tui-terminfo-capabilities      — ordered capability symbol list
;;
;;   C. introspection (2 APIs)
;;      emacs-tui-terminfo-known-terminals   — TERM strings recognised by table
;;      emacs-tui-terminfo-mvp-capabilities  — MVP minimum capability list
;;
;;   D. integration helper (1 API)
;;      emacs-tui-terminfo-backend-init-args — args to pass `emacs-tui-backend-init'
;;
;; Non-goals (per Doc 43 §3.1 + T156 scope):
;;   - POSIX terminfo file (`tic` / `infocmp` / `~/.terminfo`) parsing
;;     (= ncurses interop, separate later phase).
;;   - Persistent on-disk cache (= in-memory only, refresh on Emacs
;;     restart or explicit `clear-cache').
;;   - Live runtime probing (= DA1 / DA2 / DECRQM, requires TUI roundtrip).
;;   - Per-frame override (= every detect returns the *process* terminfo;
;;     downstream wiring picks per-frame caps).

;;; Code:

(require 'cl-lib)

;;; Errors

(define-error 'emacs-tui-terminfo-error
  "emacs-tui-terminfo error")

(define-error 'emacs-tui-terminfo-bad-input
  "Invalid input to emacs-tui-terminfo"
  'emacs-tui-terminfo-error)

;;; Contract version constants

(defconst emacs-tui-terminfo-detect-contract-version 1
  "DETECT_CONTRACT_VERSION for the plist shape returned by `detect'.
Bumped on incompatible change to the (:term/:colors/:color-mode/
:capabilities) contract.  Doc 43 v2 §2.5 substrate.")

(defconst emacs-tui-terminfo-mvp-capability-list
  '(text basic-color keyboard resize layout-box layout-grid)
  "Doc 43 §2.5 TUI MVP minimum capability set.
Same list as `emacs-tui-backend-base-capabilities' — duplicated here
to avoid a hard `require' dep on the backend for pure detection use.")

(defconst emacs-tui-terminfo-default-term "xterm"
  "Sensible default TERM when env is unset / unknown.
xterm + 16-color is the safest baseline that virtually every terminal
emulator honours — degrades gracefully on dumb terminals (which the
backend treats as text-only since they decline `basic-color' below).")

;;; Customization

(defgroup emacs-tui-terminfo nil
  "Phase 2 TUI terminfo capability detection."
  :group 'emacs)

(defcustom emacs-tui-terminfo-extra-color-terminals
  '("alacritty" "kitty" "wezterm" "ghostty" "iterm" "iterm2" "vte")
  "TERM substrings (case-insensitive) that imply truecolor support.
Used as a heuristic when COLORTERM is unset but TERM matches one of
these well-known truecolor-capable emulators.  Add a custom entry to
extend the heuristic without touching the table-driven mapping."
  :type '(repeat string)
  :group 'emacs-tui-terminfo)

(defcustom emacs-tui-terminfo-cache-enabled t
  "When non-nil, `emacs-tui-terminfo-detect' caches its result.
Set to nil for tests that need fresh env queries on every call."
  :type 'boolean
  :group 'emacs-tui-terminfo)

;;; Well-known TERM table

(defconst emacs-tui-terminfo--term-table
  ;; Each entry: (TERM-STRING COLORS EXTRA-CAPS...)
  ;; COLORS = declared baseline color count (`8' / `16' / `256').
  ;; EXTRA-CAPS = capabilities beyond the MVP minimum.
  '(;; --- xterm family ---
    ("xterm"               16  mouse cursor-shape)
    ("xterm-color"         16  mouse cursor-shape)
    ("xterm-16color"       16  mouse cursor-shape)
    ("xterm-256color"     256  mouse cursor-shape)
    ("xterm-direct"        16777216 mouse cursor-shape)
    ("xterm-kitty"        256  mouse cursor-shape)
    ;; --- screen / tmux multiplexers ---
    ("screen"              16  mouse)
    ("screen-256color"    256  mouse)
    ("tmux"                16  mouse)
    ("tmux-256color"      256  mouse)
    ("tmux-direct"         16777216 mouse)
    ;; --- linux console ---
    ("linux"                8)
    ("linux-16color"       16)
    ;; --- vt family ---
    ("vt100"                0)
    ("vt220"                0)
    ;; --- well-known modern emulators (default profile) ---
    ("alacritty"          256  mouse cursor-shape)
    ("kitty"              256  mouse cursor-shape)
    ("wezterm"            256  mouse cursor-shape)
    ("ghostty"            256  mouse cursor-shape)
    ("rxvt"                16  mouse)
    ("rxvt-unicode"        88  mouse)
    ("rxvt-unicode-256color" 256 mouse)
    ("eterm-color"         16  mouse)
    ;; --- dumb / no caps ---
    ("dumb"                 0)
    ("unknown"              0))
  "Static table of (TERM-STRING COLORS &rest EXTRA-CAPS).
COLORS may be 0 (no color), 8, 16, 88, 256, or 16777216 (= truecolor).
EXTRA-CAPS are capability symbols *added* on top of the MVP minimum.
The table is intentionally small + curated; unknown TERM falls back
to `emacs-tui-terminfo-default-term' (= `xterm', 16-color).")

;;; Cache

(defvar emacs-tui-terminfo--cache nil
  "Cached plist returned by the most recent `detect' call.
Cleared by `emacs-tui-terminfo-clear-cache' or when
`emacs-tui-terminfo-cache-enabled' is nil.")

;;;###autoload
(defun emacs-tui-terminfo-clear-cache ()
  "Invalidate the in-memory detection cache.
Next call to `emacs-tui-terminfo-detect' re-reads TERM / COLORTERM
from the process env (or from the ENV argument)."
  (setq emacs-tui-terminfo--cache nil)
  t)

;;; Internal helpers

(defun emacs-tui-terminfo--lookup-table (term)
  "Return the table entry for TERM, or nil.
Match is case-sensitive on TERM-STRING; callers normalise upstream."
  (assoc term emacs-tui-terminfo--term-table))

(defun emacs-tui-terminfo--colors-from-mode (color-mode)
  "Return the canonical integer color count for COLOR-MODE symbol.
COLOR-MODE = `16-color' / `256-color' / `truecolor'."
  (cond
   ((eq color-mode 'truecolor) 16777216)
   ((eq color-mode '256-color) 256)
   ((eq color-mode '16-color)  16)
   (t 0)))

(defun emacs-tui-terminfo--mode-from-colors (colors)
  "Return the COLOR-MODE symbol for an integer COLORS count.
Buckets: 0..15 → `16-color' (= MVP basic-color), 16..255 → `16-color'
(only 16 explicitly counts as basic), 256+ → `256-color', truecolor
(>= 16777216) → `truecolor'.

Special case: 0 colors (= `dumb' / `vt*') still returns `16-color' to
keep the *backend* default capability set sane; the caller separately
suppresses `basic-color' in that case via the EXTRA-CAPS table entry."
  (cond
   ((>= colors 16777216) 'truecolor)
   ((>= colors 256)      '256-color)
   (t                    '16-color)))

(defun emacs-tui-terminfo--colorterm-implies-truecolor-p (colorterm)
  "Return non-nil if COLORTERM string implies truecolor support.
COLORTERM = `truecolor' or `24bit' (case-insensitive) is the standard
signal documented at https://gist.github.com/XVilka/8346728."
  (and (stringp colorterm)
       (let ((lc (downcase colorterm)))
         (or (string= lc "truecolor")
             (string= lc "24bit")))))

(defun emacs-tui-terminfo--term-implies-truecolor-p (term)
  "Return non-nil if TERM matches one of the truecolor-capable emulators.
Uses `emacs-tui-terminfo-extra-color-terminals' as the substring set
(case-insensitive)."
  (and (stringp term)
       (let ((lc (downcase term)))
         (cl-some (lambda (substr)
                    (string-match-p (regexp-quote (downcase substr)) lc))
                  emacs-tui-terminfo-extra-color-terminals))))

(defun emacs-tui-terminfo--build-caps (colors extras)
  "Build the ordered capability list for COLORS + EXTRAS.
Always includes the MVP minimum (`emacs-tui-terminfo-mvp-capability-list')
in canonical order, then layers color tier (`256-color' / `truecolor'),
then extras (de-duplicated, stable order).

When COLORS = 0 the `basic-color' capability is *removed* from the
result (= dumb / monochrome terminals)."
  (let* ((mvp (copy-sequence emacs-tui-terminfo-mvp-capability-list))
         (caps (if (zerop colors)
                   (delq 'basic-color mvp)
                 mvp)))
    (when (>= colors 256)
      (unless (memq '256-color caps)
        (setq caps (append caps (list '256-color)))))
    (when (>= colors 16777216)
      (unless (memq 'truecolor caps)
        (setq caps (append caps (list 'truecolor)))))
    (dolist (cap extras)
      (unless (memq cap caps)
        (setq caps (append caps (list cap)))))
    caps))

(defun emacs-tui-terminfo--env-get (env key)
  "Return the value of KEY from ENV, falling back to `getenv' on nil ENV.
ENV is a plist or alist; KEY is a string (e.g. \"TERM\")."
  (cond
   ((null env) (getenv key))
   ;; plist form: (\"TERM\" \"xterm\" \"COLORTERM\" \"truecolor\")
   ((and (listp env)
         (or (zerop (length env))
             (stringp (car env))))
    (let ((tail env))
      (catch 'hit
        (while tail
          (when (and (stringp (car tail))
                     (string= (car tail) key))
            (throw 'hit (cadr tail)))
          (setq tail (cddr tail)))
        nil)))
   ;; alist form: ((\"TERM\" . \"xterm\") (\"COLORTERM\" . \"truecolor\"))
   ((listp env)
    (let ((cell (assoc key env)))
      (and cell (cdr cell))))
   (t (signal 'emacs-tui-terminfo-bad-input (list 'env env)))))

;;; A. detection lifecycle

;;;###autoload
(defun emacs-tui-terminfo-from-env (&optional env)
  "Pure detection from ENV (no caching, no side effects).
ENV may be:
  - nil           = use the process env (`getenv').
  - plist         = (\"TERM\" \"xterm-256color\" \"COLORTERM\" \"truecolor\")
  - alist         = ((\"TERM\" . \"xterm-256color\") ...)

Returns a plist:
  (:term TERM-STRING :colors INT :color-mode SYMBOL :capabilities LIST)

Detection algorithm (deterministic):
  1. Read TERM from ENV; empty / nil → `emacs-tui-terminfo-default-term'.
  2. Read COLORTERM from ENV; if = `truecolor' / `24bit', upgrade to
     truecolor regardless of TERM table entry.
  3. Look TERM up in the static table.  Hit = use COLORS + EXTRA-CAPS.
     Miss = fall back to default-term entry (= xterm, 16-color).
  4. If TERM matches `emacs-tui-terminfo-extra-color-terminals'
     substring (case-insensitive), upgrade to truecolor + add
     `mouse' + `cursor-shape' if not already present.
  5. Build capability list via `--build-caps'."
  (let* ((term-raw (emacs-tui-terminfo--env-get env "TERM"))
         (term (if (and (stringp term-raw)
                        (> (length term-raw) 0))
                   term-raw
                 emacs-tui-terminfo-default-term))
         (colorterm (emacs-tui-terminfo--env-get env "COLORTERM"))
         (entry (or (emacs-tui-terminfo--lookup-table term)
                    (emacs-tui-terminfo--lookup-table
                     emacs-tui-terminfo-default-term)))
         (table-colors (or (nth 1 entry) 0))
         (extras (copy-sequence (cddr entry)))
         (colors table-colors))
    ;; Step 2: COLORTERM upgrade.
    (when (emacs-tui-terminfo--colorterm-implies-truecolor-p colorterm)
      (setq colors 16777216))
    ;; Step 4: well-known truecolor TERM upgrade.
    (when (and (< colors 16777216)
               (emacs-tui-terminfo--term-implies-truecolor-p term))
      (setq colors 16777216)
      (dolist (cap '(mouse cursor-shape))
        (unless (memq cap extras)
          (setq extras (append extras (list cap))))))
    (let ((color-mode (emacs-tui-terminfo--mode-from-colors colors))
          (caps (emacs-tui-terminfo--build-caps colors extras)))
      (list :term term
            :colors colors
            :color-mode color-mode
            :capabilities caps))))

;;;###autoload
(defun emacs-tui-terminfo-detect (&optional env)
  "Detect terminfo from ENV (or process env), with caching.
Returns the same plist shape as `emacs-tui-terminfo-from-env'.

When `emacs-tui-terminfo-cache-enabled' is non-nil and a previous
result is cached, returns the cached value without re-reading env.
Use `emacs-tui-terminfo-clear-cache' to force re-detection.

Passing an explicit ENV always bypasses the cache *and* does not
populate it (= test-friendly: ERT can pass scripted env without
poisoning cache for production callers)."
  (cond
   (env (emacs-tui-terminfo-from-env env))
   ((and emacs-tui-terminfo-cache-enabled
         emacs-tui-terminfo--cache)
    emacs-tui-terminfo--cache)
   (t
    (let ((result (emacs-tui-terminfo-from-env nil)))
      (when emacs-tui-terminfo-cache-enabled
        (setq emacs-tui-terminfo--cache result))
      result))))

;;; B. capability query

;;;###autoload
(defun emacs-tui-terminfo-supports-p (capability &optional env)
  "Return non-nil iff CAPABILITY is declared by the detected terminfo.
ENV is forwarded to `detect' (= use cached process env when nil).

CAPABILITY is a symbol from the Doc 43 §2.5 capability matrix
(e.g. `truecolor', `mouse', `cursor-shape').  Always returns t / nil
— never raises for unknown CAPABILITY (Doc 43 §2.5a `pre-check guard'
contract: callers may treat any unknown cap as unsupported)."
  (let ((info (emacs-tui-terminfo-detect env)))
    (and (memq capability (plist-get info :capabilities)) t)))

;;;###autoload
(defun emacs-tui-terminfo-color-mode (&optional env)
  "Return the detected color-mode symbol: `16-color' / `256-color' / `truecolor'.
Convenience wrapper around `(plist-get (detect ENV) :color-mode)'."
  (plist-get (emacs-tui-terminfo-detect env) :color-mode))

;;;###autoload
(defun emacs-tui-terminfo-capabilities (&optional env)
  "Return the ordered list of detected capabilities.
The list always begins with the MVP minimum in canonical order
(`emacs-tui-terminfo-mvp-capability-list'), then color-tier caps,
then extras (mouse / cursor-shape / etc.) in table order."
  (copy-sequence
   (plist-get (emacs-tui-terminfo-detect env) :capabilities)))

;;; C. introspection

;;;###autoload
(defun emacs-tui-terminfo-known-terminals ()
  "Return the list of TERM strings recognised by the static table.
Useful for completion (`completing-read') and for diagnostic dumps."
  (mapcar #'car emacs-tui-terminfo--term-table))

;;;###autoload
(defun emacs-tui-terminfo-mvp-capabilities ()
  "Return a fresh copy of the MVP minimum capability list (Doc 43 §2.5)."
  (copy-sequence emacs-tui-terminfo-mvp-capability-list))

;;; D. integration helper

;;;###autoload
(defun emacs-tui-terminfo-backend-init-args (&optional env)
  "Return the args plist suitable for `emacs-tui-backend-init'.
The current backend `init' takes an optional CAPABILITIES list; this
helper returns it as a single-element list so callers can splice with
`apply':

  (apply #\\='emacs-tui-backend-init
         (emacs-tui-terminfo-backend-init-args))

Forwards ENV to `detect'."
  (list (emacs-tui-terminfo-capabilities env)))

(provide 'emacs-tui-terminfo)

;;; emacs-tui-terminfo.el ends here
